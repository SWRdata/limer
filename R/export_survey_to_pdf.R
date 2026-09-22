#' export_survey_to_pdf
#'
#' Renders a survey as a fillable PDF form using LaTeX/tinytex. Pulls the
#' survey's questions, answer options, welcome/end texts, and relevance
#' conditions via the API, then builds a LaTeX document with form fields
#' (radio buttons, checkboxes, text fields) matching each question's type,
#' and compiles it to PDF.
#'
#' @param survey_id integer, ID of the survey to export
#' @param output_name string, base filename (without extension) for the
#' generated .tex/.pdf files
#' @param output_dir string, directory to write the output files to
#' (created if it doesn't exist)
#' @param welcome_text string or NULL, welcome text to show at the top of
#' the PDF. If NULL, pulled from the survey's own welcome text.
#' @param end_text string or NULL, closing text to show at the end of the
#' PDF. If NULL, pulled from the survey's own end text.
#' @param included_questions character vector or NULL, question codes to
#' include (and their order). If NULL, all top-level questions are
#' included in their normal group/question order.
#' @param questions_with_comments character vector or NULL, question codes
#' that should get an extra free-text "Kommentar" field beneath them
#' @param groups_on_seperate_pages boolean, if TRUE inserts a page break
#' whenever the question group changes
#' @param character_limits named numeric vector, max character length for
#' free-text fields, keyed by question theme name (e.g.
#' `c(shortfreetext = 50)`); falls back to `"default"` for themes not
#' listed
#' @param custom_conditions named list or NULL, question code -> custom
#' German-language relevance/condition text to display instead of the
#' auto-generated one derived from the survey's relevance equation
#' @param show_group_names boolean, if TRUE prints the question group's
#' name as a heading whenever a new group starts
#'
#' @return invisible NULL; writes a .tex and compiled .pdf file to
#' `output_dir`
#' @examples
#' \dontrun{
#' export_survey_to_pdf(survey_id = 475835,
#'                                output_name = "pdf_example",
#'                                welcome_text = "Willkommen bei der Anfrage.",
#'                                end_text = "Danke fuer Ihre Teilnahme.")
#' }
#' @export
export_survey_to_pdf <- function(survey_id,
                                 output_name = "exported_survey",
                                 output_dir = "exported_survey",
                                 welcome_text = NULL,
                                 end_text = NULL,
                                 included_questions = NULL,
                                 questions_with_comments = NULL,
                                 groups_on_seperate_pages = TRUE,
                                 character_limits = c("default" = 100),
                                 custom_conditions = NULL,
                                 show_group_names = FALSE){

  invisible(lapply(c("limer", "dplyr", "stringr", "tinytex"),
                   library, character.only = TRUE))

  # Strips HTML tags, zero-width/invisible unicode characters, curly
  # quotes, and escapes underscores - used for texts inserted outside a
  # LaTeX form field context (titles, welcome/end text)
  cleanFun <- function(htmlString) {
    htmlString %>%
      gsub("<.*?>", "", .) %>%
      gsub("\u200B|\u200C|\u200D|\uFEFF", "", .) %>%
      gsub("[\u201C\u201D\u201E\u201F]", "", .) %>%
      gsub("_", "\\\\_", .)
  }

  if (!dir.exists(output_dir)) {
    message("--- Create output directory ---")
    dir.create(output_dir)
  }

  tex_file <- file.path(output_dir, paste0(output_name, ".tex"))
  pdf_file <- file.path(output_dir, paste0(output_name, ".pdf"))

  # Get survey properties ----
  message("--- Load survey content ---")
  survey_texts <- call_limer("get_language_properties",
                             params = list("iSurveyID" = survey_id))

  if (is.null(survey_texts)) {
    stop(
      sprintf(
        "Failed to retrieve properties for survey_id = %s.
        Make sure you have set a valid session_key for this survey",
        survey_id
      ),
      call. = FALSE
    )
  }

  if(is.null(welcome_text)){
    welcome_text <- cleanFun(survey_texts$surveyls_welcometext)
  }

  if(is.null(end_text)){
    end_text <- cleanFun(survey_texts$surveyls_endtext)
  }

  # Pull all questions (incl. subquestions) and build a cleaned,
  # whitespace-normalised version of the question text for reuse below
  question_list_full <- call_limer("list_questions",
                                   params = list("iSurveyID" = survey_id))
  question_list_full$question_clean <- question_list_full$question %>%
    stringr::str_remove_all("<[^>]+>") %>%
    stringr::str_remove_all("\\\\r\\\\n|\\r\\n") %>%
    stringr::str_remove_all("\u200B|\u200C|\u200D|\uFEFF") %>%
    stringr::str_squish()

  # Only keep top-level questions (parent_qid == 0); subquestions are
  # attached back onto their parent via the `answers` column below
  question_list <- question_list_full[question_list_full$parent_qid == 0, ]

  # Get answer options ----
  # For each top-level question, its "answers" are either: (a) its own
  # subquestions (e.g. array/multi-text items), if any exist, or (b) its
  # predefined answer options fetched via get_answer_options(). Falls back
  # to NULL if neither applies (e.g. free-text questions) or the API call
  # errors.
  question_list$answers <- lapply(seq_len(nrow(question_list)), function(i) {
    current_qid <- question_list$qid[i]
    sub_elements <- question_list_full$question_clean[
      question_list_full$parent_qid == current_qid
    ]
    if (length(sub_elements) > 0) return(sub_elements)
    tryCatch({
      opts <- get_answer_options(current_qid)
      if (length(opts) > 0) return(opts)
      return(NULL)
    }, error = function(e) NULL)
  })

  # Validate included_questions and questions_with_comments ----
  # Fail fast with a clear message if the caller references question
  # codes that don't actually exist in this survey
  available_questions <- question_list$title

  if (!is.null(included_questions)) {
    missing_included <- setdiff(included_questions, available_questions)
    if (length(missing_included) > 0) {
      stop(
        sprintf(
          "The following questions in 'included_questions' were not found in the survey: %s",
          paste(missing_included, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(questions_with_comments)) {
    missing_comments <- setdiff(questions_with_comments, available_questions)
    if (length(missing_comments) > 0) {
      stop(
        sprintf(
          "The following questions in 'questions_with_comments' were not found in the survey: %s",
          paste(missing_comments, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # custom_conditions only warns (not stops) since a typo'd/removed
  # question here just means one condition falls back to auto-generated
  # text, rather than breaking the whole export
  if (!is.null(custom_conditions)) {
    missing_custom <- setdiff(names(custom_conditions), available_questions)
    if (length(missing_custom) > 0) {
      warning(
        sprintf(
          "The following questions in 'custom_conditions' were not found in the survey: %s",
          paste(missing_custom, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Filter/reorder questions to just the requested subset (in the order
  # given), or keep the survey's natural group/question order otherwise
  if(!is.null(included_questions)){
    question_list <- question_list %>%
      dplyr::filter(title %in% included_questions) %>%
      dplyr::arrange(factor(title, levels = included_questions))
  } else {
    question_list <- question_list %>%
      dplyr::arrange(gid, question_order)
  }

  # Escapes LaTeX special characters for text inserted *inside* form
  # field labels/content, where cleanFun() alone isn't sufficient (needs
  # to handle backslashes and LaTeX-reserved symbols safely)
  escape_tex <- function(x) {
    x %>%
      gsub("\u200B|\u200C|\u200D|\uFEFF", "", .) %>%
      stringr::str_replace_all("\\\\", "\\\\textbackslash ") %>%
      stringr::str_replace_all("([#$%&_{}])", "\\\\\\1") %>%
      stringr::str_replace_all("\\^", "\\\\textasciicircum ") %>%
      stringr::str_replace_all("~", "\\\\textasciitilde ") %>%
      stringr::str_squish()
  }

  message("--- Build LaTeX file ---")
  # Latex header ----
  # AcroForm document setup (hyperref + pifont/needspace for form
  # fields), SWR brand color, and the survey title/welcome text
  latex_header <- paste0("
  \\documentclass[12pt]{article}
  \\usepackage[a4paper,margin=1in]{geometry}
  \\usepackage[T1]{fontenc}
  \\usepackage{helvet}
  \\renewcommand{\\familydefault}{\\sfdefault}
  \\usepackage{hyperref}
  \\usepackage{pifont}
  \\usepackage{needspace}
  \\usepackage{xcolor}

  \\definecolor{swr_purple}{HTML}{441993}
  \\setlength{\\parindent}{0pt}

  \\hypersetup{
      pdfborder={0 0 0},
      pdftitle={", cleanFun(survey_texts$surveyls_title),"}
  }

  \\begin{document}

  \\begin{center}
  {\\LARGE \\textbf{\\textcolor{swr_purple}{",
                         cleanFun(survey_texts$surveyls_title),"}}}\\\\[1em]
  \\end{center}
  {", welcome_text,"}

  \\vspace{1cm}

  \\begin{Form}
  ")

  # Build questions ----
  # One LaTeX block per top-level question: group heading (optional),
  # question text, auto- or custom-generated relevance/condition text,
  # then the appropriate form field(s) for the question's theme
  question_blocks <- lapply(seq_len(nrow(question_list)), function(i) {
    current_question <- question_list[i, ]
    block <- "\\needspace{5cm}\n"
    # NOTE: re-fetches all groups on every iteration - inefficient but
    # left as-is for now, since group count is typically small
    block_info <- call_limer("list_groups",
                             params = list("iSurveyID" = survey_id))
    if(show_group_names && (i == 1 || current_question$gid != question_list$gid[i-1])){
      block <- paste0(block, "\\large \\textcolor{swr_purple}{",
                      block_info %>%
                        dplyr::filter(gid == current_question$gid) %>%
                        dplyr::pull(group_name),
                      "}\\normalsize\\newline\\\\[0.5em]\n")
    }

    q_text <- escape_tex(current_question$question_clean)
    answers <- unlist(current_question$answers)
    block <- paste0(block, "\\large \\textbf{\\textcolor{swr_purple}{",
                    q_text,
                    "}}\\normalsize\\\\[0.5em]\n")

    condition <- current_question$relevance

    # Check for custom condition text first
    if (!is.null(custom_conditions) && current_question$title %in% names(custom_conditions)) {
      condition_text <- custom_conditions[[current_question$title]]
      block <- paste0(block, " ", condition_text, "\\\\[0.5em]\n")
    } else if (!is.null(condition) && condition != "" && condition != "1") {

      # Translate LimeSurvey's relevance equation (e.g.
      # "G01Q03.NAOK == "AO1" or G01Q04.NAOK == "AO2"") into readable
      # German text. Split into OR branches, drop internal is_empty()
      # checks, then for each remaining branch extract the referenced
      # question code + answer code and resolve them back to their
      # human-readable question number and answer label.
      raw_branches <- stringr::str_split(condition, "\\s+or\\s+|\\|\\|")[[1]]
      raw_branches <- stringr::str_trim(raw_branches)
      answer_branches <- raw_branches[!grepl("is_empty", raw_branches)]

      if (length(answer_branches) > 0) {
        # Extract one (question_code, answer_code) pair per remaining branch
        branch_texts <- lapply(answer_branches, function(branch) {
          qcode <- stringr::str_extract(branch, "G\\d+Q\\d+")
          acode <- stringr::str_extract(branch, '(?<=")[^"]+(?=")')
          if (is.na(qcode) || is.na(acode)) return(NULL)
          q_text_raw <- question_list %>%
            dplyr::filter(title == qcode) %>%
            dplyr::pull(question_clean)
          if (length(q_text_raw) == 0) return(NULL)
          q_numeral <- stringr::str_extract(q_text_raw, "[^ ]+")
          cond_answers <- question_list %>%
            dplyr::filter(title == qcode) %>%
            dplyr::pull(answers) %>% unlist()
          a_text <- cond_answers[acode]
          if (is.null(a_text) || is.na(a_text)) return(NULL)
          paste0("Frage ", q_numeral, " mit \u201e", a_text, "\u201c beantwortet wurde")
        })
        branch_texts <- Filter(Negate(is.null), branch_texts)

        if (length(branch_texts) > 0) {
          if (length(branch_texts) == 1) {
            condition_text <- paste0("Beantworten Sie diese Frage nur, wenn ",
                                     branch_texts[[1]], ".")
          } else {
            condition_text <- paste0(
              "Beantworten Sie diese Frage nur, wenn ",
              paste(branch_texts, collapse = " oder "),
              "."
            )
          }
          block <- paste0(block, " ", condition_text, "\\\\[0.5em]\n")
        }
      }
    }

    # Render the actual form field(s), based on question theme. Each
    # branch below corresponds to one or more LimeSurvey question themes
    # that need the same kind of AcroForm field (radio group, checkbox
    # group, single/multi-line text, numeric field).
    if(!is.na(current_question$question_theme_name)){
      if(current_question$question_theme_name %in% c("listradio",
                                                     "bootstrap_buttons",
                                                     "image_select-listradio",
                                                     "bootstrap_dropdown",
                                                     "list_dropdown",
                                                     "list_with_comment")){
        # Single-choice: one radio button per answer option, all sharing
        # the same field name so only one can be selected
        for (j in seq_along(answers)) {
          a_text <- escape_tex(answers[j])
          block <- paste0(
            block,
            "\\ChoiceMenu[radio, name=", "q_", current_question$title,
            ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", a_text, "\\\\\n"
          )
        }
      } else if(current_question$question_theme_name %in% c("5pointchoice")){
        # Fixed 1-5 rating scale, no predefined answer options to pull
        for (j in seq_along(1:5)) {
          block <- paste0(
            block,
            "\\ChoiceMenu[radio, name=", "q_", current_question$title,
            ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", j, "\\\\\n"
          )
        }
      } else if(current_question$question_theme_name %in% c("hugefreetext",
                                                            "shortfreetext",
                                                            "longfreetext")){
        # Free text: multiline field, max length from character_limits
        # (per-theme override, else "default")
        max_chars <- case_when(current_question$question_theme_name %in%
                                 names(character_limits) ~
                                 character_limits[current_question$question_theme_name],
                               TRUE ~ character_limits["default"])
        block <- paste0(
          block,
          "\\TextField[name=", "q_", current_question$title,
          ", width=\\linewidth, height=1.5cm, multiline=true",
          ", bordercolor={0.8 0.8 0.8}",
          ", maxlen=", max_chars, "]{}\\\\\n"
        )
      } else if(current_question$question_theme_name %in% c("multipleshorttext")){
        # One labeled single-line text field per subquestion
        for (j in seq_along(answers)) {
          a_text <- escape_tex(answers[j])
          block <- paste0(
            block,
            "\\TextField[name=", "q_", current_question$title, "_", j,
            ", bordercolor={0.7 0.7 0.7}",
            ", width=10cm",
            ", height=2em",
            ", charsize=10pt]{",
            a_text, ": }",
            "\\\\\n\\vspace{1mm}\n"
          )
        }
      } else if(current_question$question_theme_name %in% c("numerical")){
        # Numeric-only field, enforced via a JS keystroke restriction
        block <- paste0(
          block,
          "\\TextField[",
          "name=", "q_", current_question$title,
          ", width=\\linewidth, height=1.5cm",
          ", bordercolor={0.8 0.8 0.8}",
          ", keystroke={AFNumber_Keystroke(0,0,0,0,\"\",true);}",
          "]{}\\\\\n"
        )
      } else if(current_question$question_theme_name %in%
                c("image_select-multiplechoice",
                  "bootstrap_buttons_multi",
                  "multiplechoice",
                  "multiplechoice_with_comments")){
        # Multi-choice: one independent checkbox per answer option
        for (j in seq_along(answers)) {
          a_text <- escape_tex(answers[j])
          block <- paste0(
            block,
            "\\CheckBox[name=", "q_", current_question$title, "_", j,
            ",bordercolor={0.7 0.7 0.7}]{} ",
            a_text, "\\\\\n"
          )
        }
      }
      # any other theme not listed above gets no form field at all
    } else {
      # No theme info available - fall back to a generic free-text field
      block <- paste0(
        block,
        "\\TextField[name=q", i,
        ", width=\\linewidth, height=1.5cm, multiline=true,
        bordercolor={0.8 0.8 0.8}]{}\\\\\n"
      )
    }

    # Optional extra comment field beneath the question
    if(!is.null(questions_with_comments) &&
       current_question$title %in% questions_with_comments){
      block <- paste0(
        block,
        "\n\\vspace{0.6em}\n\\textbf{Kommentar:}\\\\\\newline\\vspace{0.5em}",
        "\\TextField[multiline=true, name=comment", i,
        ", width=\\linewidth, height=2cm, bordercolor={0.8 0.8 0.8}]{}\\\\[1cm]\n"
      )
    }

    # Page break when the next question belongs to a different group
    if(groups_on_seperate_pages && i != nrow(question_list) &&
       current_question$gid != question_list$gid[i+1]){
      block <- paste0(block, "\\newpage")
    }

    return(block)
  })

  # Write and compile tex file ----
  full_latex <- paste0(
    latex_header,
    paste(question_blocks, collapse = "\n"),
    "\\newline\\vspace{1cm}",
    "{", end_text,"}",
    "\\end{Form}\n",
    "\\end{document}"
  )

  cat(full_latex, file = tex_file)
  message("--- Render PDF from LaTeX file ---")
  old_wd <- getwd()
  setwd(output_dir)
  tinytex::pdflatex(basename(tex_file))
  setwd(old_wd)
  message("--- Done ---")
}

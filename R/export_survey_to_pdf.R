#' Export LimeSurvey to fillable PDF Form
#'
#' Exports a LimeSurvey survey into a fillable PDF form with customizable
#' formatting and question dplyr::filtering options.
#'
#' @param survey_id Integer. The LimeSurvey survey ID to export.
#' @param output_name Character. Name for the output files (without extension).
#'   Default is "exported_survey".
#' @param output_dir Character. Directory path where output files will be saved.
#'   If the directory exists, it will be deleted and recreated. Default is
#'   "exported_survey".
#' @param welcome_text Character or NULL. Custom welcome text for the PDF. If
#'   NULL, the survey's default welcome text will be used. Default is NULL.
#' @param end_text Character or NULL. Custom end text for the PDF. If NULL,
#'   the survey's default end text will be used. Default is NULL.
#' @param included_questions Character vector or NULL. Question codes (e.g.,
#'   "G01Q01") to include in the export. If NULL, all questions will be
#'   included. Questions in the survey should follow the schema GxxQxx.
#'   Default is NULL.
#' @param groups_on_seperate_pages Logical. If TRUE, each question group will
#'   start on a new page. Default is TRUE.
#' @param character_limits Named numeric vector. Character limits for different
#'   text field types. Names should match question theme names
#'   (e.g., "shortfreetext", "longfreetext", "hugefreetext") plus a "default"
#'   value for unspecified types. Default is c("default" = 100).
#' @param questions_with_comments Character vector or NULL. Question codes that
#'   should include a comment field. Default is NULL.
#'
#' @return Invisible NULL. The function creates a PDF file in the specified
#'   output directory.
#'
#' @details
#' The function requires an active LimeSurvey session via the \code{limer}
#' package. It supports various LimeSurvey question types including:
#' \itemize{
#'   \item Radio buttons (listradio, bootstrap_buttons, etc.)
#'   \item 5-point choice scales
#'   \item Free text fields (short, long, huge)
#'   \item Multiple short text fields
#'   \item Multiple choice checkboxes
#' }
#'
#' Questions should be numbered following the GxxQxx schema for conditional
#' text to function properly.
#'
#' @examples
#' \dontrun{
#' # Basic export with default settings
#' export_survey_to_pdf(survey_id = 475835)
#'
#' # Export with custom settings
#' export_survey_to_pdf(
#'   survey_id = 475835,
#'   output_name = "my_survey",
#'   output_dir = "output",
#'   groups_on_seperate_pages = FALSE,
#'   character_limits = c(
#'     "shortfreetext" = 100,
#'     "longfreetext" = 500,
#'     "hugefreetext" = 2000,
#'     "default" = 250
#'   )
#' )
#'
#' # Export specific questions only
#' export_survey_to_pdf(
#'   survey_id = 475835,
#'   included_questions = c("G01Q03", "G02Q09", "G03Q15"),
#'   questions_with_comments = c("G01Q03", "G02Q09")
#' )
#' }
#'
#' @importFrom limer call_limer
#' @importFrom dplyr filter arrange pull
#' @importFrom stringr str_remove_all str_squish str_extract str_replace_all
#'
#' @export

export_survey_to_pdf <- function(survey_id,
                     output_name = "exported_survey",
                     output_dir = "exported_survey",
                     welcome_text = NULL,
                     end_text = NULL,
                     included_questions = NULL,
                     questions_with_comments = NULL,
                     groups_on_seperate_pages = TRUE,
                     character_limits = c("default" = 100)){

  require(limer, dplyr, stringr)
  # Helper function to remove HTML tags and other problematic characters
  cleanFun <- function(htmlString) {
    return( gsub("_", "\\\\_",
                 gsub('["""„“]', "",
                      gsub("<.*?>", "", htmlString))))
  }

  if(dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }
  message("--- Create output directory ---")
  dir.create(output_dir)

  tex_file <- file.path(output_dir, paste0(output_name, ".tex"))
  pdf_file <- file.path(output_dir, paste0(output_name, ".pdf"))

  # Get survey properties ----
  message("--- Load survey content ---")
  survey_texts <- call_limer("get_language_properties",
                                    params = list("iSurveyID" = survey_id))

  # Catch missing session key
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

  question_list <- call_limer("list_questions",
                                     params = list("iSurveyID" = survey_id))
  question_list$question_clean <- question_list$question %>%
    stringr::str_remove_all("<[^>]+>") %>%
    stringr::str_remove_all("\\\\r\\\\n|\\r\\n") %>%
    stringr::str_squish()


  # Get answer options ----
  question_list$answers <- lapply(seq_len(nrow(question_list)), function(i) {
    current_qid <- question_list$qid[i]

    sub_elements <- question_list$question[question_list$parent_qid ==
                                             current_qid]

    if (length(sub_elements) > 0) {
      return(sub_elements)
    }

    tryCatch({
      opts <- get_answer_options(current_qid)
      if (length(opts) > 0) return(opts)

      subq <- call_limer(
        "list_subquestions",
        params = list("iSurveyID" = survey_id, "iQuestionID" = current_qid)
      )
      if (!is.null(subq) && nrow(subq) > 0) return(subq$question)

      return(NULL)
    }, error = function(e) NULL)
  })
  question_list <- question_list[question_list$parent_qid == 0, ]

  # Validate included_questions and questions_with_comments ----
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

  if(!is.null(included_questions)){
    question_list <- question_list %>%
      dplyr::filter(title %in% included_questions) %>%
      dplyr::arrange(factor(title, levels = included_questions))
  }else{
    question_list <- question_list %>%
      dplyr::arrange(gid, question_order)
  }

  escape_tex <- function(x) {
    x %>%
      stringr::str_replace_all("\\\\", "\\\\textbackslash ") %>%
      stringr::str_replace_all("([#$%&_{}])", "\\\\\\1") %>%
      stringr::str_replace_all("\\^", "\\\\textasciicircum ") %>%
      stringr::str_replace_all("~", "\\\\textasciitilde ") %>%
      stringr::str_squish()
  }

  message("--- Build LaTeX file ---")
  # Latex header ----
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
  {\\large \\today}
  \\end{center}
  {", welcome_text,"}

  \\vspace{1cm}

  \\begin{Form}
  ")

  # Build questions ----
  question_blocks <- lapply(seq_len(nrow(question_list)), function(i) {
    current_question <- question_list[i, ]
    # Ensure all elements of a question are on the same page
    # TODO May have to be adjusted via argument depending on question lengths
    block <- "\\needspace{5cm}\n"
    # Add Question Group name before first question in group
    block_info <- call_limer("list_groups",
                                    params = list("iSurveyID" = survey_id))
    if(i == 1 || current_question$gid != question_list$gid[i-1]){
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
    if(!is.null(condition) && condition != "" && condition != "1"){
      condition_question_code <- stringr::str_extract(condition, "G\\d+Q\\d+")
      condition_question_text <- question_list %>%
        dplyr::filter(title == condition_question_code) %>%
        dplyr::pull(question_clean)
      condition_question_numeral <- stringr::str_extract(condition_question_text,  "[^ ]+")
      condition_answer_code <- stringr::str_extract(condition, "(?<=\")[^\"]+(?=\")")
      conditional_answers <- question_list %>%
        dplyr::filter(title == condition_question_code) %>%
        dplyr::pull(answers) %>% unlist()
      conditional_answer_text <- conditional_answers[condition_answer_code]
      condition_text <- paste0("Beantworten Sie diese Frage nur, wenn Frage ",
                               condition_question_numeral, " mit ",
                               conditional_answer_text, " beantwortet wurde.")

      block <- paste0(block, " ", condition_text, "\\\\[0.5em]\n")
    }

    if(!is.na(current_question$question_theme_name)){
      if(current_question$question_theme_name %in% c("listradio",
                                                     "bootstrap_buttons",
                                                     "image_select-listradio",
                                                     "bootstrap_dropdown",
                                                     "list_dropdown",
                                                     "list_with_comment")){
        # Generate a separate radio button for each answer option
        for (j in seq_along(answers)) {
          a_text <- escape_tex(answers[j])
          # Syntax: \ChoiceMenu[radio, name=GroupID]{label}{ExportValue=Label}
          block <- paste0(
            block,
            "\\ChoiceMenu[radio, name=", "q_", current_question$title,
            ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", a_text, "\\\\\n"
          )
        }
      } else if(current_question$question_theme_name %in% c("5pointchoice")){
        for (j in seq_along(1:5)) {
          # Syntax: \ChoiceMenu[radio, name=GroupID]{label}{ExportValue=Label}
          block <- paste0(
            block,
            "\\ChoiceMenu[radio, name=", "q_", current_question$title,
            ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", j, "\\\\\n"
          )
        }
      } else if(current_question$question_theme_name %in% c("hugefreetext",
                                                         "shortfreetext",
                                                         "longfreetext")){
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
        for (j in seq_along(answers)) {
          a_text <- escape_tex(answers[j])

          # Create a text field block
          block <- paste0(
            block,
            "\\TextField[name=", "q_", current_question$title, "_", j,
            ", bordercolor={0.7 0.7 0.7}",
            ", width=10cm",
            ", height=2em",
            ", charsize=10pt]{",
            a_text, ": }",            # The label for the text box
            "\\\\\n\\vspace{1mm}\n"  # Spacing between fields
          )
        }
      } else if(current_question$question_theme_name %in% c("numerical")){
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
    } else{
      block <- paste0(
        block,
        "\\TextField[name=q", i,
        ", width=\\linewidth, height=1.5cm, multiline=true,
        bordercolor={0.8 0.8 0.8}]{}\\\\\n"
      )
    }

    # Ggf. Möglichkeit für Kommentare
    if(!is.null(questions_with_comments) &&
       current_question$title %in% questions_with_comments){
      block <- paste0(
        block,
        "\n\\vspace{0.6em}\n\\textbf{Kommentar:}\\\\\\newline\\vspace{0.5em}",
        "\\TextField[multiline=true, name=comment", i,
        ", width=\\linewidth, height=2cm, bordercolor={0.8 0.8 0.8}]{}\\\\[1cm]\n"
      )
    }

    # ensure pagebreak between groups
    if(groups_on_seperate_pages && i != nrow(question_list) &&
       current_question$gid != question_list$gid[i+1]){
      block <- paste0(block, "\\newpage")
    }

    return(block)
  })

  # Write and compile tex file ----
  # patch latex code together
  full_latex <- paste0(
    latex_header,
    paste(question_blocks, collapse = "\n"),
    "\\newline\\vspace{1cm}",
    "{", end_text,"}",
    "\\end{Form}\n",
    "\\end{document}"
  )

  # write .tex file
  cat(full_latex, file = tex_file)
  # compile PDF (pdflatex needs to run in the output directory)
  message("--- Render PDF from LaTeX file ---")
  old_wd <- getwd()          # save current working directory
  setwd(output_dir)          # switch to output directory

  res <- system2(
    command = Sys.which("pdflatex"),
    args = basename(tex_file),
    stdout = "pdflatex.log",
    stderr = "pdflatex_err.log"
  )

  setwd(old_wd)              # restore original working directory

  cat(paste(res, collapse = "\n"))
}








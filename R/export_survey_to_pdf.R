library(limer)
library(dplyr)
library(stringr)
# TODO Zeichenlimits für Textfelder?
# TODO einlesen von ausgefüllter PDF
# TODO arrays/dualscale typ richtig antwortformat auslesen und printen

# Setup ----
# TODO future function  arguments
# Kommentar, dass Fragen durchnummeriert sein sollen, damit bedingter Text funktioniett
# Fragen sollten den Schema GxxQxx folgen
# survey_id <- 253197
# survey_id <- 969888
survey_id <- 475835
output_name <- "survey_test"
welcome_text <- NULL
end_text <- NULL
included_questions <- NULL
groups_on_seperate_pages <- TRUE
# included_questions <- c("G01Q01", "G02Q02", "G02Q03", "G02Q04", "G03Q05",
#                         "G03Q06", "G03Q07", "G03Q08", "G04Q09", "G04Q10",
#                         "G04Q11", "G04Q12", "G05Q13", "G06Q15", "G06Q14")
questions_with_comments <- NULL
# questions_with_comments <- c("G01Q01", "G02Q02", "G02Q03", "G02Q04", "G03Q05",
#                              "G03Q06", "G03Q07", "G03Q08", "G04Q09", "G04Q10",
#                              "G04Q11", "G04Q12", "G05Q13")
# TODO remove source after testing
source("R/get_answer_options.R")
source("../limesurvey/report_katastrophenschutz/get_session_key.R")




# Helper function to remove HTML tags and other problematic characters
# TODO neue cleaning Funktion die Uli gepushed hast
cleanFun <- function(htmlString) {
  return( gsub("_", "\\\\_",
               gsub('["""„“]', "",
                    gsub("<.*?>", "", htmlString))))
}

tex_file <- paste0(output_name, ".tex")
pdf_file <- paste0(output_name, ".pdf")

# Get survey properties ----
survey_texts <- limer::call_limer("get_language_properties",
                                  params = list("iSurveyID" = survey_id))

if(is.null(welcome_text)){
  welcome_text <- cleanFun(survey_texts$surveyls_welcometext)
}

if(is.null(end_text)){
  end_text <- cleanFun(survey_texts$surveyls_endtext)
}

question_list <- limer::call_limer("list_questions",
                                   params = list("iSurveyID" = survey_id))
question_list$question_clean <- question_list$question %>%
  str_remove_all("<[^>]+>") %>%
  str_remove_all("\\\\r\\\\n|\\r\\n") %>%
  str_squish()


# Get answer options ----
question_list$answers <- lapply(seq_len(nrow(question_list)), function(i) {
  current_qid <- question_list$qid[i]

  sub_elements <- question_list$question[question_list$parent_qid == current_qid]

  if (length(sub_elements) > 0) {
    return(sub_elements)
  }

  tryCatch({
    opts <- get_answer_options(current_qid)
    if (length(opts) > 0) return(opts)

    subq <- limer::call_limer(
      "list_subquestions",
      params = list("iSurveyID" = survey_id, "iQuestionID" = current_qid)
    )
    if (!is.null(subq) && nrow(subq) > 0) return(subq$question)

    return(NULL)
  }, error = function(e) NULL)
})
question_list <- question_list[question_list$parent_qid == 0, ]

if(!is.null(included_questions)){
  question_list <- question_list %>%
    filter(title %in% included_questions) %>%
    arrange(factor(title, levels = included_questions))
}else{
  question_list <- question_list %>%
    arrange(qid)
}

escape_tex <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash ") %>%
    str_replace_all("([#$%&_{}])", "\\\\\\1") %>%
    str_replace_all("\\^", "\\\\textasciicircum ") %>%
    str_replace_all("~", "\\\\textasciitilde ") %>%
    str_squish()
}

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
  block_info <- limer::call_limer("list_groups",
                                  params = list("iSurveyID" = survey_id))
  if(i == 1 || current_question$gid != question_list$gid[i-1]){
    block <- paste0(block, "\\large \\textcolor{swr_purple}{",
                    block_info %>% filter(gid == current_question$gid) %>% pull(group_name),
                    "}\\normalsize\\newline\\\\[0.5em]\n")
  }

  q_text <- escape_tex(current_question$question_clean)
  answers <- unlist(current_question$answers)
  block <- paste0(block, "\\large \\textbf{\\textcolor{swr_purple}{",
                  q_text,
                  "}}\\normalsize\\\\[0.5em]\n")

  condition <- current_question$relevance
  if(!is.null(condition) && condition != "" && condition != "1"){
    condition_question_code <- str_extract(condition, "G\\d+Q\\d+")
    condition_question_text <- question_list %>% filter(title == condition_question_code) %>% pull(question_clean)
    condition_question_numeral <- str_extract(condition_question_text,  "[^ ]+")
    condition_answer_code <- str_extract(condition, "(?<=\")[^\"]+(?=\")")
    conditional_answers <- question_list %>% filter(title == condition_question_code) %>% pull(answers) %>% unlist()
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
          "\\ChoiceMenu[radio, name=q", i, ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", a_text, "\\\\\n"
        )
      }
    } else if(current_question$question_theme_name %in% c("5pointchoice")){
      for (j in seq_along(1:5)) {
        # Syntax: \ChoiceMenu[radio, name=GroupID]{label}{ExportValue=Label}
        block <- paste0(
          block,
          "\\ChoiceMenu[radio, name=q", i, ", bordercolor={0.7 0.7 0.7}]{}{", "= ", "} ", j, "\\\\\n"
        )
      }
    } else if(current_question$question_theme_name %in% c("hugefreetext",
                                                       "shortfreetext",
                                                       "longfreetext")){
      block <- paste0(
        block,
        "\\TextField[name=q", i, ", width=\\linewidth, height=1.5cm, multiline=true, bordercolor={0.8 0.8 0.8}]{}\\\\\n"
      )
    } else if(current_question$question_theme_name %in% c("multipleshorttext")){
      for (j in seq_along(answers)) {
        a_text <- escape_tex(answers[j])

        # Create a text field block
        block <- paste0(
          block,
          "\\TextField[name=q", i, "_", j,
          ", bordercolor={0.7 0.7 0.7}",
          ", width=10cm",
          ", height=2em",
          ", charsize=10pt]{",
          a_text, ": }",            # The label for the text box
          "\\\\\n\\vspace{1mm}\n"  # Spacing between fields
        )
      }
    } else if(current_question$question_theme_name %in% c("image_select-multiplechoice",
                                                        "bootstrap_buttons_multi",
                                                        "multiplechoice",
                                                        "multiplechoice_with_comments")){
      for (j in seq_along(answers)) {
        a_text <- escape_tex(answers[j])
        block <- paste0(
          block,
          "\\CheckBox[name=q", i, "_", j,
          ",bordercolor={0.7 0.7 0.7}]{} ",
          a_text, "\\\\\n"
        )
      }
    }
  } else{
    block <- paste0(
      block,
      "\\TextField[name=q", i, ", width=\\linewidth, height=1.5cm, multiline=true, bordercolor={0.8 0.8 0.8}]{}\\\\\n"
    )
  }

  # Ggf. Möglichkeit für Kommentare
  if(!is.null(questions_with_comments) &&
     current_question$title %in% questions_with_comments){
    block <- paste0(
      block, "\n\\vspace{0.6em}\n\\textbf{Kommentar:}\\\\\\newline\\vspace{0.5em}",
      "\\TextField[multiline=true, name=comment", i, ", width=\\linewidth, height=2cm, bordercolor={0.8 0.8 0.8}]{}\\\\[1cm]\n"
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
# remove all old files to ensure changes are adopted
file.remove(
  "survey_test.tex",
  "survey_test.aux",
  "survey_test.log",
  "survey_test.out",
  "survey_test.pdf"
)

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
# compile PDF
system(paste("pdflatex", tex_file))








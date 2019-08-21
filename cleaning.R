#' ---
#' title: Data cleaning
#' author: Daniel M Bader
#' output: html_document
#' ---
#' 

#+ load, message = F, echo = F -----------------------------------------------
library(data.table)
library(ggplot2)

file_raw_data <- file.path("../data_visualization_survey_master/data/cleaned_survey_results_2019.csv")
tab_raw <- fread(file_raw_data)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 9)


#+ basics --------------------------------------------------------------------
#'
#' # Questions
#'
writeLines(names(tab_raw))

#'
#' # Ansers per questions 
#'
vec_answers_per_question <- sapply(tab_raw, function(x) {
  length(unique(x))
})
tab_a_per_q <- as.data.table((vec_answers_per_question), keep.rownames = T)
tab_a_per_q[, idx := 1:nrow(tab_a_per_q)]

#+ fig.height = 10
ggplot(
  tab_a_per_q, 
  aes(x = reorder(paste0("Q", idx, ": ", substring(V1, 1, 80)), V2), y = V2)
  ) +
  geom_col() + coord_flip() + labs(x =  NULL, y = "N unique answers")



#+ q1 ------------------------------------------------------------------------
idx <- 1
#'
#' # Q1: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]

#+
ggplot(tmp_tab, aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)


#+ end -----------------------------------------------------------------------------------------------------------------


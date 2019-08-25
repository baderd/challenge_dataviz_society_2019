#' ---
#' title: Data cleaning
#' author: Daniel M Bader
#' output: 
#'   html_document:
#'     toc: yes
#'     toc_float: yes
#' ---
#' 

#+ load, message = F, echo = F -----------------------------------------------
library(data.table)
library(ggplot2)
# library(GGally)
# library(ggpubr)

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


#+ fig.height = 10
ggplot(
  tab_a_per_q[V2 < 100], 
  aes(x = reorder(paste0("Q", idx, ": ", substring(V1, 1, 80)), V2), y = V2)
) +
  geom_col() + coord_flip() + labs(x =  NULL, y = "N unique answers")



#'
#' # Simple questions
#'
#+ q1 ------------------------------------------------------------------------
idx <- 1
#'
#' ## Q1: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]

#+
ggplot(tmp_tab, aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)



#+ q4 ------------------------------------------------------------------------
idx <- 4
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)


#+ q5 ------------------------------------------------------------------------
idx <- 5
#'
#' ## Q5: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tmp_tab, aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)



#+ q6 ------------------------------------------------------------------------
idx <- 6
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)



#+ q7 ------------------------------------------------------------------------
idx <- 7
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)


#+ q12 ------------------------------------------------------------------------
idx <- 12
#'
#' # Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)

#+ q23 ------------------------------------------------------------------------
idx <- 23
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)



#+ q30 ------------------------------------------------------------------------
idx <- 30
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)

#+ q31 ------------------------------------------------------------------------
idx <- 31
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)


#+ q47 ------------------------------------------------------------------------
idx <- 47
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- tab_raw[, idx, with = F]
#+
ggplot(tab_raw[, idx, with = F], aes(x = get(names(tmp_tab)))) + 
  geom_bar() + coord_flip() + labs(x = NULL)


#+ complex ------------------------------------------------------------------
#'
#' # Complex questions
#'
idx <- 22
#'
#' ## Q`r idx`: `r names(tab_raw)[idx]` 
#'
tmp_tab <- data.table(
  gsub(" ", "", unlist(strsplit(unlist(tab_raw[, idx, with = F]), ",")))
)
tmp_tab <- tmp_tab[, .N, by = .(Technology = V1)][order(N, decreasing = T)]

#' Show top 20
#+
ggplot(tmp_tab[1:20], mapping = aes(x = reorder(Technology, N), y = N)) + 
  geom_col() + coord_flip() + labs(x = NULL)



#+ boring -------------------------------------------------------------------
#'
#' # Boring questions
#'

#+ end -----------------------------------------------------------------------------------------------------------------


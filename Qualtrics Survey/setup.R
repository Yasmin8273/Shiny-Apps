# Setup -----

require(tidyverse)
require(DataCombine)
require(gutenbergr)
require(tidytext)
library(stringi)


# Question label -----
# 1.descriptive block 
# 2.vertical single answer
# 3.horizontal single answer
# 4.vertical multiple answer
# 5.horizontal multiple answer
# 6.text entry, need to be edited on qualtrics
# qlabel <- "[[Question:DB]],[[Question:MC:SingleAnswer:Vertical]],[[Question:MC:SingleAnswer:Horizontal]],[[Question:MC:MultipleAnswer:Vertical]],[[Question:MC:MultipleAnswer:Horizontal]],[[Question:TextEntry]]" %>%
#   scan(text=., what="char", sep=",") %>%
#   paste0(.,"\n")
# qlabel


# Structure -----
question_structure <- function(qna_q, qna_c, qlabel){
  question <- qna_q %>%
    paste0(., "\n")
  choice <- qna_c %>%
    as.vector() %>%
    stri_remove_empty(., na_empty = TRUE)  # remove empty and NA cells
  if (length(choice) != 0){
    choice <- choice %>%
      paste0("[[Choice]]\n",., "\n") %>%
      toString() %>%
      str_remove_all(", ")
  }
  paste0(qlabel, question,"[[AdvancedChoices]]\n",choice)
}

block_structure <- function(header, question){
  paste0(
    "[[Question:DB]]\n",
    header,
    question,
    "[[PageBreak]]\n\n"
  )
}

survey_structure <- function(block){
  paste0(
    "[[AdvancedFormat]]\n",
    "[[Block:MC Block]]\n\n",
    block
  )
}

# Complete survey ----
complete_survey <- function(qna, blocks){
  # assign question label
  question <- vector()
  for (i in 1:ncol(qna)){
    label <- qna["question.label",i]
    qna_q <- qna["questions", i]
    qna_c <- qna[startsWith(rownames(qna), "choice"),i]
    if (label == 'single'){
      question[i] <- question_structure(qna_q, qna_c, "[[Question:MC:SingleAnswer:Vertical]]\n")
    }
    else if (label == 'multi'){
      question[i] <- question_structure(qna_q, qna_c, "[[Question:MC:MultipleAnswer:Vertical]]\n")
    }
    else if (label == 'text'){
      question[i] <- question_structure(qna_q, qna_c, "[[Question:TextEntry]]\n")
    }
  }
  # assign questions to block
  header <- blocks$header %>%
    paste0("<b>", ., "<b>", "\n")
  list <- blocks$list %>%
    as.list() %>%
    lapply(., function(x){scan(text=x, what='char', sep=" ") %>% as.numeric()})
  survey_block <- vector()
  for (j in seq_along(list)){
    block_header <- header[j]
    question_list <- list[[j]]
    block_question <- vector()
    for (m in seq_along(question_list)){
      block_question[m] <- question[question_list[m]]
    }
    block_question <- block_question %>%
      toString() %>%
      str_replace_all("\\\n, ", "\\\n")
    survey_block[j] <- block_structure(block_header, block_question)
  }
  survey_block <- survey_block %>%
    toString() %>%
    str_replace_all("\\\n, ", "\\\n")
  survey_structure(survey_block)
}

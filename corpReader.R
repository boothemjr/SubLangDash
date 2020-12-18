library(rvest)
library(stringr)
library(dplyr)
DATA_DIR <- read_html(paste0(getwd(), "/data/spanish_corpus/practiceText"))

DATA_nodes <- DATA_DIR %>%
  html_nodes("doc") 

DATA_text <- DATA_nodes %>%
  html_text(trim = TRUE) %>%
  str_replace_all("\n+", "") 

DATA_attr <- DATA_nodes %>%
  html_attrs() %>%
  bind_rows()

corpClean <- tibble(text = DATA_text) %>%
  bind_cols(DATA_attr, .)
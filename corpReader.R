### INSTALL LIBRARIES ####
library(rvest)
library(stringr)
library(dplyr)

### CORPUS READING + CLEANING ####
# set directory
DATA_DIR <- paste0(getwd(), "/data/spanish_corpus/practiceText")

# read in HTML
DATA_HTML <- read_html(DATA_DIR)

# read in nodes + build DATA_text
DATA_nodes <- DATA_HTML %>% 
  html_nodes("doc")
DATA_text <- DATA_nodes %>%
  html_text(trim = TRUE) %>%
  str_replace_all("\n+", "") # replacing newlines

# read in attributes + build DATA_attr
DATA_attr <- DATA_nodes %>%
  html_attrs() %>%
  bind_rows()

# combine DATA_attr + DATA_text into DATA_clean
DATA_clean <- tibble(text = DATA_text) %>%
  bind_cols(DATA_attr, .)
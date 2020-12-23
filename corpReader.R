### INSTALL LIBRARIES ####
library(rvest)
library(stringr)
library(dplyr)

### CORPUS READING + CLEANING ####
# set directory
DATA_DIR <-
  paste0(getwd(), "/exampleData/spanish_corpus/practiceText") # TODO update to read in ALL files

# read in HTML of corpus
DATA_HTML <- read_html(DATA_DIR)

# read in nodes + build DATA_text, replace
DATA_nodes <- DATA_HTML %>%
  html_nodes("doc")
DATA_text <- DATA_nodes %>%
  html_text(trim = TRUE)

# remove irrelevant text and newlines
DATA_DIR <- paste0(getwd(), "/cleaning/corpusRemoval.txt")

# read in removal list as character vector
REMOVE_LIST <- readLines(DATA_DIR)

# FIXME remove a series of words form a list (formatting, numbers, months, etc.)
# DATA_text <- DATA_text %>%
#   str_replace_all(pattern = REMOVE_LIST, replacement = "")

# remove new lines from DATA_text
DATA_text <- DATA_text %>%
  str_replace_all("\n+", "")

# read in attributes + build DATA_attr
DATA_attr <- DATA_nodes %>%
  html_attrs() %>%
  bind_rows()

# combine DATA_attr + DATA_text into DATA_clean
DATA_clean <- tibble(text = DATA_text) %>%
  bind_cols(DATA_attr, .)

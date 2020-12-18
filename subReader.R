#### Sub Reader ####
# import libraries
library(tidyverse)
library(SnowballC)
library(tidytext)
library(readtext)
library(stopwords)

# set DATA_DIR
DATA_DIR <- paste0(getwd(), "/subs/")

# read in subs
subs <-
  readtext(paste0(DATA_DIR, "*.txt"))

# TODO change episode names while retaining original titles
# change episode names
subs$doc_id = sub("[^-]*- ", "", as.character(subs$doc_id))
subs$doc_id = sub(" -.*$", "", as.character(subs$doc_id))

# df <- data.frame(1:18)
# replace(subs, 18, df)
# sub <- strsplit(, "-")
# strsplit(as.character("-"))

# tokenize text
tokenized <- unnest_tokens(subs, word, text)

# create stopwords dataframe
stopdf <- as.data.frame(stopwords("es"))

# change column name
stopdf <- rename(stopdf, "stops" = "stopwords(\"es\")")

# build df of extra stop words unique to this series
newstopdf <- data.frame("stops" = c("oh", "si", "ok", "bien"))

# add new stop words to df
stopdf <- rbind(stopdf, newstopdf)

#remove stop words
tokenized <- anti_join(tokenized, stopdf, by = c("word" = "stops"))

# TODO group by episode
# tokenized <- group_by(tokenized, doc_id)

# TODO truncate to stems - will require readding base word
# tokenized <- wordStem(tokenized[2], language = "spanish")

# plot occurrences for each word by episode


tokenized %>%
  filter(doc_id == "1x01") %>%
  count(word, sort = TRUE)  %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 5) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity",
           fill = "darkred",
           colour = "darkgreen")


temp <- filter(tokenized, doc_id == "1x01") %>%
  count(word, sort = TRUE)
  mutate(word = reorder(word, n))


#################################################

tokenized2 <- tokenized %>%
  select(1)


temp <- strsplit(as.character(tokenized2[,1]), " - ")

strsplit(as.character("-"))



# plot occurrences of each word OLD
tokenized %>%
  count(word, sort = TRUE) %>%
  filter(n < 1000) %>% # max = 1000
  filter(n > 50) %>% # min = 50
  mutate(word = reorder(word, n)) %>%
  #ggplot(aes(word, n)) +
  ggplot(aes(word, n)) +
  #ggplot(aes(word, n), color = "lightgray") +
  geom_col() +
  #geom_col(aes(fill = doc_id)) +
  xlab(NULL) +
  coord_flip()

# plots occurrences of each word

set.seed(1234)
df <- data.frame(sex = factor(rep(c("F", "M"), each = 200)),
                 weight = round(c(
                   rnorm(200, mean = 55, sd = 5), rnorm(200, mean = 65, sd = 5)
                 )))
head(df)

ggplot(df, aes(x = weight)) + geom_histogram()

tokenized %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 50) %>% # min = 50
  # ggplot(aes(word, n)) +
  geom_()
ggplot(tokenized, aes(x = word, y = count)) + geom_col()

# convert to vector
v <- as.vector(subs[2], "character")

# remove line breaks
v <- gsub("\n", " ", v)

tokenized = unnest_tokens(v, word, text)
head(tokenized)


stopwords::stopwords("es")


# tokenize words
tokens <- tokenize_words(v)
tokens <- tokens[[1]]

head(tokens)

tokens <- tokenize_words(v, stopwords = stopwords::stopwords("es"))

stopwords

sub <- tokenize_words(sub)

test <- wordStem(sub, language = "spanish")

test <- wordStem(c("chico", "chicos", "chica", "chicas"))

# sub <- tokenize_words(sub, stopwords = stopwords::stopwords("es"))

library(stopwords)

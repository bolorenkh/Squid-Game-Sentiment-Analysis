library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(tidytext)
library(wordcloud2)
library(widyr)
library(jiebaR)
library(topicmodels)
library(scales)
library(forcats)
library(janitor)
#Data source: https://www.kaggle.com/datasets/deepcontractor/squid-game-netflix-twitter-data

squid_data <- read_csv("Squid Games.csv")
squid_data <- squid_data %>%
  dplyr::select("date", "text")
squid_data <- squid_data[grep("^2021*", squid_data$date),]
#squid_data <- squid_data[-grep("SquidGame", squid_data$date),]

#cleaning the data
squid_data <- distinct(squid_data)
squid_data <- squid_data %>%
  filter(!text %in% c("FALSE", "???"))

#adding the id column
squid_data$id <- 1:nrow(squid_data)
squid_text <- function(text){
  # Remove "amp;", and "\n"" wich are not part of the tweet
  text <- gsub("amp;", "", text)
  text <- gsub("\\n", "", text)
  text <- gsub("^\\s+", "", text)
  text <- gsub("\\s+$", "", text)
  text <- gsub("[ |\t]+", " ", text)
  # Eliminate URLs
  text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", text)
  text <- gsub("https?://.+", "", text)
  # Eliminate unnecessary space first
  text <- gsub(' {2,}', ' ', text)
  # tweets.dat$text <- gsub("\\s+", " ", tweets.dat$text )
  # Remove extra characters
  text <- gsub("[^[:alpha:][:space:]]*", "", text)
  text <- gsub("\\d+\\w*\\d*", "", text)
  text <- gsub("[^\x01-\x7F]", "", text)
  text <- gsub("[[:punct:]]", " ", text)
  # Remove references to other twitter users
  #text <- gsub("@\\w+", "", text)
  # Convert to lower case
  text <- sapply(text, tolower)
}

squid_data$text <- squid_text(squid_data$text)

stopwords2 <- read.delim("stopwords2.txt")

#apply the stop word to the tokens
squid_tokens <- squid_data %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords("english")) %>%
  filter(!word %in% c(stopwords2$a)) %>%
  filter(!word %in% c("https", "t.co", "rt"))

#TF-IDF
squid_tfidf <- squid_tokens %>%
  count(id, word , sort=T) %>%
  bind_tf_idf(word, id, n)
max(squid_tfidf$tf_idf)

#Correlation of tokens
squid_cors <- squid_tokens %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, id, sort = TRUE)
word_cors <- squid_cors %>%
  #filter(item1 %in% c("wtf", "korea", "nft", "happy")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  facet_bar(y = item2, x = correlation, by = item1)
facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}),
                 
                 x = {{ x }},
                 fill = {{ by }})
  facet <- facet_wrap(vars({{ by }}),
                      nrow = nrow,
                      ncol = ncol,
                      scales = scales)
  ggplot(df, mapping = mapping) +
    geom_col(show.legend = FALSE) +
    scale_y_reordered() +
    facet +
    ylab("")
}
#frequency
squid_frq_tidy <- squid_tokens %>%
  count(word, sort = TRUE)

#sentiment analysis
library(textdata)
afinn <- get_sentiments('afinn')
squid_senti <- squid_frq_tidy %>%
  inner_join(afinn, by = "word")

#value = 4
wordcloud2(squid_senti[squid_senti$value == 4,])

#value = -5
wordcloud2(squid_senti[squid_senti$value == -5,])

#word association
squid_cors <- squid_tokens %>%
  group_by(word) %>%
  filter(n() >= 2) %>%
  pairwise_cor(word, date, sort = TRUE)
data(squid_tokens)

#Begin
library(tidyverse)

(word_asso <- squid_cors %>%
    pairwise_count(item1, item2, sort = TRUE))

#filter the words
word_asso %>%
  filter(item1 == "koreanmovie")
word_assoo <- squid_cors %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(item1, item2) %>%
  filter(!is.na(correlation))
TermDocumentMatrix(, control = list())
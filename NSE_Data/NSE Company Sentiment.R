install.packages(c("mnormt", "psych", "SnowballC", "hunspell",
                   "broom", "tokenizers", "janeaustenr"))
install.packages("tidytext")
install.packages("textdata")
install.packages("SentimentAnalysis")
install.packages("sentimentr")
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator
install.packages("RColorBrewer") # color palettes
install.packages("pdftools")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
#library(randomForest)
#library(plyr)
library(readr)
library(janitor)
library(purrr)
library(tidyr)
library(tidytext)
library(glue)
library(stringr)
library(SentimentAnalysis)
library(textdata)
library(sentimentr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("pdftools")


# 1. Import comments for analysis (11)

# Specify the path to your PDF file
pdf_file19 <- "/Users/PC/Downloads/ADEH-MSC/Module 2/DSA 8203-Principles of DSA/Kakuzi/Report 2019.pdf"
pdf_file20 <- "/Users/PC/Downloads/ADEH-MSC/Module 2/DSA 8203-Principles of DSA/Kakuzi/Report 2020.pdf"
pdf_file21 <- "/Users/PC/Downloads/ADEH-MSC/Module 2/DSA 8203-Principles of DSA/Kakuzi/Report 2021.pdf"


# Extract text from the PDF
pdf_text <- pdf_text(pdf_file19,pdf_file20,pdf_file21)

# Combine all pages into a single string (optional, depends on your needs)
combined_text <- paste(pdf_text, collapse = " ")

# Print the first 500 characters of the combined text
cat(substr(combined_text, 1, 500))
View(combined_text)
print(combined_text,"KakuziAnnual.csv")

setwd("/Users/PC/Downloads/ADEH-MSC/Module 2/DSA 8203-Principles of DSA/Kakuzi")

CBL00 <- read.csv(file = 'KakuziAnnual.csv')
View(CBL00)
CBL01 <- CBL00

CBL01$comment_count <- as.integer(1)

str(CBL01)
summary(CBL01)
class(CBL01)

#2a
View(CBL01)

#2 b - Separate out words with sentences

CBL02b <- CBL01 %>%
  select(CommentText) %>%
  mutate(sentence_id = row_number()) %>%
  unnest_tokens(word, CommentText)


# 2c

View(CBL02b)

#2d add back sentences and sentiments
CBL02d = CBL02b %>%
  #filter(work=='CBL02b') %>%
  get_sentences(text) %>%
  sentiment() %>%
  drop_na() %>%   # empty lines
  mutate(sentence_id = row_number())


#2e Exclude sentiments with '0' vlaue
CBL02e <- subset(CBL02d, CBL02d$sentiment != 0.00)

CBL02e1 <- subset(CBL02e, CBL02e$sentiment > 0.00)
CBL02e2 <- subset(CBL02e, CBL02e$sentiment < 0.00)
CBL02e3 <- subset(CBL02e, CBL02e$sentiment == 0.00)
View(CBL02e)

#2f Find frequency of words

CBL02f <- CBL02e %>%
  group_by(word) %>%
  summarise(freq = sum(word_count))
View(CBL02f)


#3 Create word cloud
set.seed(1234)
wordcloud(words = CBL02f$word, freq = CBL02f$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#4 Summarize sentiments
summary(CBL02e$sentiment)

#5 Summarize comments count
CBL05 <- CBL01 %>%
  group_by(QnNo) %>%
  summarise(commentcount = sum(comment_count))

View(CBL05)


#6 Summarize sentence count
CBL06 <- CBL02e %>%
  group_by(sentence_id) %>%
  summarise(commentcount = sum(word_count))

View(CBL06)

#7 Summary of sentiments

CBL07 <- CBL02e %>%
  group_by(word) %>%
  summarize(wordtcount = sum(word_count), sent = mean(sentiment))

View(CBL07)



write.csv(CBL02e,"CBL02e.csv")
write.csv(CBL02e,"CBL02f.csv")


sentiment=sentiment_by(CBL02e$word)
View(sentiment)

set.seed(1234)
wordcloud(words = CBL07$word, freq = CBL07$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

summary(sentiment$ave_sentiment)

CBL02e$ave_sentiment=sentiment$ave_sentiment
CBL02e$sd_sentiment=sentiment$sd
write.csv(CBL02e,"CBL02e.csv")
library(ggplot2)
pplt <- qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")

pplt

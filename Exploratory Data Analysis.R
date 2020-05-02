###### Survey Questions Analysis
## Loading Necessary Packages
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(scales)
library(textdata)
library(wordcloud)
library(reshape2)
library(tidyverse)
library(textreadr)
library(textshape)
library(pdftools)
library(rvest)
library(Matrix)
library(igraph)
library(ggraph)


# Setting Working Directory 
setwd("/Users/sophiebriques/Desktop/Text Analytics/Group Data")

# Using read document to import the data:
my_doc_text <-read_document(file='questions answers.docx')

# Converting into DataFrame
mydf <- data_frame(line=1:240, text=my_doc_text) #creates 2 variables, 1 is index, 2 is text

# Tokenizing
custom_stop_words <- tribble(~word, ~lexicon, 
                             "i'm", "CUSTOM",
                             "it's", "CUSTOM",
                             "material", "CUSTOM",
                             "materials", "CUSTOM",
                             "they're", "CUSTOM")
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

token_list <- mydf %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE) %>%
  anti_join(stop_words2)

# Frequency Histograms
#freq_hist <- token_list %>%
 # ggplot(aes(word, n))+
  #geom_col()+
  #xlab(NULL)+
  #coord_flip()
#print(freq_hist)

############# Transposing
answers <- read_document(file="/Users/sophiebriques/Desktop/Text Analytics/Group Data /questions answers.docx")
a <- 40 # how many observations to you have
b <- 6  # how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b)) 
# columns to the count of questions we asked
# rows are the number of people

## Transposing
# takes each person's answer and puts in each column
# a person becomes a row 
# it is transposing the data
for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- answers[i*b+z-b]
  }#closing z loop
}#closing i loop
my_df$V7 <- c(0,1,1,0,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,1,0,0,1,1)

###### Tokenizing Per Question
## Setting Up custom stop words
custom_stop_words <- tribble(~word, ~lexicon, 
                             "i'm", "CUSTOM",
                             "it's", "CUSTOM",
                             "material", "CUSTOM",
                             "materials", "CUSTOM",
                             "they're", "CUSTOM")
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)


## tokenize question by question to get frequency insights
question3_txt <- my_df$V3
data_question3 <- data_frame(line=1:a, text=question3_txt)
token_question3 <- data_question3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) 

question4_txt <- my_df$V4
data_question4 <- data_frame(line=1:a, text=question4_txt)
token_question4 <- data_question4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)

question5_txt <- my_df$V5
data_question5 <- data_frame(line=1:a, text=question5_txt)
token_question5 <- data_question5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)

question6_txt <- my_df$V6
data_question6 <- data_frame(line=1:a, text=question6_txt)
token_question6 <- data_question6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)


### Sentiment Analysis
## Setting Up Lexicons
afinn <- get_sentiments("afinn")
bing <- get_sentiments('bing')
nrc <- get_sentiments('nrc')
unique(nrc$sentiment) # these are the nrc options of sentiment labels

my_final_lex <- bind_rows(mutate(afinn, lex_name="afinn"),
                          mutate(bing, lex_name= "Bing"),
                          mutate(nrc, lex_name="nrc"))
my_final_lex %>%
  filter(lex_name == "nrc")

## Using Nrc to see joy -- can change to anything and any question for analysis
#nrcjoy <- get_sentiments("nrc") %>%
 # filter(sentiment == "joy")


###### question 3 - sentiment analysis
# joining nrc
token_question3_s <- token_question3 %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# frequency histograms
token_question3_s %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# word cloud
token_question3_s %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(3,3),
                   fix.asp = TRUE,
                   title.size = 1)

###### question 4 - sentiment analysis
# joining nrc
token_question4_s <- token_question4 %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# frequency histograms
token_question4_s %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# word cloud
token_question4_s %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(3,3),
                   fix.asp = TRUE,
                   title.size = 1)

###### question 5 - sentiment analysis
# joining nrc
token_question5_s <- token_question5 %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

# frequency histograms
token_question5_s %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# word cloud
token_question5_s %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(3,3),
                   fix.asp = TRUE,
                   title.size = 1)

###### question 6 - sentiment analysis
# joining nrc
token_question6_s <- token_question6 %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

# frequency histograms
token_question6_s %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# word cloud
token_question6_s %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(3,3),
                   fix.asp = TRUE,
                   title.size = 1)


################ Second Way to do it #################################
setwd('/Users/sophiebriques/Desktop/Text Analytics/Group Data')

library(textreadr)
library(dplyr)
library(tidytext)
library(ggplot2)

custom_stop_words <- tribble(~word, ~lexicon, 
                             "i'm", "CUSTOM",
                             "it's", "CUSTOM",
                             "material", "CUSTOM",
                             "materials", "CUSTOM",
                             "they're", "CUSTOM")
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

afinn      <- get_sentiments("afinn")
nrc        <- get_sentiments("nrc")
bing       <- get_sentiments("bing")
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)


doc_file <- "questions answers.docx"
answers   <- read_document(file = doc_file)
surveyed  <- c(0, rep(1:(length(answers)-1)%/%6)) + 1
question  <- rep(1:6, time = length(answers)/6)


answers_df <- data.frame(
  id       = paste(surveyed,question,sep = '-'),
  surveyed = surveyed,
  question = question,
  text     = answers
)

answers_df$text <- as.character(answers_df$text)

tidy_answers <- answers_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(custom_stop_words, by="word") %>%
  count(word) %>%
  mutate(word = reorder(word, n))

##################################################
## TF -IDF 
### Need to combine our dataset and need to add location information from our dataset for our question idf,
#location information(question number)
token_question3_count <- token_question3 %>% count(word, sort=TRUE)
token_question4_count <- token_question4 %>% count(word, sort=TRUE)
token_question5_count <- token_question5 %>% count(word, sort=TRUE)
token_question6_count <- token_question6 %>% count(word, sort=TRUE)

my_df <- bind_rows(mutate(token_question3_count, question = "third"),
         mutate(token_question4_count, question = "fourth"),
         mutate(token_question5_count, question = "five"),
         mutate(token_question6_count, question = "six"))

df_words <- my_df %>%
  bind_tf_idf(word, question, n) ## location information and the count
df_words %>%
  arrange(desc(tf_idf))

df_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10,n) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

# Results:
# question 3 - cold
# question 4 - wear
# question 5 - wool
# question 6 - buy

##################################################
## Bigrams
# N-grams (bigrams are best) - and what business insight do they bring, if any?
# question 3:
question3_bigrams <- data_question3 %>%
  unnest_tokens(bigram,text,token = "ngrams", n=2)
question3_bigrams_separated <- question3_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
qt3_bigrams_filtered <- question3_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
qt3_bigram_counts <- qt3_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# question 4:
question4_bigrams <- data_question4 %>%
  unnest_tokens(bigram,text,token = "ngrams", n=2)
question4_bigrams_separated <- question4_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
qt4_bigrams_filtered <- question4_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
qt4_bigram_counts <- qt4_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# question 5:
question5_bigrams <- data_question5 %>%
  unnest_tokens(bigram,text,token = "ngrams", n=2)
question5_bigrams_separated <- question5_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
qt5_bigrams_filtered <- question5_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
qt5_bigram_counts <- qt5_bigrams_filtered %>%
  na.omit() %>%
  count(word1, word2, sort = TRUE)

# question 6:
question6_bigrams <- data_question6 %>%
  unnest_tokens(bigram,text,token = "ngrams", n=2)
question6_bigrams_separated <- question6_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
qt6_bigrams_filtered <- question6_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
qt6_bigram_counts <- qt6_bigrams_filtered %>%
  na.omit() %>%
  count(word1, word2, sort = TRUE)

# Results:
# question 3: cold weather, hate cold, love cold
# question 4: it's cold, leather jacket, wear boots
# question 5: natural materials, prefer cotton, prefer natural
# question 6: buy based, amazon offers, blanket burrito
# No insight yielded -- not include in presentation

################## Cast DTM
dtm_question3 <- data_question3 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE) %>%
  cast_dtm(line, word, n )

dtm_question4 <- data_question4 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE) %>%
  cast_dtm(line, word, n )

dtm_question5 <- data_question5 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE) %>%
  cast_dtm(line, word, n )

dtm_question6 <- data_question6 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE) %>%
  cast_dtm(line, word, n )


my_df_uncounted <- bind_rows(mutate(token_question3, question = "third"),
                             mutate(token_question4, question = "fourth"),
                             mutate(token_question5, question = "five"),
                             mutate(token_question6, question = "six"))

my_dtm <- my_df_uncounted %>%
  count(line, word, sort = TRUE) %>%
  cast_dtm(line, word, n )

## LDA
my_lda <- LDA(my_dtm, k=2, control=list(seed=123))

## Beta
my_topics <- tidy(my_lda, matrix="beta")
top_terms <- my_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# Gamma
my_gamma <- tidy(my_lda, matrix = "gamma")
#LDA analysis via martrix Beta and Gamma 
#The two topics generated via LDA did not provide any significant insights
#we canceled this part in presentation

# Naive Bayes model
# Setting Working Directory 
setwd("C:/Users/junji/OneDrive/??????/my study/MSBA/Module B/text analytics/word")

# Using read document to import the data:
my_doc_text <-read_document(file='new.docx')

#create a new list
my_new_txt<- my_doc_text[1]

#use for loop to put every 6 line into one sentence
j<-1
for(i in 1:53){
  my_new_txt[i]<-paste(unlist(unlist(my_doc_text[j]),my_doc_text[j+1]),unlist(my_doc_text[j+2]),unlist(my_doc_text[j+3]),unlist(my_doc_text[j+4]),unlist(my_doc_text[j+5]))
  j<-6+j
}

# Converting into DataFrame
my_new_df <- data_frame(line=1:53, text=my_new_txt)
my_new_df$bin<-c(0,1,1,0,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,1,0,0,1,1,1,0,0,1,1,1,0,1,0,1,1,0,1)

team_corpus <- corpus(my_new_df$text) 
msg.dfm <- dfm(team_corpus, tolower = TRUE)  
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
msg.dfm <- dfm_weight(msg.dfm)

msg.dfm.train<-msg.dfm[21:53,]
msg.dfm.test<-msg.dfm[1:20,]

#building the Naive Bayes model: choose the business success or failure by the value in column 'bin'
NB_classifier <- textmodel_nb(msg.dfm.train, my_new_df$bin[21:53])
class(NB_classifier)

summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred
# the words('care','material','made'has pretty high probability of success, 
#            and 'natural','handmade','winter'are in low proportion of success)

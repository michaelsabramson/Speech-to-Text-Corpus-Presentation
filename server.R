library(shiny)
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
library(quanteda)
library(RColorBrewer)


shinyServer(function(input, output){
  ###### Data Preparation for plot
  # uploading data
  answers <- read_document(file="/Users/sophiebriques/Desktop/Text Analytics/Group Data /Shiny Dashboard/new.docx")
  a <- 53 
  b <- 6 
  my_df <- as.data.frame(matrix(nrow=a, ncol=b)) 
  # transposing to have all questions
  for(z in 1:b){
    for(i in 1:a){
      my_df[i,z]<- answers[i*b+z-b]
    }
  }
  
  # Adding Binary variable
  my_df$V7 <- c(0,1,1,0,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,1,0,0,1,1,1,0,0,1,1,1,0,1,0,1,1,0,1)
  
  # Creating custom stop words
  custom_stop_words <- tribble(~word, ~lexicon, 
                               "i’m", "CUSTOM",
                               "it's", "CUSTOM",
                               "material", "CUSTOM",
                               "materials", "CUSTOM",
                               "they’re", "CUSTOM")
  stop_words2 <- stop_words %>%
    bind_rows(custom_stop_words)
  
  # Tokenizing Questions
  question1_txt <- my_df$V1
  data_question1 <- data_frame(line=1:a, text=question1_txt)
  token_question1 <- data_question1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2) 
  #
  question2_txt <- my_df$V2
  data_question2 <- data_frame(line=1:a, text=question2_txt)
  token_question2 <- data_question2 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2) 
  #
  question3_txt <- my_df$V3
  data_question3 <- data_frame(line=1:a, text=question3_txt)
  token_question3 <- data_question3 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2) 
  #
  question4_txt <- my_df$V4
  data_question4 <- data_frame(line=1:a, text=question4_txt)
  token_question4 <- data_question4 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2) 
  #
  question5_txt <- my_df$V5
  data_question5 <- data_frame(line=1:a, text=question5_txt)
  token_question5 <- data_question5 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2) 
  #
  question6_txt <- my_df$V6
  data_question6 <- data_frame(line=1:a, text=question6_txt)
  token_question6 <- data_question6 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words2)
  
  #### Preparing Data for TF-IDF:
  token_question3_count <- token_question3 %>% count(word, sort=TRUE)
  token_question4_count <- token_question4 %>% count(word, sort=TRUE)
  token_question5_count <- token_question5 %>% count(word, sort=TRUE)
  token_question6_count <- token_question6 %>% count(word, sort=TRUE)
  
  my_df <- bind_rows(mutate(token_question3_count, question = "third"),
                     mutate(token_question4_count, question = "fourth"),
                     mutate(token_question5_count, question = "five"),
                     mutate(token_question6_count, question = "six"))
  
  df_words <- my_df %>%
    bind_tf_idf(word, question, n)  %>%
    arrange(desc(tf_idf))
  
  
  # Display TF-IDF plot
  output$firstid <- renderPlot(
    if(input$control == "3"){
    df_words %>%
      filter(question == 'third') %>%
      arrange(desc(tf_idf)) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      group_by(question) %>%
      top_n(10,n) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill=question))+
      geom_col(show.legend=FALSE)+
      labs(x=NULL, y="tf-idf")+
      coord_flip()
    } else{
      if(input$control == "4"){
        df_words %>%
          filter(question == 'fourth') %>%
          arrange(desc(tf_idf)) %>%
          mutate(word=factor(word, levels=rev(unique(word)))) %>%
          group_by(question) %>%
          top_n(10,n) %>%
          ungroup %>%
          ggplot(aes(word, tf_idf, fill=question))+
          geom_col(show.legend=FALSE)+
          labs(x=NULL, y="tf-idf")+
          coord_flip()
      } else{
        if(input$control == "5"){
          df_words %>%
            filter(question == 'five') %>%
            arrange(desc(tf_idf)) %>%
            mutate(word=factor(word, levels=rev(unique(word)))) %>%
            group_by(question) %>%
            top_n(10,n) %>%
            ungroup %>%
            ggplot(aes(word, tf_idf, fill=question))+
            geom_col(show.legend=FALSE)+
            labs(x=NULL, y="tf-idf")+
            coord_flip()
        } else{
          if(input$control == "6"){
            df_words %>%
              filter(question == 'six') %>%
              arrange(desc(tf_idf)) %>%
              mutate(word=factor(word, levels=rev(unique(word)))) %>%
              group_by(question) %>%
              top_n(10,n) %>%
              ungroup %>%
              ggplot(aes(word, tf_idf, fill=question))+
              geom_col(show.legend=FALSE)+
              labs(x=NULL, y="tf-idf")+
              coord_flip()
          }
        }
      }
    }
  )
  
  
  
  # Display Frequency Plot
  output$thisisanid <- renderPlot( 
  
    if(input$control1 == "2"){
      token_question2 %>%
        count(word, sort=TRUE) %>%
        top_n(as.integer(input$control2),n) %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
     
    } else{
      if(input$control1 == "3"){
        token_question3 %>%
          count(word, sort=TRUE) %>%
          top_n(as.integer(input$control2),n) %>%
          mutate(word=reorder(word, n)) %>%
          ggplot(aes(word, n))+
          geom_col()+
          xlab(NULL)+
          coord_flip()
        
      } else{
        if(input$control1 == "4"){
          token_question4 %>%
            count(word, sort=TRUE) %>%
            top_n(as.integer(input$control2),n) %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n))+
            geom_col()+
            xlab(NULL)+
            coord_flip()
          
        } else{
          if(input$control1 == "5"){
          token_question5 %>%
            count(word, sort=TRUE) %>%
            top_n(as.integer(input$control2),n) %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n))+
            geom_col()+
            xlab(NULL)+
            coord_flip()
          
          } else{
            if(input$control1 == "6"){
              token_question6 %>%
                count(word, sort=TRUE) %>%
                top_n(as.integer(input$control2),n) %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n))+
                geom_col()+
                xlab(NULL)+
                coord_flip()
              
            }
          }
        }
      }
    } 
  
    )
  
  ###### Preparing data for sentiment word clouds
  #
  question3_s <- token_question3 %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  negative3 <- question3_s[question3_s$sentiment == "negative",]
  positive3 <- question3_s[question3_s$sentiment == "positive",]
  #
  question4_s <- token_question4 %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  negative4 <- question4_s[question4_s$sentiment == "negative",]
  positive4 <- question4_s[question4_s$sentiment == "positive",]
  #
  question5_s <- token_question5 %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  negative5 <- question5_s[question5_s$sentiment == "negative",]
  positive5 <- question5_s[question5_s$sentiment == "positive",]
  #
  question6_s <- token_question6 %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  negative6 <- question6_s[question6_s$sentiment == "negative",]
  positive6 <- question6_s[question6_s$sentiment == "positive",]
  
  
  # Displaying Word Clouds
  output$thisisanid2 <- renderPlot(
    if(input$control3 == "3"){
      if(input$control4 == "Positive"){
        wordcloud(positive3$word, positive3$n, scale= c(4,2),min.freq = 0, max.words = 100, colors = "dark green")
      } else {
        wordcloud(negative3$word, negative3$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "red")
    
        }   
    } else if(input$control3 == "4"){
      if(input$control4 == "Positive"){
        wordcloud(positive4$word, positive4$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "dark green")
      }else{
        wordcloud(negative4$word, negative4$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "red")
      }
    } else if(input$control3 == "5"){
      if(input$control4 == "Positive"){
        wordcloud(positive5$word, positive5$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "dark green")
      }else{
        wordcloud(negative5$word, negative5$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "red")
      }
    } else{
      if(input$control3 == "6"){
        if(input$control4 == "Positive"){
          wordcloud(positive6$word, positive6$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "dark green")
        }else{
          wordcloud(negative6$word, negative6$n, scale= c(4,2), min.freq = 0, max.words = 100, colors = "red")
        }
      }
    }
  )
  
    ###### Preparing Data for Naive Bayes Predictions
    #create a new list
    my_new_txt <- answers[1]
    
    #use for loop to put every 6 line into one sentence
    j <- 1
    for(i in 1:53){
      my_new_txt[i]<-paste(unlist(unlist(answers[j]),answers[j+1]),unlist(answers[j+2]),unlist(answers[j+3]),unlist(answers[j+4]),unlist(answers[j+5]))
      j<-6+j
    }
    
    # Converting into DataFrame
    my_new_df <- data_frame(line=1:53, text=my_new_txt)
    my_new_df$bin<-c(0,1,1,0,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,1,0,0,1,1,1,0,0,1,1,1,0,1,0,1,1,0,1)
    
    twitter_corpus <- corpus(my_new_df$text) #creating the corpus on the $text var
    msg.dfm <- dfm(twitter_corpus, tolower = TRUE) #generating document 
    msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
    msg.dfm <- dfm_weight(msg.dfm)#, type = "tfidf")
    
    msg.dfm.train<-msg.dfm[1:40,]
    msg.dfm.test<-msg.dfm[41:53,]
    
    #building the Naive Bayes model:
    NB_classifier <- textmodel_nb(msg.dfm.train, my_new_df$bin[1:40]) # we need to tell which 1 and 0 to use
    
    # Outputting Summary Results
    output$thisisanid3 <- renderPrint(
      print(summary(NB_classifier))
    )
    
    #output$thisisanid4 <- renderText(
    #  print(summary(NB_classifier))
    #)
  
    
 }
)

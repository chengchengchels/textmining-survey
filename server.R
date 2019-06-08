#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(textreadr)
MIB <- read_document(file="sppech record.docx")
my_df <- as.data.frame(matrix(nrow=65, ncol=5))
for(z in 1:5){for(i in 1:65){my_df[i,z]<- MIB[i*5+z-5]}}

my_df<-cbind(seq(1:65),my_df)#add a sequence column to the front,not end.
colnames(my_df) <- c("ID","Gender", "Age", "Pro", "Con", "Attitude")##change the column names

#p.s. when cleaning data: variable name should be exactly the same, e.g. negative v.s. Negative won't work.

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  library(textreadr)
  library(readr)
  library(tidyr)
  library(dplyr)
  library(tidytext)
  library(ggplot2)
  library(wordcloud)
  library(reshape2)
  positive<-my_df%>%select(Pro)%>%
    unnest_tokens(word,Pro)%>%anti_join(stop_words)%>%count(word,sort = TRUE)
  negative<-my_df%>%select(Con)%>%unnest_tokens(word,Con)%>%anti_join(stop_words)%>%count(word,sort=TRUE)
  
  
  output$age_p <- renderPlot({
    p_age<-my_df%>%select(Age,Pro)%>%
      unnest_tokens(word,Pro)%>%anti_join(stop_words)
    
    p_afinn <- p_age %>% 
      inner_join(get_sentiments("afinn")) %>% 
      group_by(Age) %>% 
      summarise(sentiment = sum(score)) %>% 
      mutate(method = "AFINN") 
    
    p_bing_and_nrc <- bind_rows(p_age %>% 
                                  inner_join(get_sentiments("bing")) %>%
                                  mutate(method = "Bing et al."),
                                p_age%>% 
                                  inner_join(get_sentiments("nrc")%>% 
                                               filter(sentiment %in% c("positive", 
                                                                       "negative"))) %>%
                                  mutate(method = "NRC")) %>%
      count(method, Age,sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) 
    
    bind_rows(p_afinn, 
              p_bing_and_nrc) %>%
      ggplot(aes(Age, sentiment, fill = method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y")+
      theme(text  = element_text(size=18))
    

  })
  
  
  output$age_n <- renderPlot({
    n_age<-my_df%>%select(Age,Con)%>%
      unnest_tokens(word,Con)%>%anti_join(stop_words)
    
    n_afinn <- n_age %>% 
      inner_join(get_sentiments("afinn")) %>% 
      group_by(Age) %>% 
      summarise(sentiment = sum(score)) %>% 
      mutate(method = "AFINN") 
    
    n_bing_and_nrc <- bind_rows(n_age %>% 
                                  inner_join(get_sentiments("bing")) %>%
                                  mutate(method = "Bing et al."),
                                n_age%>% 
                                  inner_join(get_sentiments("nrc")%>% 
                                               filter(sentiment %in% c("positive", 
                                                                       "negative"))) %>%
                                  mutate(method = "NRC")) %>%
      count(method, Age,sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) 
    
    bind_rows(n_afinn, 
              n_bing_and_nrc) %>%
      ggplot(aes(Age, sentiment, fill = method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y")+
      theme(text=element_text(size=18))
    
  })
  
  
  output$wordcloud1 <- renderPlot({
    all<-rbind(positive,negative)
    all%>% with(wordcloud(word,n,scale=c(4,4)))
    
  })
  output$wordcloud2 <- renderPlot({
    all<-rbind(positive,negative)##这个all 虽然在last plot 里define了，但每个following用到它的plot里，也要重新assign value to object一下？？
    all %>%
      inner_join(get_sentiments("bing")) %>%
      #count(word, sentiment, sort = TRUE) %>% 我的raw subeject（positive,negative)都count过了，不需此行
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(scale=c(3,3))
    #export as image-update the preview (1500*1800) to view the full image
  
  })
  
  output$contribution <- renderPlot({
    #find out how much each word contributed to each sentiment.
    p_word_counts <- positive %>%
      inner_join(get_sentiments("bing")) %>%
      ungroup()
    p_word_counts
    
    n_word_counts <- negative %>%
      inner_join(get_sentiments("bing")) %>%
      ungroup()
    n_word_counts
    
    
    bing_word_counts<-bind_rows(p_word_counts,n_word_counts)
    bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      theme(title=element_text(size=18,face="bold"))+
      theme(text=element_text(size=20))+
      coord_flip()
    
  })
  
  output$tf_idf <- renderPlot({  
    all<-rbind(positive,negative)  
    all_ti <-all %>%
    inner_join(get_sentiments("bing"))%>%
    bind_tf_idf(word,sentiment, n)
  # looking at the graphical apprach:
  
  all_ti %>%
    arrange(desc(tf_idf)) %>%
    mutate(word=factor(word, levels=rev(unique(word)))) %>%
    group_by(sentiment) %>%
    top_n(15) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill=sentiment))+
    geom_col(show.legend=FALSE)+
    labs(x=NULL, y="tf-idf")+
    facet_wrap(~sentiment, ncol=2, scales="free")+
    theme(title=element_text(size=18,face="bold"))+
    theme(text=element_text(size=20))+
    coord_flip()
    
  
  })
  
  
  output$bigram_p <- renderPlot({
    
    p_bigrams<-my_df%>%select(Pro)%>%unnest_tokens(bigram, Pro, token = "ngrams", n=2)%>%count(bigram,sort = TRUE)
    p_bigrams #We want to see the bigrams (words that appear together, "pairs")
    
    p_bigrams %>%
      count(bigram, sort = TRUE) #this has many stop words, need to remove them 
    

    bigrams_separated <- p_bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%   
      filter(!word2 %in% stop_words$word)
    
    library(igraph)
    bigram_graph <- bigrams_filtered %>%
      graph_from_data_frame()
    
    
    library(ggraph)
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1,size=5)
    
  })
  
  output$bigram_n <- renderPlot({
    ###negative
    n_bigrams<-my_df%>%select(Con)%>%unnest_tokens(bigram, Con, token = "ngrams", n=2)%>%count(bigram,sort = TRUE)
    n_bigrams
    
    n_bigrams %>%
      count(bigram, sort = TRUE) 
    
    bigrams_separated2 <- n_bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered2 <- bigrams_separated2 %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    
    
    bigram_graph2 <- bigrams_filtered2 %>%
      graph_from_data_frame()
    
    ggraph(bigram_graph2, layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1,size=5)
  })
  
})

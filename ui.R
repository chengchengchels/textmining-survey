#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Analysis: MyCourses Feedbacks"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("age_p", label = h3("Positive Attitude & Age w/ different Lexicons"),value="1.1 attitude & age"),
      textInput("age_n", label = h3("Negative Attitude & Age w/ different Lexicons"),value="1.2 attitude & age"),
      textInput("wordcloud1", label = h3("Wordcloud: the most common words"), value="2.1 wordcloud"),
      textInput("wordcloud2", label = h3("Wordcloud w/ Sentiment Analysis"), value="2.2 wordcloud"),
      textInput("contribution", label = h3("Word Contributions"), value="3.1 sentiment from 'bing'"),
      textInput("tf_idf", label = h3("Words important to each sentiment"), value="3.2 tf-idf: graphical approach"),
      textInput("bigram_p", label = h3("Tokenizing by Bigram"), value="4.1 from LIKEs"),
      textInput("bigram_n", label = h3("Tokenizing by Bigram"), value="4.2 from HATEs")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Insight 1.1", plotOutput("age_p",  width = "100%", height = "800px")),
        tabPanel("Insight 1.2", plotOutput("age_n",  width = "100%", height = "800px")),
        tabPanel("Insight 2.1", plotOutput("wordcloud1",  width = "100%", height = "1000px")),
        tabPanel("Insight 2.2", plotOutput("wordcloud2",  width = "100%", height = "800px")),
        tabPanel("insight 3.1", plotOutput("contribution",  width = "100%", height = "800px")),
        tabPanel("Insight 3.2", plotOutput("tf_idf",  width = "100%", height = "800px")),
        tabPanel("Insight 4.1", plotOutput("bigram_p",  width = "100%", height = "800px")),
        tabPanel("insight 4.2", plotOutput("bigram_n",  width = "100%", height = "800px"))
        
      )
      
    )
  )
))

library(shinydashboard)
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
## any plot library


shinyUI(                        ## the page
    dashboardPage(                 ##the layout of the page is going to follow a dashboard layout
       header = dashboardHeader(       ## defining the header (always keep it header)
           title = "Customer Preferences" ## you can other things as well
       ),
       sidebar = dashboardSidebar(     ## dashboardsidebar is the name of the control, sidebar is what you are doing
           sidebarMenu( ## collection of tabs
               menuItem(
                   "Unique Questions",
                   tabName = "tab"
               ),
               menuItem( ## this are the tabs
                   "Common Words",    ## this it the title
                   tabName = "tab1"     ## calling it an object to connect it to our body, no space
               ),
               menuItem(
                   "Sentiments",
                   tabName = "tab2"
               ),
               menuItem(
                   "Predictions",
                   tabName = "tab3"
               )
                   
           ) ## closing sidebarMenu --> you only define tabs
       ), ## closing dashboardSidebat
       body = dashboardBody(
           tabItems(                 ## connect to tab, first in plural because its the collection of tabs
               tabItem(
                   tabName = "tab",
                   fluidPage(
                       titlePanel("Unique Key Words per Question"),
                       sidebarPanel(
                           selectInput(
                               inputId = "control",  ## each control requires a name or id
                               label = "Question Number", ## have a text attached to guide the user
                               choices = c("3","4","5","6"),
                               selected = "3"
                           )
                       ),
                       mainPanel(
                           plotOutput("firstid")
                       )
                   )
               ),
               tabItem(
                   tabName = 'tab1', ## the name of your tab from sidebar
                   fluidPage(        ## to put in the actual content 
                       titlePanel("Frequency Histograms per Question"),
                       sidebarPanel( ## sidebar inside the body - you can add you inputs
                           selectInput( ## this creates the control 
                               ## controls can be different types (check datacamp)
                               ## each control type will have a number of parameters you need to spcify
                               inputId = "control1",  ## each control requires a name or id
                               label = "Question Number", ## have a text attached to guide the user
                               choices = c("2","3","4","5","6"),
                               selected = "2"
                           ) 
                       ),
                       sidebarPanel( 
                           sliderInput( 
                               inputId = "control2",  ## each control requires a name or id
                               label = "Top Frequency", ## have a text attached to guide the user
                               min = 2,
                               max = 10,
                               value = 5
                           ) 
                       ),
                       mainPanel(                  ## your main content page part
                           plotOutput("thisisanid"), 
                           textOutput("anotherid")
                       )
                   )
               ),
               tabItem(
                   tabName = "tab2",
                   fluidPage(        ## to put in the actual content 
                       titlePanel("Sentiment Word Clouds"),
                       sidebarPanel( ## sidebar inside the body - you can add you inputs
                           selectInput(
                               inputId = "control3", 
                               label = "Question Number", 
                               choices = c("3","4","5","6"),
                               selected = "3"
                           ),
                           radioButtons(
                               inputId = "control4",
                               label = "Type of Sentiment",
                               choices = c("Positive","Negative"),
                               selected = "Positive"
                           )
                       ),
                       mainPanel(                  ## your main content page part
                           plotOutput("thisisanid2")
                           )
                   )
               ),
               tabItem(
                   tabName = "tab3",
                   fluidPage(
                       titlePanel("Machine Learning Predictions"),
                       mainPanel(
                           textOutput("thisisanid3")
                           #textOutput("thisisanid4")
                       )
                       )
                   )
                )
        ) # dahsboardBody close
       )
)
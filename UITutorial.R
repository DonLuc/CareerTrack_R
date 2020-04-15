#install.packages("rtweet")
library(rtweet)

# plotting and pipes - tidyverse!
# install.packages("ggplot2")
library("ggplot2")
# install.packages("dplyr")
library("dplyr")

#Text-mining library
# install.packages("tidytext")
library("tidytext")

# Plotting packages
#install.packages("igraph")
library("igraph")
#install.packages("ggraph")
library("ggraph")

title <- "CareerTrack"
twitter_handle <- "Twitter Handle"
about <- "Use your Twitter account to determine your possible career paths based on what you normally tweet about."
about_title <- "About CareerTrack"
ui <- fluidPage(
  titlePanel(title),
  sidebarLayout(
    sidebarPanel(
      h3(about_title),
      helpText(about),
      hr(),
      textInput("twitterHandle", h4(twitter_handle), 
                value = "", placeholder = "Twitter handle"),   
    
      selectInput("includeRTs", h4("Include retweets?"), 
                  choices = list("Yes" = 1, "No" = 2)
      ),
      actionButton("process", "Process")
    ),
    mainPanel(
      fluidRow(
        column(6,
               # h4(textOutput("processed")),    
               # dataTableOutput("processed")
               
               fluidRow(
                  column(4,
                        uiOutput("profile_image")         
                  ),
                  column(8,
                         fluidRow(
                            column(12,
                                   textOutput("name")
                            ),
                            column(12,
                                   textOutput("location")
                            ),
                            column(12,
                                   textOutput("followers")
                            ),
                            column(12,
                                   textOutput("following")
                            ),
                            column(12,
                                   textOutput("description")
                            )
                           
                         )
                  ),
                  fluidRow(
                    column(6, 
                      uiOutput("recent_tweets")       
                    )
                    
                  )
                  
                  
               )
               
               
        ),
        column(6,
               # textOutput("include")
               #uiOutput("profile_image")
        )
      )
      
      
    )
  )
  
)
server <- function(input, output){
  observeEvent(input$process, {
    user <- search_users(input$twitterHandle, n = 1)
    tweets <- search_tweets("drone OR technology or finance", n = 20, include_rts = TRUE)
    output$processed <- renderDataTable({
      # paste("Twitter handle", input$twitterHandle)
      #search_users("@bacee2", n = 1)
    })
    output$recent_tweets <- renderUI({
      tweets
    })
    output$include <- renderText({
      paste('Include Retweets? ', input$includeRTs)
    })
    output$profile_image <- renderUI({
      tags$img(src = user$profile_image_url, height = 100, width = 100)
    })
    output$name <- renderText({
      paste("Name", user$name) 
    })
    output$location <- renderText({
      paste("Location", user$location) 
    })
    output$followers <- renderText({
      paste("Followers: ", user$followers_count)
    })
    output$following <- renderText({
      paste("Following: ", user$friends_count)
    })
    output$description <- renderText({
      paste("About: ", user$description)
    })
    
  })
  
}
shinyApp(ui = ui, server = server)

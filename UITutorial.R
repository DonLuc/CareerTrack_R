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

library(reactable) 


make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}


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
      actionButton("process", "Process", class = "btn-primary btn-block")
    ),
    mainPanel(
      fluidRow(
        column(6,
               # h4(textOutput("processed")),    
               # dataTableOutput("processed")
               h4("User Profile"),
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
                           #reactableOutput("recent_tweets")       
                    )
                    
                  )
                  
                  
               )
               
               
        ),
        column(6,
               h4("Recent tweets"),
               # textOutput("include")
               #uiOutput("profile_image")
               reactableOutput("recent_tweets")
               #textOutput("testing")
        )
      )
      
      
    )
  )
  
)
server <- function(input, output){
  user = eventReactive(input$process, {
    search_users(input$twitterHandle, n = 1)
  })
  
  tweets_df = eventReactive(input$process, {
    search_tweets(input$twitterHandle, n = 10, include_rts = FALSE)
  })
  
  tweets_table_data = eventReactive(input$process, {
    req(tweets_df())
    tweets_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$profile_image <- renderUI({
    tags$img(src = user()$profile_image_url, height = 100, width = 100)
  })
  
  output$name <- renderText({
    paste("Name", user()$name) 
  })
  output$location <- renderText({
    paste("Location", user()$location) 
  })
  output$followers <- renderText({
    paste("Followers: ", user()$followers_count)
  })
  output$following <- renderText({
    paste("Following: ", user()$friends_count)
  })
  output$description <- renderText({
    paste("About: ", user()$description)
  })
  
  output$recent_tweets <- renderReactable({
    reactable::reactable(tweets_table_data(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         height = 400,
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })
  
  
  
}
shinyApp(ui = ui, server = server)

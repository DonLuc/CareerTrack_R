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
      h4(textOutput("processed")),
      textOutput("include")
      
    )
  )
  
)
server <- function(input, output){
  observeEvent(input$process, {
    output$processed <- renderText({
      paste("Twitter handle", input$twitterHandle)
    })
    output$include <- renderText({
      paste('Include Retweets? ', input$includeRTs)
    })
    
  })
  
}
shinyApp(ui = ui, server = server)

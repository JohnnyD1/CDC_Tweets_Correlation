#setwd("/Users/JohnnyD/Documents/CSE487/Lab1EDApart3/ShinyFluTweetsApp")
library('shiny')
# ok the goal is to have to 
#setwd("/Users/JohnnyD/Documents/CSE487/Lab1EDApart3/ShinyFluTweetsApp")
ui <- fluidPage(

  titlePanel("Relationship Between Flu Keywords Mentioned in Tweets And Actual Flu Cases in the US",windowTitle=
               "Relationship Between Tweets Mentioning the Flu\n And Actual Flu Cases in the US"),
  sidebarPanel(
    fluidRow(
    selectInput("variable1","Compare",
                c("CDC"="HeatMapInfluenza", "all keywords"="HeatMapMultKeys", 
                  "flu season"="HeatMapFluSeasonKey","#flu"="HeatMapHashFluKey"))
    )
  ),
  sidebarPanel(
    fluidRow(
      selectInput("variable2","with",
                  c("CDC"="HeatMapInfluenza", "all keywords"="HeatMapMultKeys", 
                    "flu season"="HeatMapFluSeasonKey","#flu"="HeatMapHashFluKey"))
    )
  ),
  

  mainPanel(
    splitLayout(
      cellWidths = c("50%", "50%"),
      imageOutput("first"),
      imageOutput("second")
    ),width=12
  )
)


library('png')
server <- function(input, output){
  output$first <- renderImage({
    filename <- normalizePath(file.path('./www',
                                        paste(input$variable1, '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename,width=560,height=400)

  }, deleteFile=FALSE)
  output$second <- renderImage({
    filename <- normalizePath(file.path('./www',
                                        paste(input$variable2, '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename,width=560,height=400)
    
  }, deleteFile=FALSE)
}
shinyApp(ui=ui,server=server)

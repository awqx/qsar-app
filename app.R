library(shiny)
source("./functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cyclodextrin Affinity Prediction"),
   
   # Start with a file input for .csv of descriptors
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("padel",
                   "PaDEL-Descriptor Output",
                   multiple = T, 
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")), 
         br(), 
         checkboxGroupInput("cd", "Cyclodextrin Type", 
                            choices = list("Alpha" = "a", "Beta" = "b", "Gamma" = "c"))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating a reactive dataframe of ensemble prediction results
  # that responds to loading a file
  pred <- reactive({
    # Checks if a file has been loaded
    if(is.null(input$padel))
      return(NULL)
    
    # The local path for the file is stored in datapath
    raw <- read.csv(input$padel$datapath, header = T)
    
    
  })
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


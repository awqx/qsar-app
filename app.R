# install.packages("ggvis")
library(shiny)
library(ggvis)
library(data.table)
source("./functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cyclodextrin Affinity Prediction"),
   
   navbarPage(
     theme = "yeti", 
     "Cyclodextrin", 
     tabPanel("Upload", 
              sidebarPanel(
                # Start with a file input for .csv of descriptors
                # Sidebar with a slider input for number of bins 
                fileInput("padel",
                          "PaDEL-Descriptor Output",
                          multiple = T, 
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")), 
                br(), 
                checkboxGroupInput("cd", "Cyclodextrin Type", 
                                   choices = list("Alpha" = "a", "Beta" = "b")) 
                # "Gamma" = "c" not available yet
                
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                # ggvisOutput("predPlot"), 
                tableOutput("predTable")
              )
              ), 
     tabPanel("Download"), 
     tabPanel("Explore")
   )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # Creating a dataframe of ensemble prediction results
  # that responds to pre-processing

  
  output$predTable <- renderTable({
    if(is.null(input$padel))
      return(NULL)
    if(is.null(input$cd))
      return(NULL)
    
    raw <- read.csv(input$padel$datapath, header = T)
    alpha <- data.frame()
    beta <- data.frame()
    
    if("a" %in% input$cd) {
      
      pp.a <- pp.desc(raw, alpha.vars, alpha.pp, alpha.fill) %>%
        dplyr::rename(guest = guests) 
      ad <- domain.num(pp.a) %>% select(., guest, domain)
      qsar <- ensemble(pp.a, alpha.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      alpha <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Alpha")  %>%
        mutate(guest = as.character(guest))# %>%
      #   rename(Guest = guest, `Binding, kJ/mol` = ensemble, `Applicability` = domain)
    }
    
    if("b" %in% input$cd) {
      
      pp.b <- pp.desc(raw, beta.vars, beta.pp, beta.fill) %>%
        dplyr::rename(guest = guests) 
      ad <- domain.num(pp.b) %>% select(., guest, domain)
      qsar <- ensemble(pp.b, beta.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      beta <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Beta")  %>%
        mutate(guest = as.character(guest)) # %>%
      #   rename(Guest = guest, `Binding, kJ/mol` = ensemble, `Applicability` = domain)
    }
    
    rbind(alpha, beta)
    
  })
   # 
   # output$distPlot <- renderPlot({
   #   return(NULL)
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)


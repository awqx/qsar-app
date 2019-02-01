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
  
  # Creates a reactive dataframe of pre-processed values
  # An intermediate step for applicability domain as well as prediction
  
  pp <- reactive({
    if(is.null(input$padel))
      return(NULL)
    if(is.null(input$cd))
      return(NULL)
    raw <- read.csv(input$padel$datapath, header = T)
    out <- data.frame()
    if("a" %in% input$cd) {
      pp.a <- pp.desc(raw, alpha.vars, alpha.pp, alpha.fill) %>%
        rename(guest = guests) %>%
        mutate(cd = "a")
      out <- rbind(out, pp.a)
    }
    if("b" %in% input$cd) {
      pp.b <- pp.desc(raw, beta.vars, beta.pp, beta.fill) %>%
        rename(guest = guests) %>%
        mutate(cd = "b")
      out <- rbind(out, pp.b)
    }
    out
  })
  
  # Creating a reactive dataframe of ensemble prediction results
  # that responds to pre-processing
  pred <- reactive({

    # Checks if a file has been loaded
    if(is.null(input$padel))
      return(NULL)
    if(is.null(input$cd))
      return(NULL)
    # Creating a blank to rbind results to
    alpha.results <- data.frame()
    beta.results <- data.frame()

    if("a" %in% input$cd) {

      # # Pre-processing the descriptors
      # desc <- pp.desc(raw, alpha.vars, alpha.pp, alpha.fill) %>%
      #   rename(guest = guests)
      # # Creating applicability domain
      # app.domain <- domain.num(df) %>% select(., guest, domain)
      # Ensemble prediction, only returning the ensemble value
      alpha.df <- pp() %>% filter(cd == "a") %>% select(., -cd)
      ad <- domain.num(alpha.df) %>% select(., guest, domain)
      qsar <- ensemble(alpha.df, alpha.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      alpha.results <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Alpha") # %>%
      #    rename(Guest = guest, `Binding, kJ/mol` = ensemble, `Applicability` = domain)

    }
    
    if("b" %in% input$cd) {

      beta.df <- pp() %>% filter(cd == "b") %>% select(., -cd)
      ad <- domain.num(beta.df) %>% select(., guest, domain)
      qsar <- ensemble(beta.df, beta.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      beta.results <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Beta") # %>%
      #   rename(Guest = guest, `Binding, kJ/mol` = ensemble, `Applicability` = domain)

    }
    
    # for some reason rbind() doesn't work - states there are different numbers of columns
    # which is not correct
    

  })
  
  output$predTable <- renderTable({
    if(is.null(input$cd))
      return(NULL)
    pred()
  })
   # 
   # output$distPlot <- renderPlot({
   #   return(NULL)
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)


# install.packages("ggvis")
library(shiny)
library(ggvis)
library(data.table)

source("./functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("yeti"),
   # Application title
   # titlePanel("Cyclodextrin Affinity Prediction"),
   
   navbarPage(
     theme = "yeti", 
     "Cyclodextrin Affinity Predictions", 
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
                                   choices = list("Alpha" = "a", "Beta" = "b")),
                br(), 
                radioButtons("xaxis", "Variable on x-axis", 
                             c("Molecular weight" = "MW",
                               "Number of atoms" = "nAtom", 
                               "Applicability" = "ad")), 
                br(), 
                sliderInput("yrange", "Range of predictions, kJ/mol", 
                            min = -50, max = 50, value = c(-30, 10)),
                # "Gamma" = "c" not available yet
                br(), 
                uiOutput("xaxisRange")
                
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                tabsetPanel(
                  tabPanel("Table", DT::dataTableOutput("predTable")),
                  tabPanel("Plot", plotOutput("predPlot")) 
                )
              )
              ), 
     tabPanel("Download"), 
     tabPanel("Explore")
   )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating a dataframe that will be attached to prediction results
  # and will provide the x-axis to the graph
  
  choose.x <- reactive({
    if(is.null(pred()))
      return(NULL)
    
    if (input$xaxis == "ad")
      pred() %>% select(., guest)
    else
      read.csv(input$padel$datapath, header = T) %>%
        rename(guest = Name) %>%
        select(., guest,!!input$xaxis)
    
  })

  # Creating a dataframe of ensemble prediction results
  # that responds to pre-processing

  pred <- reactive({
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
      ad <- domain.num(pp.a) %>% select(., guest, newSk, domain)
      qsar <- ensemble(pp.a, alpha.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      alpha <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Alpha")  %>%
        mutate(guest = as.character(guest), 
               ensemble = signif(ensemble, digits = 3))
    }
    
    if("b" %in% input$cd) {
      
      pp.b <- pp.desc(raw, beta.vars, beta.pp, beta.fill) %>%
        dplyr::rename(guest = guests) 
      ad <- domain.num(pp.b) %>% select(., guest, newSk, domain)
      qsar <- ensemble(pp.b, beta.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      beta <- full_join(ad, qsar, by = "guest") %>% mutate(CD = "Beta")  %>%
        mutate(guest = as.character(guest), 
               ensemble = signif(ensemble, digits = 3))
    }
    
    rbind(alpha, beta)
  })
  
  output$predPlot <- renderPlot({
    if(is.null(input$cd) || is.null(input$padel))
      return()
    if(input$xaxis == "ad")
      ggplot(pred(), aes(y = ensemble, 
                         x = newSk, 
                         color = CD, 
                         shape = domain)) + 
      geom_point() + 
      theme_bw() + 
      lims(x = input$xrange, y = input$yrange) + 
      labs(x = "newSk", y = "Binding, kJ/mol")
    else
      ggplot(full_join(pred(), choose.x(), by = "guest"), 
             aes_string(y = "ensemble", 
                        x = input$xaxis, 
                        color = "CD", 
                        shape = "domain")) + 
        geom_point() + 
        theme_bw() + 
        lims(x = input$xrange, y = input$yrange) + 
        labs(x = input$xaxis, y = "Binding, kJ/mol") 
  })
  
  # A table that displays guest name, predicted affinity, applicability domain, 
  # and the chosen x variable
  output$predTable <- DT::renderDataTable({
    if(is.null(pred()))
      NULL
    pred() %>% 
      full_join(., choose.x(), by = "guest") %>%
      rename(Guest = guest, `Binding, kJ/mol` = ensemble, 
             `Applicability` = domain) %>%
      select(., -newSk)
  })
  
  # A slider for adjusting the x-axis depending on the variable
  output$xaxisRange <- renderUI({
    if(is.null(pred()))
      NULL
    if(input$xaxis == "ad")
      xrange <- c(0.0, 6.5)
    else
      xrange <- choose.x()[ , 2] %>% range() %>% round()

    sliderInput("xrange", "Range of x-axis", 
                min = xrange[1], max = xrange[2], xrange)
  })
  
   # 
   # output$distPlot <- renderPlot({
   #   return(NULL)
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)


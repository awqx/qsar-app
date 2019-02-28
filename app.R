# install.packages("ggvis")
library(shiny)

source("./functions.R")
p_load(data.table, shiny, shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("yeti"),
   # Application title
   # titlePanel("Cyclodextrin Affinity Prediction"),
   
   navbarPage(
     theme = "yeti", 
     "Cyclodextrin Affinity Predictions", 
     tabPanel(
       "Upload", 
       fluidRow(
         column(
           2, offset = 2,
          # Start with a file input for .csv of descriptors
          # Sidebar with a slider input for number of bins 
          fileInput("padel",
                    h5("PaDEL-Descriptor Output"),
                    multiple = T, 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")), 
          br(), 
          checkboxGroupInput("cd", h5("Cyclodextrin Type"), 
                             choices = list("Alpha" = "a", "Beta" = "b")),
          br(), 
          radioButtons("xaxis", h5("Variable on x-axis"), 
                       c("Molecular weight" = "MW",
                         "Number of atoms" = "nAtom", 
                         "Applicability" = "ad")), 
          br(), 
          sliderInput("yrange", h5("Range of predictions, kJ/mol"), 
                      min = -50, max = 50, value = c(-30, 10)),
          # "Gamma" = "c" not available yet
          br(), 
          uiOutput("xaxisRange"), 
          br(),
          downloadButton('downloadPred', 'Download Results')
         ),
         column(
           6,
           tabsetPanel(
             tabPanel("Table", DT::dataTableOutput("predTable")),
             tabPanel("Plot", plotOutput("predPlot")) 
           )
         )
      )),
     tabPanel("Download", 
              fluidRow(
                column(
                  2, offset = 2, 
                  textInput("search", 
                            label = h5("Chemical Identifier Resolver")),
                  actionButton("searchButton", "Search"),
                  br(), 
                  br(),
                  downloadButton('downloadData', 'Download')
                  
                ), 
                column(
                  6, 
                  uiOutput("filePreview"), 
                  DT::dataTableOutput("SDFTable")
                )
              )), 
     tabPanel("Explore", 
              fluidRow(
                column(
                  2, offset = 2, 
                  textInput("search", 
                            label = h5("Guest Name")), 
                  actionButton("searchExplore", "Search")
                )
              ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # A slider for adjusting the x-axis depending on the variable
  output$xaxisRange <- renderUI({
    if(is.null(pred()))
      return()
    if(input$xaxis == "ad")
      xrange <- c(0.0, 6.5)
    else
      xrange <- choose.x()[ , 2] %>% range() %>% round()
    
    sliderInput("xrange", "Range of x-axis", 
                min = xrange[1], max = xrange[2], xrange)
  })
  
  # Creating a dataframe that will be attached to prediction results
  # and will provide the x-axis to the graph
  
  choose.x <- reactive({
    if(is.null(input$cd) || is.null(input$padel))
      return()
    
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
    if(is.null(input$cd) || is.null(input$padel))
      return()
    
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
      scale_shape_manual(values = c(16, 4)) + 
      labs(x = "newSk", y = "Binding, kJ/mol", 
           color = "Cyclodextrin", shape = "Applicability")
    else
      ggplot(full_join(pred(), choose.x(), by = "guest"), 
             aes_string(y = "ensemble", 
                        x = input$xaxis, 
                        color = "CD", 
                        shape = "domain")) + 
        geom_point() + 
        theme_bw() + 
        lims(x = input$xrange, y = input$yrange) + 
        scale_shape_manual(values = c(16, 4)) + 
        labs(x = input$xaxis, y = "Binding, kJ/mol", 
             color = "Cyclodextrin", shape = "Applicability") 
  })
  
  # A table that displays guest name, predicted affinity, applicability domain, 
  # and the chosen x variable
  output$predTable <- DT::renderDataTable({
    if(is.null(pred()))
      return()
    pred() %>% 
      full_join(., choose.x(), by = "guest") %>%
      rename(Guest = guest, `Binding, kJ/mol` = ensemble, 
             `Applicability` = domain) %>%
      select(., -newSk)
  })
  
  output$downloadPred <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("results", ".xls", sep = "")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      # Write to a file specified by the 'file' argument
      write.xlsx(pred() %>% 
                   full_join(., choose.x(), by = "guest") %>%
                   rename(Guest = guest, `Binding, kJ/mol` = ensemble, 
                          `Applicability` = domain), 
                 file, 
                 col.names = T,
                 row.names = F)
    }
  )
  
  SDFile <- eventReactive(
    input$searchButton, {
      download.cactus.results(input$search)
    }, ignoreNULL = T
  )
  
  output$SDFTable <- DT::renderDataTable({
    SDFile()
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$search, ".SDF", sep = "")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      # Write to a file specified by the 'file' argument
      write.table(SDFile(), file, 
                  quote = F,
                  col.names = F,
                  row.names = F)
    }
  )
  
  # GGVIS
  # guest.pointer <- function(x) {
  #   if(is.null(x))
  #     return(NULL)
  #   if(is.null(x$guest))
  #     return(NULL)
  #   
  #   # pick out the guest
  #   pred.df <- isolate(pred())
  #   the.guest <- pred.df[pred.df$guest == x$guest, ]
  #   
  #   paste0(the.guest$guest, "<br>", 
  #          the.guest$CD, "-CD complex<br>",
  #          the.guest$ensemble, " kJ/mol<br>",
  #          the.guest$domain, " the domain<br>")
  # }
#   vis <- reactive({
#     # if(is.null(pred()))
#     #   data.frame(x = 0, y = 0) %>% ggvis(~x, ~y)
#     # Naming the x-axis
#     if(input$xaxis == "ad")
#       xaxis <- "newSk"
#     else
#       xaxis <- input$xaxis
# 
#     # Assigning the variables - requires some finessing because the inputs
#     # are characters (see: the movie explorer example in the Shiny gallery)
#     xvar <- prop("x", as.symbol(xaxis))
#     yvar <- prop("y", as.symbol("ensemble"))
#     
#     pred() %>%
#       ggvis(x = xvar, y = yvar) %>%
#       layer_points(size := 25, size.hover:= 100, 
#                    fillOpacity := 0.5, fillOpacity.hover := 1, 
#                    stroke = ~CD, key := ~guest) %>%
#       add_tooltip(guest.pointer, "hover") %>%
#       add_axis("x", title = xvar) %>%
#       add_axis("y", title = yvar) %>%
#       add_legend("stroke", title = "Cyclodextrin", values = c("Alpha", "Beta")) %>%
#       scale_nominal("stroke", domain = c("Alpha", "Beta"),
#                     range = c("red", "blue")) %>%
#       set_options(width = 500, height = 500)
#     
#   })
# if(!is.null(observe({pred()})))
#   vis %>% bind_shiny("plot2")
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)


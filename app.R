# install.packages("ggvis")
library(shiny)

source("./functions.R")
p_load(data.table, shiny, shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("sandstone"),
   # Application title
   # titlePanel("Cyclodextrin Affinity Prediction"),
   
   navbarPage(
     theme = "yeti", 
     "Cyclodextrin Affinity Predictions", 
     tabPanel(
       "Instructions",
       column(
         6, offset = 3, 
         h3("Overview"),
         p("You can use this web app to download SDFs of various molecules then 
         run QSARS to predict their affinity to cyclodextrin. The code for the 
         development of the models can be found ", 
         a(href = "https://www.github.com/awqx/wip-cactus", "on GitHub. "),
         "The QSARs are calculated off of molecular descriptors from 
         PaDEL-Descriptor, which can be downloaded ", 
         a(href = "https://www.yapcwsoft.com/dd/padeldescriptor/", 
           "here, from Yap Chun Wei.")),
         br(), 
         "Using the app should work like this:",
         tags$ol(
           tags$li("Download: Obtain structure file(s) for molecules"), 
           tags$li("Calculate Descriptors: Run the structure file(s) through 
                   PaDEL-Descriptor"),
           tags$li("Upload: Calculate affinity from the descriptors with QSARs")
         ),
         br(),
         p("The Explore tab can be used to find molecules that fit specific 
         parameters (e.g., analgesic, anti-cancer, antibiotic, FDA-approved). 
         It is currently under construction."), 
         h3("Downloading Structure Files"), 
         p("The search box on the download page goes through ", 
           a(href = "https://cactus.nci.nih.gov/chemical/structure", 
             "the Chemical Identifier Resolver "), 
           "provided by the CADD group of NCI to find SDFs (structure data 
           files). You can download molecules one-by-one or a few at a time. 
           For the latter, separate the names with commas. For example, type 
           \"acetaminophen, ibuprofen, histamine\"." ), 
         p("After hitting \"Search\", a table will appear on the right indicating
           if the molecule's SDF could be located. If a molecule is not found, first 
           check for typos. Alternatively, chemical synonyms may work (e.g., 
           acetylsalicyclic acid instead of aspiring). Uncommon molecules or 
           newly developed pharmaceuticals may be absent from the database, and 
           will have to be hand-drawn or found elsewhere."),
         h3("Using PaDEL-Descriptor"), 
         p("Make sure that ", 
           a(href = "https://www.yapcwsoft.com/dd/padeldescriptor/",
             "PaDEL-Descriptor "), "and ",
           a(href = "https://www.java.com/en/download/help/index_installing.xml",
             "Java "), "are installed. Open the executable .jar file and check 
           off the boxes to match the interface below.", 
           br(),
           img(src = "interface.PNG", width = "600px"),
           br(),
           "These are the changes that need to be made:", 
           tags$ul(
             tags$li("Check off \"3D\""), 
             tags$li("Check off \"Retain 3D coordinates\""),
             tags$li("Change \"Convert to 3D\" to \"Yes (use MM2 forcefield)\""),
             tags$li("Under the tab \"Fingerprints\", 
                     check off \"SubstructureFingerprinterCount\" (not shown in 
                     image above)")
           ),
           "These are optional changes:", 
           tags$ul(
             tags$li("Set \"Max. running timer per molecule\" to 200,000 (this may 
                     prevent PaDEL from getting stuck on certain molecules)"), 
             tags$li("Check off \"Use filename as molecule name\" (this may be 
                     convenient if every molecule is saved individually)")
           ),
           p("Set \"Molecules directory/file\" to the desired SDF. PaDEL-Descriptor 
             can handle multiple molecules in one file, which using \"Download\"
             in the app will produce. Also, with \"Descriptor output file\", save 
             the file as a .csv.")
           ),
         h3("Obtaining Predictions"), 
         p("Upload the .csv from PaDEL-Descriptor and select the cyclodextrin type 
           that you want to model. As of now, only alpha- and beta-cyclodextrin 
           are available. The app will then generate a table and a plot. The table
           contains information about the applicability (if the molecule is inside 
           the applicability domain, it's similar enough to the molecules the model 
           was trained on to be predicted reliably), the binding affinity, and the
           cyclodextrin being modeled."),
         p("The last column will display the x-axis variable chosen by the user. 
           This variable will be used to generate the graph on the next tab. 
           If you select \"Applicability\", the x-variable on the plot will be 
           \"newSk\", which is a metric used to determine similarity of data. 
           A newSk < 3 would be considered inside the applicability domain."),
         p("The table can be downloaded with \"Download Results\" and will be 
           saved as a .csv."),
         h3("Exploring Candidates"), 
         p(em("Coming soon!"))
       )
     ),
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
                  h6("Add multiple chemical names by separating names with a comma"),
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
      download.cactus.multiple(input$search)
    }, ignoreNULL = T
  )
  
  output$SDFTable <- DT::renderDataTable({
    if(is.null(SDFile()))
      return(NULL)
    check.found <- isolate(check.sdf(SDFile(), input$search)) 
    # row names are molecule names due to check.sdf
    results <- data.frame(ifelse(check.found, "Found", "Not found"))
    colnames(results) <- "Search Status"
    # Aesthetic decision to change molecule names to title case
    rownames(results) <- toTitleCase(rownames(results))
    return(results)
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      if(str_detect(input$search, ","))
        return("molecules.SDF")
      else
        return(paste(input$search, ".SDF", sep = ""))
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


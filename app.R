# Cyclodextrin Affinity Prediction

source("./functions.R")

# UI =====
# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("sandstone"),
   # Style ====
   #' tags$head(
   #'   tags$style(
   #'     HTML("
   #'      @import url('https://fonts.googleapis.com/css?family=Open+Sans');
   #'      @import url('https://fonts.googleapis.com/css?family=Nanum+Gothic+Coding');
   #' 
   #'      p {
   #'        font-family: 'Open Sans';    
   #'      }
   #'      
   #'      h3 {
   #'        font-family: 'Nanum Gothic Coding';
   #'      }
   #'  "))
   #' ),
   # End style ====
   # Application title
   # titlePanel("Cyclodextrin Affinity Prediction"),
   
   navbarPage(
     theme = "yeti", 
     "Cyclodextrin Affinity Prediction", 
     # Instructions -----
     tabPanel(
       "Instructions",
       column(
         6, offset = 3, 
         # Overview ----
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
         # Download ----
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
         p("Additionally, successfully obtained SDFs will be rendered in the 
           next tab, \"Molecules\". Currently, these molecules are created 
           using ChemmineR. Though comprehensible, I will work on my own 
           code to clean up the graphing and make it more convenient for large 
           numbers of molecules."),
         # PaDEL ----
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
         # Upload ----
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
         # Explore ----
         h3("Exploring Candidates"), 
         p(em("Coming soon!"))
       )
     ),
     # Download from CIR ----
     tabPanel("Download", 
              fluidRow(
                
                # A sidebar containing the searchbar for Cactus as well as an 
                # option to download the generated .SDF
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
                # The main panel. Contains the results of the CIR search and
                # a panel containining plots of the molecular structures.
                column(
                  6, 
                  tabsetPanel(
                    tabPanel(
                      "Results", 
                      DT::dataTableOutput("SDFTable")
                    ),
                    tabPanel(
                      "Molecules", 
                      plotOutput("molecules")
                    )
                  ),
                  uiOutput("filePreview")
                )
              )), 
     # Upload and run QSAR ----
     tabPanel(
       "Upload", 
       fluidRow(
         # A sidebar that allows for the uploading of a file of descriptors
         # and customization of the plot
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
         
         # Main panel: contains the table of results and the plot of affinity
         column(
           6,
           tabsetPanel(
             tabPanel("Table", DT::dataTableOutput("predTable")),
             tabPanel("Plot", plotOutput("predPlot")) 
           )
         )
      )),
     
     # Explore predictions ----
     tabPanel("Explore", 
              fluidRow(
                column(
                  2, offset = 2, 
                  h6("Search for specific molecules in the data table or search by category"),
                  checkboxGroupInput("exploreCD", h5("Cyclodextrin Type"), 
                                     choices = list("Alpha" = "a", "Beta" = "b")),
                  br(), 
                  sliderInput("explorePredRange", h5("Range of predictions, kJ/mol"), 
                              min = -50, max = 50, value = c(-30, 10)),
                  br(), 
                  sliderInput("exploreAD", h5("Applicability Domain, newSk"), 
                              min = 0, max = 6, value = c(0, 3))
                ), 
                # Main panel with the predictions
                column(
                  6,
                  tabsetPanel(
                    tabPanel("Table", DT::dataTableOutput("exploreTable")),
                    tabPanel("Plot", 
                             div(
                               style = "position:relative", 
                               plotOutput("explorePlot",  
                                          hover = hoverOpts(id = "exploreHover")), 
                               uiOutput("hoverInfo")) 
                             )
                             
                             #          , 
                             # verbatimTextOutput("hoverInfo")) 
                  )
                )
              ))
   )
)

# Server =====

server <- function(input, output) {
  
  # QSAR ----
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
           color = "Cyclodextrin", shape = "Applicability") + 
      theme(text = element_text(size = 14))
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
             color = "Cyclodextrin", shape = "Applicability") + 
      theme(text = element_text(size = 14))
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
      # Because certain molecules contain commas in their name, 
      # this will be written as a .xlsx
      write.xlsx(pred() %>% 
                   full_join(., choose.x(), by = "guest") %>%
                   rename(Guest = guest, `Binding, kJ/mol` = ensemble, 
                          `Applicability` = domain), 
                 file, 
                 col.names = T,
                 row.names = F)
    }
  )
  
  # SDF/CIR ----
  
  # An SDF in the form of a data.frame
  # Unfortunately, I cannot handle molecules with commas in their name 
  # because of how I'm splitting the search bar.
  SDFile <- eventReactive(
    input$searchButton, {
      download.cactus.multiple(input$search)
    }, ignoreNULL = T
  )
  
  # Showing the results of downloading
  output$SDFTable <- DT::renderDataTable({
    if(is.null(SDFile()))
      return(NULL)
    check.found <- isolate(check.sdf(SDFile(), input$search)) 
    # Finding names of guests
    guest.names <- 
      strsplit(input$search, ",") %>% 
      lapply(str_trim) %>% unlist() %>% 
      toTitleCase()
    # Creating a data frame
    results <- data.frame(guest.names, 
                          ifelse(check.found, "Found", "Not found"), 
                          row.names = NULL)
    colnames(results) <- c("Molecule", "Search Status")
    return(results)
  })
  
  # Enabling the user to download the SDF
  # Running from the RStudio browser misnames the file, but running in the 
  # default browser works
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
  
  # Plotting molecules from the SDF using ChemmineR
  output$molecules <- renderPlot({
    if(is.null(SDFile()))
      return()
    guest.sdf <- isolate(
      SDFile() %>%
        unlist() %>% as.character() %>% 
        read.SDFstr() %>% read.SDFset()
      ) 
    guest.name <- isolate({
      all.guests <- strsplit(input$search, ",") %>% 
        lapply(str_trim) %>% unlist() %>% toTitleCase()
      check.guests <- isolate(check.sdf(SDFile(), input$search)) 
      all.guests[check.guests]
    }
      
    )
    plot(guest.sdf,
         # griddim = c(2, 2), # I would like for plots to be separated
         print_cid = guest.name,
         print = F)
  }
  )

  # Explore ----
  
  explore.sort <- reactive({
    
    explore.sort <- fda.explore %>%
      filter(ensemble > input$explorePredRange[1] & ensemble < input$explorePredRange[2]) %>%
      filter(newSk > input$exploreAD[1] & newSk < input$exploreAD[2]) 
    
    if(!("a" %in% input$exploreCD))
      explore.sort <- explore.sort %>% filter(cd != "Alpha")
    if(!("b" %in% input$exploreCD))
      explore.sort <- explore.sort %>% filter(cd != "Beta")
    
    return(explore.sort)
    
  })
  
  output$exploreTable <- DT::renderDataTable({
    if(is.null(explore.sort()))
      return(NULL)
    explore.sort() %>% 
      mutate(ensemble = round(ensemble, 1), 
             newSk = round(newSk, 2)) %>%
      mutate(guest = toTitleCase(guest)) %>%
      rename(Guest = guest, `Binding, kJ/mol` = ensemble, 
             `Applicability` = domain, `Cyclodextrin Type` = cd) 
  })
  
  output$explorePlot <- renderPlot({
    if(is.null(explore.sort()))
      return(NULL)
    fda.data <- as.data.frame(explore.sort())
    fda.data$cd <- as.factor(fda.data$cd)
    levels(fda.data$cd) <- c("Alpha-CD", "Beta-CD")
    ggplot(fda.data, aes(x = newSk, y = ensemble, color = cd)) + 
      geom_point() + 
      theme_bw() + 
      labs(y = "Predicted dG, kJ/mol", 
           x = "Applicability, newSk", 
           color = "Host") + 
      theme(text = element_text(size = 14))
    
  })
  # Credit to Pawel, https://gitlab.com/snippets/16220
  output$hoverInfo <- renderUI({
    
    hover <- input$exploreHover
    point <- nearPoints(explore.sort(), hover, threshold = 5, 
                        maxpoints = 1, addDist = T)
    if(nrow(point) == 0) # no plot exists
      return(NULL)
    
    # Calculate point position inside the image as percent of dimensions
    left.pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top.pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # Calculate distance in pixels
    left.px <- hover$range$left + left.pct * (hover$range$right - hover$range$left)
    top.px <- hover$range$top + top.pct * (hover$range$bottom - hover$range$top)
    
    # Style the tooltip
    style <- paste0("position:absolute; z-index:100; ", 
                    "background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left.px + 2, "px; top:", top.px + 2, "px;")
    
    # Create tooltip as wellPanel
    wellPanel(
      style = style, 
      p(HTML(paste0(
        "<b>Guest: </b>", toTitleCase(point$guest), "<br/>",
        "<b>Affinity: </b>", round(point$ensemble, 1), "kJ/mol <br/>", 
        "<b>Host: </b>", point$cd, "<br/>", 
        "<b>Applicability: </b>", ifelse(point$newSk < 3, "Inside", "Outside")
        )))
    )
    
  })
}


# Run =====
# Run the application 
shinyApp(ui = ui, server = server)


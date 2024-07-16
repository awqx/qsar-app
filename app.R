# Cyclodextrin Affinity Prediction

source("./functions.R")

# UI =====
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(includeCSS("www/CSS.css")),
  
  navbarPage(
    theme = "yeti",
    "Cyclodextrin Affinity Prediction",
    
    # Instructions -----

    tabPanel(
      "Instructions",
      column(
        6,
        offset = 3,

        # Overview ----
        
        h3("Overview"),
        p(
          "You can use this web app to download SDFs of various molecules then
         run QSARS to predict their affinity to cyclodextrin. The code for the
         development of the models can be found ",
          a(href = "https://www.github.com/awqx/wip-cactus", "on GitHub. "),
          "The QSARs are calculated off of molecular descriptors from
         PaDEL-Descriptor, which can be downloaded ",
          a(href = "https://www.yapcwsoft.com/dd/padeldescriptor/",
            "here, from Yap Chun Wei.")
        ),
        br(),
        "Using the app should work like this:",
        tags$ol(
          tags$li("Download: Obtain structure file(s) for molecules"),
          tags$li(
            "Calculate Descriptors: Run the structure file(s) through
                   PaDEL-Descriptor"
          ),
          tags$li("Upload: Calculate affinity from the descriptors with QSARs")
        ),
        br(),
        p(
          "The Explore tab can be used to find molecules that fit specific
         parameters (e.g., analgesic, anti-cancer, antibiotic, FDA-approved).
         It is currently under construction."
        ),
        # Download ----
        h3("Downloading Structure Files"),
        p(
          "The search box on the download page goes through ",
          a(href = "https://cactus.nci.nih.gov/chemical/structure",
            "the Chemical Identifier Resolver "),
          "provided by the CADD group of NCI to find SDFs (structure data
           files). You can download molecules one-by-one or a few at a time.
           For the latter, separate the names with commas. For example, type
           \"acetaminophen, ibuprofen, histamine\"."
        ),
        p(
          "After hitting \"Search\", a table will appear on the right indicating
           if the molecule's SDF could be located. If a molecule is not found, first
           check for typos. Alternatively, chemical synonyms may work (e.g.,
           acetylsalicyclic acid instead of aspiring). Uncommon molecules or
           newly developed pharmaceuticals may be absent from the database, and
           will have to be hand-drawn or found elsewhere."
        ),
        p(
          "Additionally, successfully obtained SDFs will be rendered in the
           next tab, \"Molecules\". Currently, these molecules are created
           using ChemmineR. Though comprehensible, I will work on my own
           code to clean up the graphing and make it more convenient for large
           numbers of molecules."
        ),
        
        # PaDEL ----
        
        h3("Using PaDEL-Descriptor"),
        p(
          "Make sure that ",
          a(href = "https://www.yapcwsoft.com/dd/padeldescriptor/",
            "PaDEL-Descriptor "),
          "and ",
          a(href = "https://www.java.com/en/download/help/index_installing.xml",
            "Java "),
          "are installed. Open the executable .jar file and check
           off the boxes to match the interface below.",
          br(),
          img(src = "interface.PNG", width = "600px"),
          br(),
          "These are the changes that need to be made:",
          tags$ul(
            tags$li("Check off \"3D\""),
            tags$li("Check off \"Retain 3D coordinates\""),
            tags$li("Change \"Convert to 3D\" to \"Yes (use MM2 forcefield)\""),
            tags$li(
              "Under the tab \"Fingerprints\",
                     check off \"SubstructureFingerprinterCount\" (not shown in
                     image above)"
            )
          ),
          "These are optional changes:",
          tags$ul(
            tags$li(
              "Set \"Max. running timer per molecule\" to 200,000 (this may
                     prevent PaDEL from getting stuck on certain molecules)"
            ),
            tags$li(
              "Check off \"Use filename as molecule name\" (this may be
                     convenient if every molecule is saved individually)"
            )
          ),
          p(
            "Set \"Molecules directory/file\" to the desired SDF. PaDEL-Descriptor
             can handle multiple molecules in one file, which using \"Download\"
             in the app will produce. Also, with \"Descriptor output file\", save
             the file as a .csv."
          )
        ),
        
        # Upload ----
        
        h3("Obtaining Predictions"),
        p(
          "Upload the .csv from PaDEL-Descriptor and select the cyclodextrin type
           that you want to model. As of now, only alpha- and beta-cyclodextrin
           are available. The app will then generate a table and a plot. The table
           contains information about the applicability (if the molecule is inside
           the applicability domain, it's similar enough to the molecules the model
           was trained on to be predicted reliably), the binding affinity, and the
           cyclodextrin being modeled."
        ),
        p(
          "The last column will display the x-axis variable chosen by the user.
           This variable will be used to generate the graph on the next tab.
           If you select \"Applicability\", the x-variable on the plot will be
           \"newSk\", which is a metric used to determine similarity of data.
           A newSk < 3 would be considered inside the applicability domain."
        ),
        p(
          "The table can be downloaded with \"Download Results\" and will be
           saved as a .csv."
        ),
        
        # Explore ----
        
        h3("Exploring Candidates"),
        p(
          "This tab contains the predicted affinities of around 1000 FDA approved
           molecules, sourced from the Orange Book (39th Edition). The table to the
           right contains all the available values. Molecules can be searched by
           name with the search feature of the data table, or by selecting specific
           points on the graph in the next page. To narrow down candidates, there are
           options to sort the group by cyclodextrin type, predicted affinity, and
           applicability."
        )
      )
    ),
    
    # Download from CIR ----
    
    tabPanel(
      "Download",
      fluidRow(
       # A sidebar containing the searchbar for Cactus as well as an
       # option to download the generated .SDF
        column(
         2,
         offset = 1,
         textInput("search",
                   label = h4("Chemical Identifier Resolver")),
         h6("Add multiple chemical names by separating names with a comma"),
         actionButton("searchButton", "Search"),
         br(),
         br(),
         downloadButton('downloadData', 'Download')
         
        ),
        # The main panel. Contains the results of the CIR search and
        # a panel containining plots of the molecular structures.
        column(
         8,
        tabsetPanel(
          tabPanel(
            "Results",
            DT::dataTableOutput("SDFTable")),
          tabPanel(
            "Molecules",
            # slickROutput("molecules")
            plotOutput("molecules", width = "100%"),
            fluidRow(
              column(1, offset = 4, actionButton("prev", "PREV")),
              column(2, offset = 0.5, textOutput("pageCount")),
              column(1, offset = 0.5, actionButton("next", "NEXT")),
              tags$style(type = 'text/css', "#pageCount { text-align: center; font-size: 16px}")
            )
          )
        ))
      )),
    
    # Upload and run QSAR ----
    tabPanel(
      "Upload",
      fluidRow(
      # A sidebar that allows for the uploading of a file of descriptors
      # and customization of the plot
      column(
        2,
        offset = 1,
        # Start with a file input for .csv of descriptors
        # Sidebar with a slider input for number of bins
        fileInput(
         "padel",
         h4("PaDEL-Descriptor Output"),
         multiple = T,
         accept = c(
           "text/csv",
           "text/comma-separated-values,text/plain",
           ".csv"
        )
       ),
       br(),
       checkboxGroupInput(
         "cd",
         h4("Cyclodextrin Type"),
         choices = list("Alpha-CD" = "a", "Beta-CD" = "b")
       ),
       br(),
       radioButtons(
         "xaxis",
         h4("Variable on x-axis"),
         c(
           "Molecular weight" = "MW",
           "Number of atoms" = "nAtom",
           "Applicability" = "ad"
         )
       ),
       br(),
       sliderInput(
         "yrange",
         h4("Range of predictions, kJ/mol"),
         min = -50,
         max = 50,
         value = c(-30, 10)
       ),
       # "Gamma" = "c" not available yet
       br(),
       uiOutput("xaxisRange"),
       br(),
       downloadButton('downloadPred', 'Download Results')
      ),
      
      # Main panel: contains the table of results and the plot of affinity
      column(8,
            tabsetPanel(
              tabPanel("Table", DT::dataTableOutput("predTable")),
              tabPanel(
                "Plot",
                div(
                  style = "position:relative",
                  plotOutput("predPlot",
                             hover = hoverOpts(id = "predHover")),
                  uiOutput("predInfo")
                )
              )
            ))
      )),
    
    # Explore predictions ----
    
    tabPanel(
      "Explore",
      fluidRow(
        column(
         2,
         offset = 1,
         h6(
           "Search for specific molecules in the data table or search by category"
         ),
         checkboxGroupInput(
           "exploreCD",
           h4("Cyclodextrin Type"),
           choices = list("Alpha-CD" = "a", "Beta-CD" = "b")
         ),
         br(),
         sliderInput(
           "explorePredRange",
           h4("Range of predictions, kJ/mol"),
           min = -30,
           max = 0,
           value = c(-20, -10)
         ),
         br(),
         sliderInput(
           "exploreAD",
           h4("Range of newSk values"), 
           min = 0, max = 6, 
           step = 0.1,
           value = c(0, 3)
         ),
         h6("Changing below does not alter the applicability domain."),
         sliderInput(
           "ADcutoff",
           h4("Applicability Domain newSk cutoff"), 
           min = 0,
           max = 4,
           step = 0.1,
           value = 3,
         )
        ),
        # Main panel with the predictions
        column(
          8,
          tabsetPanel(
            tabPanel("Table", DT::dataTableOutput("exploreTable")),
            tabPanel(
              "Plot",
              div(
                style = "position:relative",
                plotOutput(
                  "explorePlot",
                  hover = hoverOpts(
                    id = "exploreHover",
                    delay = 100, 
                    delayType = 'debounce'
                  ),
                ),
                uiOutput("exploreInfo")
              )
            )
          # , verbatimTextOutput("exploreInfo"))
          )
        )
      )
    ),
    tabPanel("Release",
             fluidRow(
               column(
                 2,
                 offset = 1,
                 h6(
                   "Simulate the release of drug from a solid cylinder of
                     polymer into water"
                 ),
                 sliderInput(
                   "releaseTime",
                   h4("Duration of Release, hours"),
                   min = 100,
                   max = 1000,
                   value = 350
                 ),
                 numericInput("releaseAff", h4("Calculated Affinity, dG in kJ/mol"),
                              value = -10),
                 actionButton("runODE", "Simulate Release")
               ),
               column(7,
                      offset = 0.5,
                      tabsetPanel(
                        tabPanel("Table", DT::dataTableOutput("releaseTable")),
                        tabPanel("Plot", plotOutput("releasePlot"))
                      ))
             ))
  )
)

# Server =====

server <- function(input, output) {
  options(DT.fillContainer = FALSE)
  options(DT.autoHideNavigation = FALSE)
  
  # QSAR ----
  # A slider for adjusting the x-axis depending on the variable
  output$xaxisRange <- renderUI({
    if (is.null(pred()))
      return()
    if (input$xaxis == "ad")
      xrange <- c(0.0, 6.5)
    else
      xrange <- choose.x()[, 2] %>% range() %>% round()
    
    sliderInput("xrange",
                h4("Range of x-axis"),
                min = xrange[1],
                max = xrange[2],
                xrange)
  })
  
  # Creating a dataframe that will be attached to prediction results
  # and will provide the x-axis to the graph
  
  choose.x <- reactive({
    if (is.null(input$cd) || is.null(input$padel))
      return()
    
    if (input$xaxis == "ad")
      pred() %>% select(., guest)
    else
      read.csv(input$padel$datapath, header = T) %>%
      rename(guest = Name) %>%
      select(., guest, !!input$xaxis)
    
  })
  
  # Creating a dataframe of ensemble prediction results
  # that responds to pre-processing
  
  pred <- reactive({
    if (is.null(input$cd) || is.null(input$padel))
      return()
    
    raw <- read.csv(input$padel$datapath, header = T)
    alpha <- data.frame()
    beta <- data.frame()
    
    if ("a" %in% input$cd) {
      pp.a <- pp.desc(raw, alpha.vars, alpha.pp, alpha.fill) %>%
        dplyr::rename(guest = guests)
      ad <- domain.num(pp.a) %>% select(., guest, newSk, domain)
      qsar <- ensemble(pp.a, alpha.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      alpha <-
        full_join(ad, qsar, by = "guest") %>% mutate(CD = "Alpha-CD")  %>%
        mutate(guest = as.character(guest),
               ensemble = signif(ensemble, digits = 3))
    }
    
    if ("b" %in% input$cd) {
      pp.b <- pp.desc(raw, beta.vars, beta.pp, beta.fill) %>%
        dplyr::rename(guest = guests)
      ad <- domain.num(pp.b) %>% select(., guest, newSk, domain)
      qsar <- ensemble(pp.b, beta.models) %>%
        select(., guest, ensemble)
      # Appending cyclodextrin type and results of app domain to results
      beta <-
        full_join(ad, qsar, by = "guest") %>% mutate(CD = "Beta-CD")  %>%
        mutate(guest = as.character(guest),
               ensemble = signif(ensemble, digits = 3))
    }
    
    rbind(alpha, beta)
  })
  
  output$predPlot <- renderPlot({
    if (is.null(input$cd) || is.null(input$padel))
      return()
    # pred()$domain <- as.factor(pred()$domain)
    # levels(pred()$domain) <- c("Inside", "Outside")
    if (input$xaxis == "ad")
      ggplot(pred(), aes(
        y = ensemble,
        x = newSk,
        color = CD,
        shape = domain
      )) +
      geom_point() +
      theme_bw() +
      lims(x = input$xrange, y = input$yrange) +
      scale_shape_manual(values = c(16, 4)) +
      scale_color_manual(values = c("#A0C3AA", "#40798C")) +
      labs(
        x = "newSk",
        y = "Binding, kJ/mol",
        color = "Host",
        shape = "Applicability"
      ) +
      theme(text = element_text(size = 16, family = "D-DIN"))
    else
      ggplot(
        full_join(pred(), choose.x(), by = "guest"),
        aes_string(
          y = "ensemble",
          x = input$xaxis,
          color = "CD",
          shape = "domain"
        )
      ) +
      geom_point() +
      theme_bw() +
      lims(x = input$xrange, y = input$yrange) +
      scale_shape_manual(values = c(16, 4)) +
      labs(
        x = input$xaxis,
        y = "Binding, kJ/mol",
        color = "Host",
        shape = "Applicability"
      ) +
      theme(text = element_text(size = 16, family = "D-DIN")) +
      scale_color_manual(values = c("#A0C3AA", "#40798C"))
    
  })
  
  # A table that displays guest name, predicted affinity, applicability domain,
  # and the chosen x variable
  output$predTable <- DT::renderDataTable({
    if (is.null(pred()))
      return()
    
    ptable <- full_join(pred(), choose.x(), by = "guest")
    ptable <- ptable %>%
      dplyr::filter(ensemble >= input$yrange[1] &
                      ensemble <= input$yrange[2])
    
    if (input$xaxis == "ad") {
      ptable %>%
        filter(newSk >= input$xrange[1] &
                 newSk <= input$xrange[2]) %>%
        mutate(newSk = round(newSk, 2)) %>%
        rename(
          Guest = guest,
          `Binding, kJ/mol` = ensemble,
          `Applicability` = domain
        )
    } else {
      ptable[, 6] <- round(ptable[, 6])
      ptable[ptable[, 6] >= input$xrange[1] &
               ptable[, 6] <= input$xrange[2],] %>%
        mutate(newSk = round(newSk, 2)) %>%
        rename(
          Guest = guest,
          `Binding, kJ/mol` = ensemble,
          `Applicability` = domain
        )
    }
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
      write.xlsx(
        pred() %>%
          full_join(., choose.x(), by = "guest") %>%
          rename(
            Guest = guest,
            `Binding, kJ/mol` = ensemble,
            `Applicability` = domain
          ),
        file,
        col.names = T,
        row.names = F
      )
    }
  )
  
  # Credit to Pawel, https://gitlab.com/snippets/16220
  output$predInfo <- renderUI({
    ptable <- full_join(pred(), choose.x(), by = "guest")
    hover <- input$predHover
    point <- nearPoints(
      ptable,
      hover,
      threshold = 5,
      maxpoints = 1,
      addDist = F
    )
    if (nrow(point) == 0)
      # no plot exists
      return(NULL)
    
    # Calculate point position inside the image as percent of dimensions
    left.pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top.pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # Calculate distance in pixels
    left.px <-
      hover$range$left + left.pct * (hover$range$right - hover$range$left)
    top.px <-
      hover$range$top + top.pct * (hover$range$bottom - hover$range$top)
    
    # Style the tooltip
    style <- paste0(
      "position:absolute; z-index:100; ",
      "background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left.px - 2,
      "px; ",
      "top:",
      top.px - 2,
      "px;"
    )
    
    # Create tooltip as wellPanel
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<b>Guest: </b>",
                  toTitleCase(point$guest),
                  "<br/>",
                  "<b>Affinity: </b>",
                  round(point$ensemble, 1),
                  "kJ/mol <br/>",
                  "<b>Host: </b>",
                  point$CD,
                  "<br/>",
                  "<b>Applicability: </b>",
                  ifelse(point$newSk < input$ADcutoff, "Inside", "Outside")
                )
              )))
    
  })
  
  # SDF/CIR ----
  
  # Linking the action button to generating a string of guests
  # This fixes the problem where typing into the search box causes the
  # table to update without querying CIR
  
  guest <- eventReactive(input$searchButton,
                         input$search)
  
  # An SDF in the form of a data.frame
  # Unfortunately, I cannot handle molecules with commas in their name
  # because of how I'm splitting the search bar.
  SDFile <- eventReactive(input$searchButton, {
    download.cactus.multiple(guest())
  }, ignoreNULL = T)
  
  # Showing the results of downloading
  output$SDFTable <- DT::renderDataTable({
    if (is.null(SDFile()))
      return(NULL)
    check.found <- check.sdf(SDFile(), guest())
    # Finding names of guests
    guest.names <-
      strsplit(guest(), ",") %>%
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
      if (str_detect(input$search, ","))
        return("molecules.SDF")
      else
        return(paste(input$search, ".SDF", sep = ""))
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(
        SDFile(),
        file,
        quote = F,
        col.names = F,
        row.names = F
      )
    }
  )
  
  # # Credit to Stephane Laurent,
  # https://stackoverflow.com/questions/52071825/shiny-click-for-next-plot
  mol.plots <- reactive({
    if (is.null(SDFile()))
      return(NULL)
    plot.smiles(input$search)
  })
  index <- reactiveVal(1)
  observeEvent(input[["prev"]], {
    index(max(index() - 1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index() + 1, length(mol.plots())))
  })
  output$molecules <- renderPlot({
    mol.plots()[[index()]]
  })
  
  # a UI output recording the pages of molecules
  output$pageCount <- renderText({
    if (is.null(SDFile()))
      print("Page 0/0")
    paste0("Page ", index(), "/", length(mol.plots()))
  })
  
  # Explore ----
  
  explore.sort <- reactive({
    explore.sort <- fda.explore %>%
      filter(ensemble > input$explorePredRange[1] &
               ensemble < input$explorePredRange[2]) %>%
      filter(newSk > input$exploreAD[1] &
               newSk < input$exploreAD[2]) %>%
      mutate(
        domain = ifelse(
          newSk < input$ADcutoff, 
          "Inside", 
          "Outside"
        )
      )
    
    if (!("a" %in% input$exploreCD))
      explore.sort <- explore.sort %>% filter(cd != "Alpha")
    if (!("b" %in% input$exploreCD))
      explore.sort <- explore.sort %>% filter(cd != "Beta")
    
    return(explore.sort)
    
  })
  
  output$exploreTable <- DT::renderDataTable({
    if (is.null(explore.sort()))
      return(NULL)
    explore.sort() %>%
      mutate(ensemble = round(ensemble, 1),
             newSk = round(newSk, 2)) %>%
      mutate(guest = toTitleCase(guest)) %>%
      rename(
        Guest = guest,
        `Binding, kJ/mol` = ensemble,
        `Applicability` = domain,
        `Cyclodextrin Type` = cd
      )
  })
  
  output$explorePlot <- renderPlot({
    if (is.null(explore.sort()))
      return(NULL)
    fda.data <- as.data.frame(explore.sort())
    fda.data$cd <- as.factor(fda.data$cd)
    levels(fda.data$cd) <- c("Alpha-CD", "Beta-CD")
    levels(fda.data$domain) <- c("Inside", "Outside")
    ggplot(fda.data, aes(
      x = newSk,
      y = ensemble,
      color = cd,
      shape = domain
    )) +
      geom_point() +
      theme_bw() +
      labs(
        y = "Predicted dG, kJ/mol",
        x = "Applicability, newSk",
        color = "Host",
        shape = "Domain"
      ) +
      theme(text = element_text(size = 16, family = "D-DIN")) +
      scale_color_manual(values = c("#A0C3AA", "#40798C")) +
      scale_shape_manual(values = c(16, 4)) + 
      lims(
        x = input$exploreAD, 
        y = input$explorePredRange
      ) + 
      theme(legend.position = 'bottom')
  })
  # Credit to Pawel, https://gitlab.com/snippets/16220
  output$exploreInfo <- renderUI({
    hover <- input$exploreHover
    point <- nearPoints(
      explore.sort(),
      hover,
      threshold = 5,
      maxpoints = 1,
      addDist = T
    )
    
    if (nrow(point) == 0)
      # no plot exists
      return(NULL)
    
    # Calculate point position inside the image as percent of dimensions
    # TODO: constrain the hover domain to only the points
    # Current configuration 
    
    left.pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) - 0.05
    top.pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) - 0.05
    
    # Calculate distance in pixels
    left.px <-
      hover$range$left + left.pct * (hover$range$right - hover$range$left)
    top.px <-
      hover$range$top + top.pct * (hover$range$bottom - hover$range$top)
    
    # Style the tooltip
    style <- paste0(
      "position:absolute; z-index:100; ",
      "background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left.px,
      "px; top:",
      top.px,
      "px;"
    )
    
    # Create tooltip as wellPanel
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<b>Guest: </b>",
                  toTitleCase(point$guest),
                  "<br/>",
                  "<b>Affinity: </b>",
                  round(point$ensemble, 1),
                  "kJ/mol <br/>",
                  "<b>Host: </b>",
                  point$cd,
                  "<br/>",
                  "<b>Applicability: </b>",
                  ifelse(point$newSk < 3, "Inside", "Outside")
                )
              )))
    
  })
  # Release ----
  ode.output <- eventReactive(input$runODE, {
    time <- input$releaseTime        # Length of simulation in hrs
    M_l  <- 0.0004      # Initial drug in polymer cylinder in millimoles.
    V_h  <- 0.0785      # Volume of polymer cylinder mL
    
    # k2    <-    36.9         # LIGAND decoupling rate from host in 1/hrs
    # k1    <-   303.7 * 1000 * 36.9 # LIGAND coupling rate to host in 1/mM*hrs
    # K     <-   k1 / k2       # Binding strength between drug and host in mM^-1
    K <- dg.to.ka(input$releaseAff)
    k2 <<- 36.9
    k1 <- k2 * K
    
    N     <<-    50.0         # Number of boxes
    delta <<-    1 / N        # Spacing
    
    C_o <- M_l / V_h       # LIGAND in polymer cylinder in mM
    C_T <- 0.00881 / V_h   # Host concentration in polymer cylinder in mM
    C_c <- 0.00001 * C_T   # Fraction of hosts without bound drug
    
    ## Equilibrium concentrations
    
    ### LIGAND  + COMPLEX (LIGAND:host) <- C_o <- Total LIGAND in polymer cylinder
    ligand_eq  <- (C_o) / (1 + K * C_c) # Free LIGAND at time = 0 in mM
    complex_eq <- C_o - (C_o) / (1 + K * C_c) # COMPLEX at time = 0  in mM
    
    ## Dimensionless parameters
    ligand_init  <-  ligand_eq / C_o  # Dimensionless free LIGAND
    complex_init <- complex_eq / C_o  # Dimensionless COMPLEX
    tau          <-       time * k2   # Dimensionless time
    times <- seq(0, tau, by = 1)
    
    p1 <<- k1 * C_o / k2
    p2 <<- 0.933
    p3 <<- C_T / C_o
    
    # State variables
    
    ### Free LIGAND divided into the total number of layers with the layer in direct
    ### contact to the liquid media set to 0
    
    LIGAND    <- c(rep((ligand_init / (N - 1)), times = N - 1), 0)
    
    ### COMPLEX divided into the total number of layers
    
    COMPLEX   <- rep((complex_init / N), times = N)
    
    ### Initially there's no drug in the liquid environment
    
    RELEASE   <- 0
    state  <- c(LIGAND = LIGAND,
                COMPLEX = COMPLEX,
                RELEASE = RELEASE)
    ncall <<- 0
    
    ode(y = state,
        times = times,
        func = affinity)
    
  })
  
  release.tidy <- reactive({
    if (is.null(ode.output()))
      return()
    tidy_release_polymer  <- as.data.frame(ode.output()) %>%
      select(-RELEASE) %>%
      gather(., key = "species", value = "concentration",-time) %>%
      separate(col = species,
               into = c("species", "z"),
               sep = "(?<=[A-Za-z])(?=[0-9])") %>% # Separating the state
      # variable names from the
      # vertical coordinates of
      # the polymer cylinder
      mutate(z = as.numeric(z)) %>%
      mutate(time = time / k2) %>% # Reverting back time from dimensionless to hrs
      filter(species %in% c("LIGAND", "COMPLEX")) %>% #
      group_by(time) %>%
      summarise(ligand_in_polymer = sum(concentration))
    
    # Grabbing the RELEASE variable. I expect it to go up to 1.
    
    tidy_release_media <- ode.output() %>%
      as.data.frame() %>%
      select(time, RELEASE) %>%
      mutate(time = time / k2) %>%
      rename(ligand_in_media = RELEASE)
    
    # When I sum both the drug remaining in the polymer cylinder and the drug
    # entering the solution I expect a flat line around 1 (+/- numerical error) due
    # to the principle of mass conservation. However, I obtain a depletion curve.
    
    tidy_release_df <- tidy_release_polymer %>%
      inner_join(tidy_release_media, by = "time")  %>%
      mutate(mass_conservation = ligand_in_polymer + ligand_in_media)
    return(tidy_release_df)
    
  })
  
  # a table where only times in 10 hour increments are shown
  release.info <- reactive({
    if (is.null(ode.output()))
      return()
    # # Using 10% polymer, 20% polymer, ... released
    # # Benchmarks for amount of polymer released
    # release.amt <- seq(0, 1, by = 0.10)
    # reached <- lapply(release.amt,
    #                      function(x)
    #                        which.min(abs(
    #                          release.tidy()$ligand_in_media - x))) %>%
    #   unlist()
    # release.tidy()[reached, ]
    
    # Simply dividing into 10 hour increments
    timepoints <- seq(0, input$releaseTime, by = 10)
    times.index <- lapply(timepoints, function(x)
      which.min(abs(release.tidy()$time - x))) %>%
      unlist()
    release.tidy()[times.index,] %>%
      select(time, ligand_in_media) %>%
      mutate(ligand_in_media = round(ligand_in_media, 3)) %>%
      rename(`Time, in hrs` = time,
             `Proportion of Cumulative Release` = ligand_in_media)
    
  })
  
  output$releaseTable <- DT::renderDataTable(# head(ode.output())
    release.info())
  
  output$releasePlot <- renderPlot(release.tidy() %>%
                                     ggplot(aes(time, ligand_in_media)) +
                                     geom_point() +
                                     theme_bw())
  
}

# Run =====
# Run the application
shinyApp(ui = ui, server = server)

# ode.output <- eventReactive(input$runODE, {
#   t <- input$releaseTime
#   ka <- dg.to.ka(input$releaseAff)
#
#   k1 <- 36.9*ka
#   k2 <<- 36.9 # experimentally determined by Fu
#   k1 <- 12.6 * 1000 * 26.7
#   k2 <<- 26.7
#
#   drug <- 0.0004 # initial drug amount in mM
#   vol <- 0.0785 # volume of polymer (in L?)
#
#   nstep <- 40.0 # number of steps in ODE
#   delta <- 1/nstep # spacing between steps
#
#   c.drug <- drug/vol # [drug] in polymer cylinder, mM/L
#   c.cd <- 0.00881/vol # [cd] total in mM/L
#   c.empty <- 0.00001*c.cd # [unbound cd] in mM/L
#
#   # Equilibrium conditions
#   c.drug.eq <- c.drug/(1 + k1/k2 * c.empty) # [drug] @ eq
#   c.comp.eq <- c.drug - c.drug.eq/(1 + k1/k2*c.empty) # [complex] @ eq
#
#   # Dimensionless parameters
#   # (no "c." prefix because the units cancel)
#   drug.init <- c.drug.eq/c.drug # [drug] initially
#   comp.init <- c.comp.eq/c.drug # [complex] initially
#   tau <- t*k2
#
#   ligand <- c(rep(drug.init/(nstep - 1), times = nstep - 1), 0)
#   complex <- rep(comp.init/nstep, times = nstep)
#   media <- 0 # no drug in media
#
#   # initializing the state, parameters, and tau
#   state <- c(ligand = ligand, complex = complex, media = media)
#   p1 <- ka*c.drug
#   p2 <- 0.933
#   p3 <- c.cd/c.drug
#
#   t.tau <- seq(0, tau, by = 1)
#   ncall <- 0
#   ode(y = state, times = t.tau, func = affinity)
# }
# )

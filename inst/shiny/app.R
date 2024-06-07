library(shiny)
library(shinydashboard)
library(terra)
library(sf)
library(randomForest)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Image Classification"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Processing", tabName = "processing", icon = icon("play")),
      menuItem("Postprocessing", tabName = "postprocessing", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Preprocessing tab
      tabItem(tabName = "preprocessing",
              fluidRow(
                box(
                  title = "File Inputs", status = "primary", solidHeader = TRUE,
                  fileInput("shpFile", "Choose Shapefile or Geopackage", accept = c(".shp", ".gpkg")),
                  fileInput("tifFile", "Choose .tif File", accept = c(".tif"))
                ),
                box(
                  title = "Channel Selection", status = "primary", solidHeader = TRUE,
                  uiOutput("redChannel"),
                  uiOutput("greenChannel"),
                  uiOutput("blueChannel"),
                  uiOutput("nirChannel")
                ),
                box(
                  title = "Actions", status = "primary", solidHeader = TRUE,
                  actionButton("plotRGB", "Plot RGB", class = "btn-primary")
                ),
                box(
                  title = "Plot", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("rgbPlot")
                )
              )
      ),
      # Processing tab
      tabItem(tabName = "processing",
              fluidRow(
                box(
                  title = "Index Calculation", status = "primary", solidHeader = TRUE,
                  checkboxGroupInput("indices", "Select Indices",
                                     choices = c("NDVI", "EVI", "SAVI", "GCI", "OSAVI", "MSAVI", "TVI",
                                                 "DVI", "RVI", "IPVI", "GNDVI", "VARI", "TGI", "NDRE", "CIgreen",
                                                 "CIrededge", "ARVI", "MTVI", "NDWI", "NDSI", "BSI", "NBRI", "IBI")),
                  actionButton("calculateIndices", "Calculate Indices", class = "btn-primary")
                ),
                box(
                  title = "Indices Output", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("preprocessOutput")
                )
              )
      ),
      # Postprocessing tab
      tabItem(tabName = "postprocessing",
              fluidRow(
                box(
                  title = "Classification", status = "primary", solidHeader = TRUE,
                  actionButton("trainModel", "Train Random Forest Model", class = "btn-primary"),
                  actionButton("validateModel", "Validate Model", class = "btn-primary"),
                  actionButton("calculateAoA", "Calculate Area of Applicability", class = "btn-primary")
                ),
                box(
                  title = "Classification Output", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("classificationOutput"),
                  verbatimTextOutput("postclassificationOutput")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Increase the maximum file upload size to 500 MB
  options(shiny.maxRequestSize = 500 * 1024^2)

  # Reactive value to store the indices stack and random forest model
  reactive_vals <- reactiveValues(indices_stack = NULL, rf_model = NULL)

  observe({
    req(input$tifFile)

    # Load the input tif file
    tif_path <- input$tifFile$datapath
    tif <- rast(tif_path)

    # Get layer names
    layer_names <- names(tif)

    # Update the dropdown menus for channel selection
    output$redChannel <- renderUI({
      selectInput("redChannel", "Red Channel", choices = layer_names, selected = layer_names[1])
    })
    output$greenChannel <- renderUI({
      selectInput("greenChannel", "Green Channel", choices = layer_names, selected = layer_names[2])
    })
    output$blueChannel <- renderUI({
      selectInput("blueChannel", "Blue Channel", choices = layer_names, selected = layer_names[3])
    })
    output$nirChannel <- renderUI({
      selectInput("nirChannel", "NIR Channel", choices = layer_names, selected = layer_names[4])
    })
  })

  observeEvent(input$plotRGB, {
    req(input$tifFile, input$redChannel, input$greenChannel, input$blueChannel)

    # Load the input tif file
    tif_path <- input$tifFile$datapath
    tif <- rast(tif_path)

    # Assign the correct channels
    red <- tif[[input$redChannel]]
    green <- tif[[input$greenChannel]]
    blue <- tif[[input$blueChannel]]

    # Plot RGB image
    output$rgbPlot <- renderPlot({
      rgb_stack <- c(red, green, blue)
      plotRGB(rgb_stack, r = 1, g = 2, b = 3, stretch = "lin")
    })
  })

  observeEvent(input$calculateIndices, {
    req(input$tifFile, input$indices, input$redChannel, input$greenChannel, input$blueChannel, input$nirChannel)

    # Load the input tif file
    tif_path <- input$tifFile$datapath
    tif <- rast(tif_path)

    # Assign the correct channels
    red <- tif[[input$redChannel]]
    green <- tif[[input$greenChannel]]
    blue <- tif[[input$blueChannel]]
    nir <- tif[[input$nirChannel]]

    # Calculate indices
    indices_stack <- calculate_indices(red, green, blue, nir, input$indices)
    reactive_vals$indices_stack <- indices_stack

    output$preprocessOutput <- renderPrint({
      print(indices_stack)
    })
  })

  observeEvent(input$trainModel, {
    req(input$tifFile, input$shpFile, reactive_vals$indices_stack)

    # Load the training sites shapefile or geopackage
    shp_path <- input$shpFile$datapath
    if (grepl(".shp$", shp_path)) {
      training_sites <- st_read(shp_path)
    } else if (grepl(".gpkg$", shp_path)) {
      training_sites <- st_read(shp_path)
    }

    # Extract training data
    training_data <- extract(reactive_vals$indices_stack, training_sites)

    # Train Random Forest model
    rf_model <- randomForest(training_data[,-1], as.factor(training_data[,1]))
    reactive_vals$rf_model <- rf_model

    output$classificationOutput <- renderPrint({
      print(rf_model)
    })
  })

  observeEvent(input$validateModel, {
    req(reactive_vals$rf_model)

    # Perform validation (e.g., using cross-validation or test data)
    # This is a placeholder for actual validation code
    validation_results <- "Validation results here"

    output$postclassificationOutput <- renderPrint({
      print(validation_results)
    })
  })

  observeEvent(input$calculateAoA, {
    req(reactive_vals$rf_model)

    # Calculate Area of Applicability (AoA)
    # This is a placeholder for actual AoA calculation code
    aoa_results <- "Area of Applicability results here"

    output$postclassificationOutput <- renderPrint({
      print(aoa_results)
    })
  })
}

# Function to calculate indices (placeholder, please implement the actual indices)
calculate_indices <- function(red, green, blue, nir, indices) {
  index_stack <- rast()

  if ("NDVI" %in% indices) {
    ndvi <- (nir - red) / (nir + red)
    names(ndvi) <- "NDVI"
    index_stack <- c(index_stack, ndvi)
  }

  if ("EVI" %in% indices) {
    evi <- 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)
    names(evi) <- "EVI"
    index_stack <- c(index_stack, evi)
  }

  # Add more indices calculations as needed

  return(index_stack)
}

# Run the application
shinyApp(ui = ui, server = server)


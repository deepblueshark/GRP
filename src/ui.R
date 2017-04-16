shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      #helpText("Default max. file size is 5MB"),
      tags$hr(),
      helpText("Choose this option if your first row contains labels"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      #checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      
      radioButtons(inputId = 'sep' , label = 'Separator',choices = c(Comma= ',',Semicolon= ';',Tab='\t', Space=' '),selected = ','),
      
      #h5(helpText("Please specify your range below before uploading the data file")),
      sliderInput("eps", "eps Range:", min = 1, max = 100, value = c(1,50)),
      sliderInput("pts", "minPts Range:", min = 1, max = 100, value = c(1,50)),
      
      helpText("Cluster label legend: 0: Noise points"),
      helpText("Other numbers : Non-noise points "),
      helpText("Please note that it may take a few seconds until the save window pops up"),
      downloadButton('downloadData', 'Save classification'),
      br(),
      downloadButton('downloadData2', 'Save cluster'),
      br(),
      downloadButton('downloadData3', 'Save top cluster')),
    
    
    
    mainPanel(
      uiOutput("tb")
    )
  )
))
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
      sliderInput("eps", "eps Range:", min = 1, max = 100, value = c(1,3)),
      sliderInput("pts", "minPts Range:", min = 1, max = 100, value = c(1,3)),
      
      helpText("Select the 3D Wood tab, move the sliders until you can see the tree. The slider values would be the optimum point "),
      sliderInput("eps1", "eps Range:", min = 1, max = 100, value = 1),
      sliderInput("pts1", "minPts Range:", min = 1, max = 100, value = 3),
      
      
      #numericInput("eps", "Eps:", min = 0, max = 100, value = 30),
      #br(),
      #numericInput("pts", "MinPts:", min = 0, max = 100, value = 30),
      #br(),
      #actionButton("gButton", "Generate!"),
      
      
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
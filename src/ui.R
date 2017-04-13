shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      
      radioButtons(inputId = 'sep' , label = 'Separator',choices = c(Comma= ',',Semicolon= ';',Tab='\t', Space=' '),selected = ','),
      
      h5(helpText("Please specify your range below before uploading the data file")),
      sliderInput("eps", "eps Range:", min = 1, max = 10, value = c(1,4)),
      sliderInput("pts", "minPts Range:", min = 1, max = 10, value = c(1,4)),
      
      
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
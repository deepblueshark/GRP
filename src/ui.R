library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("File Input"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        "Main",
        
        # fileinput() function is used to get the file upload contorl option
        fileInput("data", 
                  "Dataset CSV File:",
                  accept = c(
                    "text/csv",
                    ".csv",
                    "text/comma-separated-vales,text/plain"
                  )
         ),
                 
         helpText("Default max. file size is 5MB"),
                 
        tabPanel(
          tags$hr(),
          h5(helpText("Select your parameters")),
          #numericInput('n', 'min of slider', 1),
          #numericInput('m', 'Max of slider', 30),
          sliderInput("epsRange", "eps Range:", min = 1, max = 30, value = c(1,30)),
          sliderInput("minPtsRange", "minPts Range:", min = 1, max = 30, value = c(1,30))
        ),
    
        tabPanel(
          h5(helpText("Select the read.table parameters below")),
          checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
          checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
          br(),
          radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
        )
      ),
  
      mainPanel(
        uiOutput("tb"),
        dataTableOutput('fcluster')
        
        # Use below code if you want the tabset programming in the main panel. 
        # If so, then tabset will appear when the app loads for the first time.
        
        #tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
        #tabPanel("Data", tableOutput("table")))
      )
    
    )
  )
)

library(shiny)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  #data <- reactive({
    #file<-read.csv(file.choose(), header=T)
   # file1 <- input$file
    #if(is.null(file1)){return()} 
    #read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    #})
  
  #x <- reactive({
   # file2 <- input$file
    #file22<-as.matrix(file2)
    #if(is.null(file1)){return()} 
    #read.table(file=file22$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  #})
  
  #merged <- reactive({
    #file3 <- input$file
    #if(is.null(file3)){return()} 
   # read.table(file=file3$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
 # })
  
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  #merged<-data
  #x<-as.matrix(data)
  
  #output$fcluster <- renderDataTable({
   # if(is.null(data())){return ()}
    library(dbscan)
    x<-as.matrix(data())
    merged<-data
    q<-1
    
    classified <- data.frame(matrix(NA, nrow=2, ncol=0))
    rownames(classified) <- c("Wood","Non-Wood")
    
    for(i in .1:.2)
    {
      for(j in .1:.2)
      {
        db <- dbscan(x, eps = i , minPts = j)
        
        temp<- as.data.frame(table(db$cluster))
        if(temp[1,1] == 0)
        {
          temp = temp[-1,]
        }
        
        
        wood <- max(temp[2])
        nonwood <- sum(temp[2]) - max(temp[2])
        
        
        classified[1,q] <- wood
        classified[2,q] <- nonwood
        names(classified)[q] <- print(paste0("Eps ",i," MinPts ",j))
        
        merged[q+3] <- db$cluster
        names(merged)[q+3] <- print(paste0("Eps ",i," MinPts ",j))
        q <- q+.1
      }
    }
    output$fcluster <- renderDataTable(merged)
    #output$table1 <- renderDataTable(merged)
    #table
    #dataTableOutput(merged)
    #data()
    #server = function(input, output) {
      #output$table <- renderDataTable(merged)
    #}
   # tableOutput(merged)
  #}
  #)
  
  
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      ""
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
  })
})
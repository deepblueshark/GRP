
if(!require(openxlsx)) {
  install.packages("openxlsx", repos="http://cran.us.r-project.org")
  require(openxlsx)
}
# dbscan for the clustering
if(!require(dbscan)) {
  install.packages("dbscan", repos="http://cran.us.r-project.org")
  require(dbscan)
}
# shiny to build the app
if(!require(shiny)) {
  install.packages("shiny", repos="http://cran.us.r-project.org")
  require(shiny)
}
if(!require(DT)) {
  install.packages("DT", repos="http://cran.us.r-project.org")
  require(DT)
}

shinyServer(function(input,output){
  
  data <- reactive({
    file1 <- input$file
    # Determine document format; 
    ptn <- "\\.[[:alnum:]]{1,5}$" 
    suf <- tolower(regmatches(file1$name, regexpr(ptn, file1$name))) 
    
    #if no argument 
    if(is.null(input$file)){return()}
    # Options for Excel documents; 
    if (suf %in% c('.xls', '.xlsx')) { 
      if( is.null(file1)){return()}
      read.xlsx(file1$datapath, 1)
    }
    else{
      read.table(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    }
  })
  
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  output$sum <- renderTable({
    if(is.null(data())){return()}
    summary(data())
  })
  
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  output$cluster <- renderTable({
    if(is.null(data())){return()}
    
    x <-as.matrix(data())
    merged <<-data()
    q <- 1
    
    classified <<- data.frame(matrix(NA, nrow=2, ncol=0))
    rownames(classified) <- c("Wood","Non-Wood")
    
    for(i in input$eps[1]:input$eps[2])
    {
      
      for(j in input$pts[1]:input$pts[2])
      {
        
        
        db <- dbscan(x, eps = i , minPts = j)
        
        temp<- as.data.frame(table(db$cluster))
        if(temp[1]==0)
          temp = temp[-1,]
        
        wood <- max(temp[2])
        nonwood <- sum(temp[2]) - max(temp[2])
        
        
        classified[1,q] <<- wood
        classified[2,q] <<- nonwood
        names(classified)[q] <<- print(paste0("Eps ",i," MinPts ",j))
        
        merged[q+3] <<- db$cluster
        names(merged)[q+3] <<- print(paste0("Eps ",i," MinPts ",j))
        q <- q+1
        
      }
      
    }
    merged
    
  })
  
  output$class <- renderTable({
    classified
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("classification_result", "xlsx", sep='.') },
    content = function(file){
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook()
      print(class(classified))
      addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1, x = classified)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() { paste("cluster_result", "xlsx", sep='.') },
    content = function(file){
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook()
      print(class(merged))
      addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1, x = merged)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src = 'RStudio-Ball.png', height = 200, width = 200))
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")),tabPanel("Cluster",tableOutput("cluster")),tabPanel("Classification",tableOutput("class")))
  })
})
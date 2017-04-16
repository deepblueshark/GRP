
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

if(!require(readxl)) {
  install.packages("readxl", repos="http://cran.us.r-project.org")
  require(readxl)
} 


shinyServer(function(input,output){
  
  data <- reactive({
    file1 <- input$file
    
    
    # Determine document format; 
    #ptn <- "\\.[[:alnum:]]{1,5}$" 
    #suf <- tolower(regmatches(file1$name, regexpr(ptn, file1$name))) 
    
    #if no argument 
    if(is.null(input$file)){return()}
    file.rename(file1$datapath,paste(file1$datapath,".xlsx", sep=""))
    # Options for Excel documents; 
    # if (suf %in% c('.xls', '.xlsx')) { 
    #  if( is.null(file1)){return()}
    #read.xlsx(file1$datapath, 1)
    #read.xlsx(file1$datapath, sheet = 1, colNames = TRUE)
    read_excel(paste(file1$datapath,".xlsx",sep=""))
    #read.big.matrix(file1$datapath,".xlsx",sep="")
    #}
    # else{
    # read.table(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    # }
  })
  
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  output$sum <- DT::renderDataTable({
    if(is.null(data())){return()}
    datatable(summary(data()))
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(data())){return()}
    datatable(data())
  })
  
  output$cluster <- DT::renderDataTable({
    if(is.null(data())){return()}
    
    x <-as.matrix(data())
    merged <<-data()
    q <- 1
    
    
    
    for(i in input$eps[1]:input$eps[2])
    {
      
      for(j in input$pts[1]:input$pts[2])
      {
        
        
        db <- dbscan(x, eps = i , minPts = j)
        
        
        merged[q+3] <<- db$cluster
        names(merged)[q+3] <<- print(paste0("Eps ",i," MinPts ",j))
        q <- q+1
        
      }
      
    }
    datatable(merged)
    
  })
  
  output$class <- DT::renderDataTable({
    if(is.null(data())){return()}
    
    x <-as.matrix(data())
    q <- 1
    
    classified <<- data.frame(matrix(NA, nrow=3, ncol=0))
    rownames(classified) <<- c("Wood","Non-Wood","Total No. of Clusters")
    
    for(i in input$eps[1]:input$eps[2])
    {
      
      for(j in input$pts[1]:input$pts[2])
      {
        
        
        db <- dbscan(x, eps = i , minPts = j)
        
        temp<- as.data.frame(table(db$cluster))
        
        noise <- 0
        
        if(temp[1,1]==0){
          
          noise <- as.numeric(temp[1,2])
          
          temp = temp[-1,]
        }
        
        
        
        wood <- max(temp[2])
        nonwood <- (sum(temp[2]) - max(temp[2])) + noise 
        
        
        classified[1,q] <<- wood
        classified[2,q] <<- nonwood
        classified[3,q] <<- wood+nonwood
        names(classified)[q] <<- print(paste0("Eps ",i," MinPts ",j))
        q <- q+1
        
      }
      
    }
    datatable(classified)
  })
  
  output$sort <- DT::renderDataTable({
    if(is.null(data())){return()}
    
    x <-as.matrix(data())
    q <- 1
    
    first <- NULL
    second <- NULL
    third <- NULL
    fourth <- NULL
    fifth <- NULL
    sixth <- NULL
    seventh <- NULL
    eighth <- NULL
    ninth <- NULL
    tenth <- NULL
    
    sorted <<- data.frame(matrix(NA, nrow=10, ncol=0))
    rownames(sorted) <<- c("Most freq. cluster","2nd freq.","3rd freq.","4th freq.","5th freq.","6th freq.","7th freq.","8th freq.","9th freq.","10th freq.")
    
    for(i in input$eps[1]:input$eps[2])
    {
      
      for(j in input$pts[1]:input$pts[2])
      {
        
        
        db <- dbscan(x, eps = i , minPts = j)
        
        temp<- as.data.frame(table(db$cluster))
        
        
        temp <- temp[rev(order(temp$Freq)),]
        
        
        first <- temp$Var1[1]
        second <- temp$Var1[2]
        third <- temp$Var1[3]
        fourth <- temp$Var1[4]
        fifth <- temp$Var1[5]
        sixth <- temp$Var1[6]
        seventh <- temp$Var1[7]
        eighth <- temp$Var1[8]
        ninth <- temp$Var1[9]
        tenth <- temp$Var1[10]
        
        sorted[1,q] <<- first
        sorted[2,q] <<- second
        sorted[3,q] <<- third
        sorted[4,q] <<- fourth
        sorted[5,q] <<- fifth
        sorted[6,q] <<- sixth
        sorted[7,q] <<- seventh
        sorted[8,q] <<- eighth
        sorted[9,q] <<- ninth
        sorted[10,q] <<- tenth
        
        names(sorted)[q] <<- print(paste0("Eps ",i," MinPts ",j))
        q <- q+1
        
      }
    }
    
    datatable(sorted)  
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() { paste("classification_result", "xlsx", sep='.') },
    content = function(file){
      if(is.null(data())){return()}
      
      x <-as.matrix(data())
      q <- 1
      
      classified <<- data.frame(matrix(NA, nrow=3, ncol=0))
      rownames(classified) <<- c("Wood","Non-Wood","Total No. of Clusters")
      
      for(i in input$eps[1]:input$eps[2])
      {
        
        for(j in input$pts[1]:input$pts[2])
        {
          
          
          db <- dbscan(x, eps = i , minPts = j)
          
          temp<- as.data.frame(table(db$cluster))
          
          noise <- 0
          
          if(temp[1,1]==0){
            
            noise <- as.numeric(temp[1,2])
            
            temp = temp[-1,]
          }
          
          
          
          wood <- max(temp[2])
          nonwood <- (sum(temp[2]) - max(temp[2])) + noise 
          
          
          classified[1,q] <<- wood
          classified[2,q] <<- nonwood
          classified[3,q] <<- wood+nonwood
          names(classified)[q] <<- print(paste0("Eps ",i," MinPts ",j))
          q <- q+1
          
        }
        
      }
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook()
      print(class(classified))
      addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1,rowNames = TRUE, x = classified)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() { paste("cluster_result", "xlsx", sep='.') },
    content = function(file){
      if(is.null(data())){return()}
      
      x <-as.matrix(data())
      merged <<-data()
      q <- 1
      
      
      
      for(i in input$eps[1]:input$eps[2])
      {
        
        for(j in input$pts[1]:input$pts[2])
        {
          
          
          db <- dbscan(x, eps = i , minPts = j)
          
          
          merged[q+3] <<- db$cluster
          names(merged)[q+3] <<- print(paste0("Eps ",i," MinPts ",j))
          q <- q+1
          
        }
        
      }
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook()
      print(class(merged))
      addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1, x = merged)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { paste("top10_cluster", "xlsx", sep='.') },
    content = function(file){
      if(is.null(data())){return()}
      
      x <-as.matrix(data())
      q <- 1
      
      first <- NULL
      second <- NULL
      third <- NULL
      fourth <- NULL
      fifth <- NULL
      sixth <- NULL
      seventh <- NULL
      eighth <- NULL
      ninth <- NULL
      tenth <- NULL
      
      sorted <<- data.frame(matrix(NA, nrow=10, ncol=0))
      rownames(sorted) <<- c("Most freq. cluster","2nd freq.","3rd freq.","4th freq.","5th freq.","6th freq.","7th freq.","8th freq.","9th freq.","10th freq.")
      
      for(i in input$eps[1]:input$eps[2])
      {
        
        for(j in input$pts[1]:input$pts[2])
        {
          
          
          db <- dbscan(x, eps = i , minPts = j)
          
          temp<- as.data.frame(table(db$cluster))
          
          
          temp <- temp[rev(order(temp$Freq)),]
          
          
          first <- temp$Var1[1]
          second <- temp$Var1[2]
          third <- temp$Var1[3]
          fourth <- temp$Var1[4]
          fifth <- temp$Var1[5]
          sixth <- temp$Var1[6]
          seventh <- temp$Var1[7]
          eighth <- temp$Var1[8]
          ninth <- temp$Var1[9]
          tenth <- temp$Var1[10]
          
          sorted[1,q] <<- first
          sorted[2,q] <<- second
          sorted[3,q] <<- third
          sorted[4,q] <<- fourth
          sorted[5,q] <<- fifth
          sorted[6,q] <<- sixth
          sorted[7,q] <<- seventh
          sorted[8,q] <<- eighth
          sorted[9,q] <<- ninth
          sorted[10,q] <<- tenth
          
          names(sorted)[q] <<- print(paste0("Eps ",i," MinPts ",j))
          q <- q+1
          
        }
      }
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook()
      print(class(sorted))
      addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1,rowNames = TRUE, x = sorted)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$tb <- renderUI({
    if(is.null(data()))
      ""
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", dataTableOutput("table")),tabPanel("Summary", dataTableOutput("sum")),tabPanel("Cluster",dataTableOutput("cluster")),tabPanel("Classification",dataTableOutput("class")),tabPanel("Top 10 Clusters",dataTableOutput("sort")))
  })
})
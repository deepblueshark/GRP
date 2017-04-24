# uploading frameworks
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

if(!require(shinyRGL)) {
  install.packages("shinyRGL", repos="http://cran.us.r-project.org")
  require(shinyRGL)
} 

if(!require(rgl)) {
  install.packages("rgl", repos="http://cran.us.r-project.org")
  require(rgl)
} 

if(!require(rglwidget)) {
  install.packages("rglwidget", repos="http://cran.us.r-project.org")
  require(rglwidget)
} 



shinyServer(function(input,output){
  
  data <- reactive({
    file1 <- input$file
    
    # reads excel files
    if(is.null(input$file)){return()}
    file.rename(file1$datapath,paste(file1$datapath,".xlsx", sep=""))
    read_excel(paste(file1$datapath,".xlsx",sep=""))
  })
  
  #prints dataset information
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  # prints summary of dataset
  output$sum <- DT::renderDataTable({
    if(is.null(data())){return()}
    datatable(summary(data()))
  })
  
  # prints dataset input
  output$table <- DT::renderDataTable({
    if(is.null(data())){return()}
    datatable(data())
  })
  
  # runs clustering algorithm
  output$cluster <- DT::renderDataTable({
    if(is.null(data())){return()}
    
    x <-as.matrix(data())
    merged <<-data()
    q <- 1
    
    
    # goes through range of eps
    for(i in input$eps[1]:input$eps[2])
    {
      # goes through range of minPts
      for(j in input$pts[1]:input$pts[2])
      {
        
        
        db <- dbscan(x, eps = i , minPts = j)
        
        # appends cluster column
        merged[q+3] <<- db$cluster
        names(merged)[q+3] <<- print(paste0("Eps ",i," MinPts ",j))
        q <- q+1
        
      }
      
    }
    datatable(merged)
    
  })
  
  # runs classification algorithm
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
        
        
        # takes largest cluster as wood element
        wood <- max(temp[2])
        # takes other clusters as non-wood elements
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
  
  # sorts clusters and prints top 10 clusters
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
  
  # rendering 3d tree
  output$plot3d <- renderRglwidget({ 
    try(rgl.close())
   
     points3d(merged[,1],
             merged[,2],
             merged[,3], col = 'green')
    axes3d()
    rglwidget()
  })
  
  # rendering wood element of 3d tree
  output$plot3d2 <- renderRglwidget({ 
    try(rgl.close())
  
      e <- as.numeric(input$eps1)
    m <- as.numeric(input$pts1)
    
    
    x <-as.matrix(data())
    db <- dbscan(x, eps = e , minPts = m)
    
    temp<- as.data.frame(table(db$cluster))
    
    
    temp <- temp[rev(order(temp$Freq)),]
    
    
    first <- temp$Var1[1]
    
    
    
    db_filtered = merged[merged$`Eps 1 MinPts 3`== first ,e:m]
    points3d(db_filtered[,1],
             db_filtered[,2],
             db_filtered[,3] )
    axes3d()
    rglwidget()
  })
  
  
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
  
  # prints entire output tabs onto the webpage
  output$tb <- renderUI({
    if(is.null(data()))
      ""
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", dataTableOutput("table")),tabPanel("Summary", dataTableOutput("sum")),tabPanel("Cluster",dataTableOutput("cluster")),tabPanel("Classification",dataTableOutput("class")),tabPanel("Top 10 Clusters",dataTableOutput("sort")),tabPanel("3D Plot",rglwidgetOutput('plot3d')),tabPanel("Wood Only 3D",rglwidgetOutput('plot3d2')))
  })
})
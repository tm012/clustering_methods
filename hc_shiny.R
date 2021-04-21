library(shiny)
library(ggdendro)
library(cluster)
library(MASS)
library("dendextend")
library(shinyjs)
library(rsconnect)
library(factoextra)
library(densityClust)
#https://shiny.rstudio.com/articles/shinyapps.html

  
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File - Distance Matrix",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$h5("Sample Data to get an idea what your data should look like"),
        tags$a("Sample Data",href="https://drive.google.com/file/d/1hvdz6R2Hsz1pfPvoJHtshAtWk9fx7ZNY/view?usp=sharing"),
        tags$h5("Github Page - To understand the options and get an overview of the project, see the project's Github page Readme file"),
        tags$a("Github link",href="https://github.com/tm012/clustering_methods_R"),
        
        
        tags$hr(),
        selectInput("type_metric_1",label="Select a method for Distance Object",
                    choices=c("euclidean","manhattan","maximum","canberra","binary","minkowski")), 
        numericInput("no_coders", "Number of individuals who created the Matrix (optional)", 2, min = 2),
        
        tags$h1("Hierarchical Clustering Variables"),
        checkboxInput("header", "Add rectangle(s) to the clusters", TRUE),
        numericInput("rect_value", "Minimum Rectangles", 2, min = 2),
        selectInput("type_metric",label="Select a metric for 'agnes' and 'diana' clusterings",
                    choices=c("euclidean","manhattan")), 
        tags$hr(),
        tags$h2("hclust"),
        
        selectInput("type",label="Select the method for 'hclust' (1st method)",
                    choices=c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")),
        selectInput("type_1",label="Select another method for 'hclust' comparison (2nd method)",
                    choices=c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")),
        tags$hr(),
        tags$h2("agnes"),
        selectInput("type_agnes_1",label="Select the method for 'agnes' (1st method)",
                    choices=c("average","single","complete","ward","weighted")),
        
        selectInput("type_agnes_2",label="Select another method for 'agnes' comparison (2nd method)",
                    choices=c("average","single","complete","ward","weighted")), 
        tags$hr(),
        tags$h1("Non-Hierarchical Clustering/Finite Clustering and Multi-dimensional Scaling Variables"),
        numericInput("k_value", "K-value (Cluster Number)/You can also use this for MDS", 2, min = 2),
        tags$hr(),
        selectInput("fanny_metric",label="Select metric for Fuzzy clustering (Fanny)",
                   choices=c("euclidean","SqEuclidean","manhattan")),
        numericInput("fanny_memb_exp", "Select memb.exp for Fuzzy clustering (Fanny)", 1.2, min = 1, step = 0.1),
        tags$hr(),
        selectInput("pam_metric",label="Select a metric for Partition around Mediods (PAM)",
                    choices=c("euclidean","manhattan")),
        
        
      ),
      mainPanel(
        #tableOutput("contents")
        tabsetPanel(
          tabPanel( "Agglomerative Hierarchical Clustering",
                    plotOutput("aglo1",width=1000, height=1000),
                    plotOutput("aglo2",width=1000, height=1000),
                    
                   ),
          tabPanel("Divisive Hierarchical Clustering",
                   # fluidRow(...)
                   plotOutput("plot1",width=1000, height=1000),
                   
          ),
          tabPanel("Compare Agglomerative and Divisive Clusters",
                   # fluidRow(...)
                   tags$h1("Comparing hclust and agnes"),
                   plotOutput("compare_1",width=1000, height=1000), #aglo1&aglo2
                   tags$h1("Comparing hclust and diana"),
                   plotOutput("compare_2",width=1000, height=1000), #aglo1&plot1
                   tags$h1("Comparing diana and agnes"),
                   plotOutput("compare_3",width=1000, height=1000), #plot1&aglo2
                   
          ),
          
          tabPanel("Compare 'hclust' Clusters",
                   # fluidRow(...)
                   tags$h1("Comparing hclusts (1st method and 2nd method)"),
                   plotOutput("compare_4",width=1000, height=1000),
                   
          ),
          
          tabPanel("Compare 'agnes' Clusters",
                   
                   # fluidRow(...)
                   tags$h1("Comparing agnes (1st method and 2nd method)"),
                   plotOutput("compare_5",width=1000, height=1000),
                   
          ),
          tabPanel("Non-Hierarchical Clustering",
                   
                   # fluidRow(...)
                   tags$h1("Non-Hierarchical Clustering"),
                   plotOutput("kmeans_out"),
                   
                   plotOutput("pam_out"),
                   plotOutput("pam_out_silhouette"),
                   plotOutput("fanny_out"),
                   plotOutput("fanny_out_silhouette"),
                   
          ),
          tabPanel("Multi-dimensional Scaling",
                   
                   # fluidRow(...)
                   tags$h1("Multi-dimensional Scaling"),
                   plotOutput("classical_MDS",width=1000, height=1000),
                  plotOutput("iso_mds",width=1000, height=1000),
                  plotOutput("kmeans_sammon",width=1000, height=1000),
                   #plotOutput("nonmetric_MDS ",width=1000, height=1000),
                   
          )
          
        )        
        
      )
    )
  )
  
  server <- function(input, output) {
    
    
    observeEvent(input$header, {
      
      if(input$header != TRUE){
        shinyjs::disable("rect_value")
        
      }else{
        shinyjs::enable("rect_value")
        
      }
      
    })
    
    data <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      theme<- read.csv(inFile$datapath, header = TRUE)[,-1]
      #theme_1 <- as.matrix(theme)

      theme <- t(as.matrix(theme))
      theme[is.na(theme)]   <- 0
      theme <- na.omit(theme)
      
      if((sum(diag(theme)) != 0) && !isSymmetric(theme)){
        showNotification(paste("Message:", "Matrix Problem. Check sample."), duration = NULL)
        return(NULL)
      }
        
      d <- dist(theme, method = input$type_metric_1)
      d
    })
    data_1 <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      theme<- read.csv(inFile$datapath, header = TRUE)[,-1]
      theme_1 <- as.matrix(theme)

      theme_1[is.na(theme_1)]   <- 0
      theme_1 <- na.omit(theme_1)
      
      if((sum(diag(theme_1)) != 0) && !isSymmetric(theme_1)){
        showNotification(paste("Message:", "Matrix Problem. Check sample."), duration = NULL)
        return(NULL)
      }      
      theme_1
      
    })    
    data_2 <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      theme<- read.csv(inFile$datapath, header = TRUE)[,-1]
      theme_1 <- as.matrix(theme)
      theme_1[is.na(theme_1)]   <- 0
      theme_1 <- na.omit(theme_1)
      if((sum(diag(theme_1)) != 0) && !isSymmetric(theme_1)){
        showNotification(paste("Message:", "Matrix Problem. Check sample."), duration = NULL)
        return(NULL)
      }
      theme_1
      
      
    })    
    
    output$aglo1 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      if(input$header != TRUE){
        hc1 <- hclust(x, method = input$type ) #agglomeration method to be used (i.e. "complete", "average", "single", "ward.D")
        
        plot(hc1, main = "Dendrogram of hclust") 
        
      }else{
        
        hc1 <- hclust(x, method = input$type ) #agglomeration method to be used (i.e. "complete", "average", "single", "ward.D")
        
        plot(hc1, main = "Dendrogram of hclust") 
        
        rect.hclust(hc1, k = input$rect_value, border = 2:(input$rect_value+1))
      }
      
      
      
      
     
       
      
    })
    output$aglo2 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      if(input$header != TRUE){
        hc3 <- agnes(x, metric = input$type_metric, method = input$type_agnes_1 )
        plot(hc3, main = "Dendrogram of agnes") 
        
      }else{
        
        hc3 <- agnes(x, metric = input$type_metric, method = input$type_agnes_1 )
        plot(hc3, main = "Dendrogram of agnes")
        
        rect.hclust(hc3, k = input$rect_value, border = 2:(input$rect_value+1))
      }
      
      
    })
    
    
    output$plot1 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      if(input$header != TRUE){
        hc4 <- diana(x, metric = input$type_metric)
        
        plot(hc4, main = "Dendrogram of diana")
        
        
        
      }else{
        
        hc4 <- diana(x, metric = input$type_metric)
        
        plot(hc4, main = "Dendrogram of diana")
        
       
        
        rect.hclust(hc4, k = input$rect_value, border = 2:(input$rect_value+1))
      }
      
      
    })
    
    ########## aglo1&aglo2
    
    output$compare_1 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      hc1 <- hclust(x, method = input$type )
      hc2 <- agnes(x, metric = input$type_metric, method = input$type_agnes_1)
      
      # Create two dendrograms
      dend1 <- as.dendrogram (hc1)
      dend2 <- as.dendrogram (hc2)
      
      tanglegram(dend1, dend2)
      

      
      
    })  
    
    ########aglo1&plot1
    
    output$compare_2 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      hc1 <- hclust(x, method = input$type )
      hc2 <- diana(x, metric = input$type_metric)
      
      # Create two dendrograms
      dend1 <- as.dendrogram (hc1)
      dend2 <- as.dendrogram (hc2)
      
      tanglegram(dend1, dend2)
      
      
      
      
    })  
    
    #######plot1&aglo2
    
    
    output$compare_3 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      hc1 <- diana(x, metric = input$type_metric)
      hc2 <- agnes(x, metric = input$type_metric, method = input$type_agnes_1)
      
      # Create two dendrograms
      dend1 <- as.dendrogram (hc1)
      dend2 <- as.dendrogram (hc2)
      
      tanglegram(dend1, dend2)
      
      
      
      
    }) 
   
    #################################
    output$compare_4 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      hc1 <- hclust(x, method = input$type )
      hc2 <- hclust(x, method = input$type_1)
      
      # Create two dendrograms
      dend1 <- as.dendrogram (hc1)
      dend2 <- as.dendrogram (hc2)
      
      tanglegram(dend1, dend2)
      
      
      
      
    })  
    
    #################################
    output$compare_5 <- renderPlot({
      x <- data()
      
      if (is.null(x))
        return(NULL)
      
      hc1 <- agnes(x, metric = input$type_metric, method = input$type_agnes_1)
      hc2 <- agnes(x, metric = input$type_metric, method = input$type_agnes_2)
      
      # Create two dendrograms
      dend1 <- as.dendrogram (hc1)
      dend2 <- as.dendrogram (hc2)
      
      tanglegram(dend1, dend2)
      
      
      
      
    })  
    
    ####
    output$kmeans_out <- renderPlot({ 
      
      x <- data_1()
      if (is.null(x))
        return(NULL)
      x <- t(x)
      k1 <- kmeans(x,centers = input$k_value)
      
      barplot(k1$cluster,col = k1$cluster,xlab = "Features", ylab = "Cluster Number",main="K-means clustering")
      
    })
    output$pam_out <- renderPlot({ 
      
      x <- data_1()
      if (is.null(x))
        return(NULL)
      x <- t(x)
      d <- dist(x, method = input$type_metric_1)
      
      p3 <- pam(d, k=input$k_value,metric = input$pam_metric)
      
      barplot(p3$clustering , col = p3$clustering,xlab = "Features", ylab = "Cluster Number",main="Partition around Mediods")
      
    })
    output$pam_out_silhouette <- renderPlot({ 
      
      x <- data_1()
      if (is.null(x))
        return(NULL)
      x <- t(x)
      d <- dist(x, method = input$type_metric_1)
      
      p3 <- pam(d, k=input$k_value,metric = input$pam_metric)
      
      #barplot(p3$clustering , col = p3$clustering,xlab = "Features", ylab = "Cluster Number",main="Partition around Mediods")
      #plot(p3)
      fviz_silhouette(p3, palette = "jco",
                      ggtheme = theme_minimal())
      
    })
    
    output$fanny_out_silhouette <- renderPlot({ 
      
      x <- data_1()
      if (is.null(x))
        return(NULL)
      d <- dist(x, method = input$type_metric_1)
      
      fan5 <- fanny(d, k=input$k_value, metric = input$fanny_metric, memb.exp = input$fanny_memb_exp)
      
      #plot(fan5)
      
      
      fviz_silhouette(fan5, palette = "jco",
                      ggtheme = theme_minimal())
    })
    
    output$fanny_out <- renderPlot({ 
      
      x <- data_1()
      if (is.null(x))
        return(NULL)
      d <- dist(x, method = input$type_metric_1)
     
      fan5 <- fanny(d, k=input$k_value, metric = input$fanny_metric, memb.exp = input$fanny_memb_exp)
      
      matplot(fan5$membership, type = "b", xaxt = "n",main="Fuzzy clustering", ylab = "",xlab = "Features")
      
      axis(1, 1:ncol(x), colnames(x))
      
    })
    
    output$classical_MDS <- renderPlot({ 
      

      x <- data_2()
      if (is.null(x))
        return(NULL)
      x <- as.data.frame(x)
      if (is.null(x))
        return(NULL)
      
      max_value <- max(x, na.rm=TRUE)
      
      x <- t(x)
      x[is.na(x)]   <- 0
      
      
      d <- as.dist(x)
        #as.dist((max_value+1) - x)
      
      
      
      
      x <- tryCatch(
        {
          
          
          fit <- cmdscale(d,k=input$k_value) # k is the number of dim
          mds1 <- fit
          
          plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "",
               main = "Classical MDS", axes = FALSE)
          text(mds1[,1], mds1[,2], labels(d), cex = 0.9, xpd = TRUE)
          #fit # view results
          
          # plot solution
         # plot(fit$points, type = "n", xlab="Coordinate 1", ylab="Coordinate 2",
         #      main="Metric MDS")
         # text(fit$points, rownames(fit$points)) 
        },
        error = function(e){
        #   
        #   x <- data_2()
        #   if (is.null(x))
        #     return(NULL)
        #    x <- t(x)
        #   # d <- dist(x, method = input$type_metric_1)
        #   # d[is.na(d)]   <- 0
        #   # d <- na.omit(d)
        #   max_value <- max(x, na.rm=TRUE)
        #   d <- as.dist((max_value+1) - x)
        #   d[is.na(d)]   <- 0
        #   d <- na.omit(d)
        # 
        # 
        #   #mds1 = cmdscale(d,k=input$k_value)
        # 
        #   # plot
        #   #plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
        #    #    main = "cmdscale (stats)")
        #  # text(mds1[,1], mds1[,2], labels(d), cex = 0.9, xpd = TRUE)
        #   x <- tryCatch(
        #     {
        #       fit <- cmdscale(d,k=input$k_value) # k is the number of dim
        #       #fit # view results
        # 
        #       # plot solution
        #       plot(fit$points, type = "n", xlab="Coordinate 1", ylab="Coordinate 2",
        #            main="Metric MDS")
        #       text(fit$points, rownames(fit$points))
        #     },
        #     error = function(e1){
        # 
        #     }
        #   )
          
         }
      )
      
      
    })
    
    
    output$iso_mds <- renderPlot({ 
      
      # dat <- as.matrix(read.csv("plaindist.csv")[, -1])
      #read.csv("plaindist.csv", header=TRUE) #read the file
      x <- data_2()
      if (is.null(x))
        return(NULL)
      # max_value <- max(x, na.rm=TRUE)
      # d <- as.dist((max_value+1) - x)
      # d[is.na(d)]   <- 0
      # d <- na.omit(d)
      
      dat <- x
      x <- tryCatch(
        {
      
          max_value <- max(dat, na.rm=TRUE)
          
          dat[is.na(dat)]   <- 0
          dat <- na.omit(dat)
          # d <- as.dist((max_value+1) - dat)
          d <- as.dist(dat)
          
          fit <- isoMDS(d,k=input$k_value) # k is the number of dim
          #fit # view results
          
          # plot solution
          plot(fit$points, type = "n", xlab="", ylab="",
               main="Non-metric MDS", axes = FALSE)
          text(fit$points, rownames(fit$points)) 
        },
        error = function(e){
          
          
          x <- data_2()
          if (is.null(x))
            return(NULL)
          x[is.na(x)]   <- 0
          x<- na.omit(x)
          x <- t(x)
          max_value <- max(x, na.rm=TRUE)
          
          if((max_value+1) > input$no_coders){
            max_value <- max_value
          }else{
            max_value <- input$no_coders -1
          }
            
          d <- as.dist((max_value+1) - x)
          





          # plot

          x <- tryCatch(
            {
              fit <- isoMDS(d,k=input$k_value) # k is the number of dim
              #fit # view results

              # plot solution
              plot(fit$points, type = "n", xlab="", ylab="",
                   main="Non-metric MDS", axes = FALSE)
              text(fit$points, rownames(fit$points))
            },
            error = function(e1){

            }
          )

          
        }
      )
      
    })
    
    output$kmeans_sammon <- renderPlot({ 
      
      # dat <- as.matrix(read.csv("plaindist.csv")[, -1])
      #read.csv("plaindist.csv", header=TRUE) #read the file
      x <- data_2()
      if (is.null(x))
        return(NULL)
      # max_value <- max(x, na.rm=TRUE)
      # #max_value <- max(x, na.rm=TRUE)
      # d <- as.dist((max_value+1) - x)
      # d[is.na(d)]   <- 0
      # d <- na.omit(d)
      
      dat <- x
      
      x <- tryCatch(
        {
          
          max_value <- max(dat, na.rm=TRUE)
          
          dat[is.na(dat)]   <- 0
          dat <- na.omit(dat)
          d <- as.dist(dat)
            #as.dist((max_value+1) - dat)
          swiss.sam <- sammon(d)
          
          kmeans_clust <- kmeans(swiss.sam$points, centers = input$k_value)  # k-means wihth 3 clusters.
          plot(swiss.sam$points, type = "n", main="MDS with sammon() and clustered", xlab="", ylab="", axes = FALSE)
          text(swiss.sam$points, labels = rownames(swiss.sam$points), col = kmeans_clust$cluster) 
        },
        error = function(e){
          
          
          x <- data_2()
          if (is.null(x))
            return(NULL)
          x[is.na(x)]   <- 0
          x <- na.omit(x)
          x <- t(x)
          max_value <- max(x, na.rm=TRUE)
          if((max_value+1) > input$no_coders){
            max_value <- max_value
          }else{
            max_value <- input$no_coders -1
          }          
          d <- as.dist((max_value+1) - x)
          





          # plot

          x <- tryCatch(
            {
              swiss.sam <- sammon(d)

              kmeans_clust <- kmeans(swiss.sam$points, centers = input$k_value)  # k-means wihth 3 clusters.
              plot(swiss.sam$points, type = "n", main="MDS with sammon() and clustered", xlab="", ylab="", axes = FALSE)
              text(swiss.sam$points, labels = rownames(swiss.sam$points), col = kmeans_clust$cluster)
            },
            error = function(e1){

            }
          )
          
        }
      )
      
    })
    
    
 

  }
  
  shinyApp(ui, server)



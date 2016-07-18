options(shiny.maxRequestSize=50*1024^2)
#install.packages('plyr')
library(plyr)
#install.packages('reshape')
library(reshape)

server = function(input, output) {
  
  data1 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors =FALSE)
      #colnames(my_data) <-c("Positon.1.Provider", "Position.1.Insurer", "Position.1.Price", "Positon.2.Provider", "Position.2.Insurer", "Position.2.Price")
      #my_data$ApricotPriceDifference <- (my_data$Position.1.Price + 1)
      #my_data$ResponseSqRoot <- sqrt(my_data$Response + 1)
    })
    #my_data
    y <- count(my_data, "Site.Name.1")
    y
    #ProviderInsurer <- cast(y, Site.Name.1 ~ Insurer.1)
    #ProviderInsurer
  })
  
  data2 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data1 <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors =FALSE)
      #colnames(my_data) <-c("Positon.1.Provider", "Position.1.Insurer", "Position.1.Price", "Positon.2.Provider", "Position.2.Insurer", "Position.2.Price")
      #my_data$ApricotPriceDifference <- (my_data$Position.1.Price + 1)
      #my_data$ResponseSqRoot <- sqrt(my_data$Response + 1)
    })
    UploadColumnNumbers <- ncol(my_data1)
    NumberofProviders <- (ncol(my_data1)-(which( colnames(my_data1)=="Price.Position.1" )-1))/3
    for(j in 1:nrow(my_data1)){
      for(i in 1:NumberofProviders){if(my_data1[j, (which( colnames(my_data1)=="Price.Position.1" ))+(i*3)-1] == input$insurance.provider){ 
        my_data1[j, UploadColumnNumbers+1] <- my_data1[j, (which( colnames(my_data1)=="Price.Position.1" ))+(i*3)-3]
        my_data1[j, UploadColumnNumbers+2] <- my_data1[j, (which( colnames(my_data1)=="Price.Position.1" ))+(i*3)-2]
        my_data1[j, UploadColumnNumbers+3] <- i
      }
      }
    }
    x <- my_data1[,c((which( colnames(my_data1)=="Price.Position.1" )):(which( colnames(my_data1)=="Price.Position.1" )+2),(UploadColumnNumbers+1):(UploadColumnNumbers+3))]
    x[,2] <- as.numeric(x[,2])
    x[,5] <- as.numeric(x[,5])
    x$Price.Difference <- x[,5]-x[,2]
    x[is.na(x)] <- "-"
    colnames(x)[4] <- "Selected.Provider.Price"
    colnames(x)[5] <- "Selected.Provider.Insurer"
    colnames(x)[6] <- "Selected.Provider.Position"
    x
  })
  
  data3 <- reactive({
    if(input$Load3 == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load3
      my_data3 <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors =FALSE)
    })
    my_data2 <- subset(my_data3[,c(1, which(colnames(my_data3)=="Price.Position.1"):ncol(my_data3))], Site.Name.1 == input$insurance.provider)
    my_data2 <- my_data2[,c(1:4, 5:7)]
    my_data2$Price.To.2nd <- as.numeric(my_data2[,5])- as.numeric(my_data2[,2])
    my_data2 <- my_data2[order(-my_data2$Price.To.2nd),]
    my_data2
  })
  
  data4 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data4 <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors =FALSE)
    })
    UploadColumnNumbers <- ncol(my_data4)
    NumberofProviders <- (ncol(my_data4)-(which( colnames(my_data4)=="Price.Position.1" )-1))/3
    for(j in 1:nrow(my_data4)){
      for(i in 1:NumberofProviders){if(my_data4[j,(which( colnames(my_data4)=="Price.Position.1" ))+(i*3)-1] == input$insurance.provider){ 
        my_data4[j,UploadColumnNumbers+1] <- my_data4[j, (which( colnames(my_data4)=="Price.Position.1" ))+(i*3)-3]
        my_data4[j,UploadColumnNumbers+2] <- my_data4[j, (which( colnames(my_data4)=="Price.Position.1" ))+(i*3)-2]
        my_data4[j,UploadColumnNumbers+3] <- i
      }
      }
    }
    z <- my_data4[,c(1, which(colnames(my_data4)=="Price.Position.1"):(which(colnames(my_data4)=="Price.Position.1")+2),(UploadColumnNumbers+1):(UploadColumnNumbers+3))]
	colnames(z)[5] <- "Selected.Provider.Price"
	colnames(z)[6] <- "Selected.Provider.Insurer"
	colnames(z)[7] <- "Selected.Provider.Position"
    z1 <- count(z, c("Selected.Provider.Insurer", "Selected.Provider.Position"))
    ProviderInsurer <- cast(z1, Selected.Provider.Insurer ~ Selected.Provider.Position, value = "freq")
    ProviderInsurer[is.na(ProviderInsurer)] <- 0
    colnames(ProviderInsurer)[1] <- input$insurance.provider
    ProviderInsurer
  })
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SummaryData.csv' }, content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    }
  )
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE)
  
#  output$my_output_data4 <- renderTable({data4()},include.rownames=FALSE)
  
  output$downloadData4 <- downloadHandler(
    filename = function() { 'InsurerRankData.csv' }, content = function(file) {
      write.csv(data4(), file, row.names = FALSE)
    }
  )
  
  output$main_plot <- renderPlot({
    
    hist(as.numeric(data3()[,2]),
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Policy Price",
         main = "Policy Price (£)")
    
#    if (input$individual_obs) {
#     rug(faithful$eruptions)
#    }
    
#    if (input$density) {
#      dens <- density(faithful$eruptions,
#                      adjust = input$bw_adjust)
#      lines(dens, col = "blue")
#    }
    
  })
}
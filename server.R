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
    y <- count(my_data, c("Site.Name.1", "Site.Name.2" ))
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
    x <- my_data1[,c(1,19)]
    x[,1] <- as.numeric(x[,1])
    x[,2] <- as.numeric(x[,2])
    x$Price.Difference <- x[,2]-x[,1]
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
    my_data2 <- subset(my_data3[,1:6], Site.Name.1 == input$insurance.provider)
    my_data2
  })
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SummaryData.csv' }, content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    }
  )
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE)
  
  output$main_plot <- renderPlot({
    
    hist(as.numeric(data3()[,1]),
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
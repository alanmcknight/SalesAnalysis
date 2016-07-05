install.packages('plyr')
library(plyr)
install.packages('reshape')
library(reshape)

server = function(input, output) {
  
  data1 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      #colnames(my_data) <-c("Positon.1.Provider", "Position.1.Insurer", "Position.1.Price", "Positon.2.Provider", "Position.2.Insurer", "Position.2.Price")
      #my_data$ApricotPriceDifference <- (my_data$Position.1.Price + 1)
      #my_data$ResponseSqRoot <- sqrt(my_data$Response + 1)
    })
    #my_data
    y <- count(my_data, c("Provider.Insurer", "Provider.Position"))
    #y
    ProviderInsurer <- cast(y, Provider.Insurer ~ Provider.Position)
    ProviderInsurer
  })
  
  data2 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data1 <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote, stringsAsFactors =FALSE)
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
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SummaryData.csv' }, content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    }
  )
}
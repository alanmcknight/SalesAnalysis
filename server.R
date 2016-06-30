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
    my_data[,c(1:5,19:21)]
  })
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  output$downloadData <- downloadHandler(
    filename = function() { 'filtered_data.csv' }, content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
}
options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)

server = function(input, output, session) {
  
  my_data <- read.csv("Data.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  my_data$Quote.sale.date <- as.Date( as.character(my_data$Quote.sale.date), "%d/%m/%Y")
  
  filterList1 <- colnames(my_data) 
  percent <- function(x, digits = 2, format = "f") {
    paste0(formatC(100 * x, format = format, digits = digits), "%")
  }
  currency <- function(x) {
  paste("£",format(x, big.mark=","),sep="")
  }
  specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
   
  data5 <- reactive({
    my_data1 <- subset(my_data, my_data$Quote.sale.date > input$dateRange[1] & my_data$Quote.sale.date < input$dateRange[2])
    #if(input$filterName2 == 0){return()}
    #if(is.null(input$filterName2)) return(NULL)
    Profit <- aggregate(as.numeric(my_data1$Net.Value), by=list(Category=my_data1[[input$dataset]]), FUN=sum)
    Sales <- my_data1[ which(my_data1$Cancelled=='N'),]
    CountSales <- aggregate(as.numeric(Sales$Net.Value), by=list(Category=Sales[[input$dataset]]), FUN=length)
    
    Cancellations <- my_data1[ which(my_data1$Cancelled=='Y'),]
    CountCancellations <- aggregate(as.numeric(Cancellations[, 15]), by=list(Category=Cancellations[[input$dataset]]), FUN=length)
    
    names(CountSales)[2]<-"Count"
    names(CountCancellations)[2]<-"Count"
    Profit$Sales <- CountSales$Count[match(Profit$Category, CountSales$Category)]
    Profit$Cancellations <- CountCancellations$Count[match(Profit$Category, CountCancellations$Category)]
    Profit[,5] <- percent(as.numeric(Profit[,4])/(as.numeric(Profit[,3])+as.numeric(Profit[,4])))
#    Profit$CancellationPercentage <- as.numeric(Profit$Cancellations)/(as.numeric(Profit$Sales)+as.numeric(Profit$Cancellations))
    names(Profit)[1]<-input$dataset
    names(Profit)[2]<-"Profit"
    names(Profit)[5]<-"Sales Cancelation Percentage"
    Profit <- Profit[order(Profit$Profit),] 
    Profit[is.na(Profit)] <- 0
    Profit
  })
  
  data7 <- reactive({
    Profit <- data5()
    Profit[,6] <- currency(Profit[,2])
    Profit[,2] <- Profit[,6]
    Profit[,6] <- NULL
    Profit
  })
  
  data6 <- reactive({
    data2 <- data5()
#    colSums(as.numeric(data2[,3:4]))
    Totals <- data.frame(matrix(NA, nrow = 1, ncol = 4))
    colnames(Totals) <- c("Profit", "Sales", "Cancellations", "Cancellations Percentage")
    Totals[1,1] <- currency(sum(as.numeric(data2$Profit)))
    Totals[1,2] <- specify_decimal(sum(as.numeric(data2$Sales)), 0)
    Totals[1,3] <- specify_decimal(sum(as.numeric(data2$Cancellations)), 0)
    Totals[1,4] <- percent(as.numeric(Totals[1,3])/(as.numeric(Totals[1,2]) + as.numeric(Totals[1,3])))
    Totals
  })
  
  output$my_output_data6 <- renderTable({data6()}, include.rownames=FALSE)
  output$my_output_data7 <- renderDataTable({data7()})
  
  output$selectUI2 <- renderUI({ 
    selectInput("filterName1", "Select Filter Criteria", filterList1)
  })
}
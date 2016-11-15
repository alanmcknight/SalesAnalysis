options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)
library(shiny)
library(plotly)

server = function(input, output, session) {
  
  my_data <- read.csv("ApricotSalesMaster4.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  my_data$BTXDatecreated <- as.Date( as.character(my_data$BTXDatecreated), "%d/%m/%Y")
  
  filterList1 <- colnames(my_data) 
  percent <- function(x, digits = 2, format = "f") {
    paste0(formatC(100 * x, format = format, digits = digits), "%")
  }
  currency <- function(x) {
  paste("£",format(x, big.mark=","),sep="")
  }
  specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
  
  data <- reactive({
    my_data1 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
    my_data1  
  })
  
  data1 <- reactive({
    my_data1 <- subset(my_data[, c(1:(which(colnames(my_data)=="Proposer.Convictions.Count")), (ncol(my_data)-2):ncol(my_data))], my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
    my_data1  
  })
  
  data6 <- reactive({
    my_data1 <- data()
    my_data2 <- subset(my_data1, BTXPaydt != "")
    Totals <- data.frame(matrix(NA, nrow = 1, ncol = 6))
    colnames(Totals) <- c("New Business", "Renewals", "Renewals Outstanding", "Cancellations", "Cancellation Percentage", "Profit")
    Totals[1,1] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "New Business"))
    Totals[1,2] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "Renewal"))
    Totals[1,3] <- nrow(subset(my_data1, BTXPaydt == "" & BTXTrantype == "Renewal"))
    Totals[1,4] <- nrow(subset(my_data1, Cancellation == "Cancellation"))
    Totals[1,5] <- percent(as.numeric(Totals[1,4])/(as.numeric(Totals[1,1]) + as.numeric(Totals[1,2] + as.numeric(Totals[1,4]))))
    Totals[1,6] <- currency(sum(as.numeric(my_data2$TotalValue)))
    Totals
  })
  
  data7 <- reactive({
    Profit <- data8()
    Profit[,6] <- currency(Profit[,2])
    Profit[,2] <- Profit[,6]
    Profit[,6] <- NULL
    Profit
  })
  
  data8 <- reactive({
    if(length(input$dataset3) == 1){
      my_data1 <- subset(my_data, my_data$BTXDatecreated > input$dateRange[1] & my_data$BTXDatecreated < input$dateRange[2] & BTXPaydt != "")
      my_data2 <- subset(my_data, my_data$BTXDatecreated > input$dateRange[1] & my_data$BTXDatecreated < input$dateRange[2])
      #if(input$filterName2 == 0){return()}
      #if(is.null(input$filterName2)) return(NULL)
      Profit <- aggregate(as.numeric(my_data1$TotalValue), by=list(Category=my_data1[[input$dataset3[1]]]), FUN=sum)
      Profit[2] <- round(Profit[2], 2)
      Sales <- my_data1[ which(my_data1$Cancellation=='N'),]
      CountSales <- aggregate(as.numeric(Sales$TotalValue), by=list(Category=Sales[[input$dataset3[1]]]), FUN=length)
      names(CountSales)[2]<-"Count"
      Cost <- aggregate(as.numeric(Sales$TrafficCost), by=list(Category=Sales[[input$dataset3[1]]]), FUN=sum)
      names(Cost)[2]<-"TrafficCost"
      Profit$Sales <- CountSales$Count[match(Profit$Category, CountSales$Category)]
      Cancellations <- my_data2[ which(my_data2$Cancellation=="Cancellation"),]
      if(nrow(Cancellations) >0){
        CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue), by=list(Category=Cancellations[[input$dataset3[1]]]), FUN=length)
        names(CountCancellations)[2]<-"Count"
        Profit$Cancellations <- CountCancellations$Count[match(Profit$Category, CountCancellations$Category)]
      } else{Profit$Cancellations <- 0}
      Profit[is.na(Profit)] <- 0
      Profit[,5] <- percent(as.numeric(Profit[,4])/(as.numeric(Profit[,3])+as.numeric(Profit[,4])))
      Profit[,6] <- round(Profit[,2]/(as.numeric(Profit[,3])+as.numeric(Profit[,4])), 2)
      Profit$TrafficCost <- Cost$TrafficCost[match(Profit$Category, Cost$Category)]
      #    Profit$CancellationPercentage <- as.numeric(Profit$Cancellations)/(as.numeric(Profit$Sales)+as.numeric(Profit$Cancellations))
      Profit[7][Profit[7]==""]<- 0.00 
      Profit$Y1Profit <- round(Profit[,2] + (Profit[,2]+Profit[,7])*0.5, 2)
      Profit$Y2Profit <- round(Profit[,2] + (Profit[,2]+Profit[,7])*0.5 + (Profit[,2]+Profit[,7])*0.25, 2)
      names(Profit)[1]<-input$dataset3[1]
      names(Profit)[2]<-"Gross Profit (£)"
      names(Profit)[5]<-"Sales Cancellation Percentage"
      names(Profit)[6]<-"Average Gross Profit per Sale (£)"
      names(Profit)[7]<-"Traffic Cost (£)"
      names(Profit)[8]<-"Profit inc. projected 1 year renewals (£)"
      names(Profit)[9]<-"Profit inc. projected 2 year renewals (£)"
      Profit <- Profit[order(Profit[1]),] 
      Profit[is.na(Profit)] <- 0
      Profit
    } else if(length(input$dataset3) == 2){
    my_data1 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2] & BTXPaydt != "")
    my_data2 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
    Profit <- aggregate(as.numeric(my_data1$TotalValue) ~ my_data1[[input$dataset3[1]]] + my_data1[[input$dataset3[2]]], my_data1, FUN=sum)
    Profit[3] <- round(Profit[3], 2)
    Sales <- my_data1[ which(my_data1$Cancellation=='N'),]
    CountSales <- aggregate(as.numeric(Sales$TotalValue) ~ Sales[[input$dataset3[1]]] + Sales[[input$dataset3[2]]], Sales, FUN=length)
    Cost<- aggregate(as.numeric(Sales$TrafficCost) ~ Sales[[input$dataset3[1]]] + Sales[[input$dataset3[2]]], Sales, FUN=sum)
    names(Profit)[1]<-input$dataset3[1]
    names(Profit)[2]<-input$dataset3[2]
    names(CountSales)[1]<-input$dataset3[1]
    names(CountSales)[2]<-input$dataset3[2]
    names(Cost)[1]<-input$dataset3[1]
    names(Cost)[2]<-input$dataset3[2]
    Summary <- merge(Profit, CountSales, by=c(input$dataset3[1],input$dataset3[2]), all=TRUE)
    Cancellations <- my_data2[ which(my_data2$Cancellation=='Cancellation'),]
    if(nrow(Cancellations) >0){
      CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]], Cancellations, FUN=length)
      names(CountCancellations)[1]<-input$dataset3[1]
      names(CountCancellations)[2]<-input$dataset3[2]
      Summary <- merge(Summary, CountCancellations, by=c(input$dataset3[1],input$dataset3[2]), all=TRUE)
    } else{Summary$Cancellations <- 0}
    Summary[is.na(Summary)] <- 0
    Summary[,6] <- percent(as.numeric(Summary[,5])/(as.numeric(Summary[,4])+as.numeric(Summary[,5])))
    Summary[,7] <- Summary[,3]/(as.numeric(Summary[,4])+as.numeric(Summary[,5]))
    names(Summary)[3]<-"Gross Profit (£)"
    names(Summary)[4]<-"Sales"
    names(Summary)[5]<-"Cancellations"
    names(Summary)[6]<-"Sales Cancellation Percentage"
    names(Summary)[7]<-"Average Gross Profit per Sale (£)"
    Summary <- merge(Summary, Cost, by=c(input$dataset3[1],input$dataset3[2]), all=TRUE)
    Summary[8][Summary[8]==""]<- 0.00 
    names(Summary)[8]<-"Traffic Cost (£)"
    Summary[8][Summary[8]==""]<- 0 
    Summary$Y1Profit <- round(Summary[,3] + (Summary[,3]+Summary[,8])*0.5, 2)
    Summary$Y2Profit <- round(Summary[,3] + (Summary[,3]+Summary[,8])*0.5 + (Summary[,3]+Summary[,8])*0.25, 2)
    names(Summary)[9]<-"Profit inc. projected 1 year renewals (£)"
    names(Summary)[10]<-"Profit inc. projected 2 year renewals (£)"
    Summary <- Summary[order(Summary[3]),] 
    Summary
    } else if(length(input$dataset3) == 3){
        my_data1 <- subset(my_data, my_data$BTXDatecreated > input$dateRange[1] & my_data$BTXDatecreated < input$dateRange[2] & BTXPaydt != "")
        my_data2 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
        Profit <- aggregate(as.numeric(my_data1$TotalValue) ~ my_data1[[input$dataset3[1]]] + my_data1[[input$dataset3[2]]]+ my_data1[[input$dataset3[3]]], my_data1, FUN=sum)
        Profit[4] <- round(Profit[4], 2)
        Sales <- my_data1[ which(my_data1$Cancellation=='N'),]
        CountSales <- aggregate(as.numeric(Sales$TotalValue) ~ Sales[[input$dataset3[1]]] + Sales[[input$dataset3[2]]] + Sales[[input$dataset3[3]]], Sales, FUN=length)
        Cost<- aggregate(as.numeric(Sales$TrafficCost) ~ Sales[[input$dataset3[1]]] + Sales[[input$dataset3[2]]] + Sales[[input$dataset3[3]]], Sales, FUN=sum)
        names(Profit)[1]<-input$dataset3[1]
        names(Profit)[2]<-input$dataset3[2]
        names(Profit)[3]<-input$dataset3[3]
        names(CountSales)[1]<-input$dataset3[1]
        names(CountSales)[2]<-input$dataset3[2]
        names(CountSales)[3]<-input$dataset3[3]
        names(Cost)[1]<-input$dataset3[1]
        names(Cost)[2]<-input$dataset3[2]
        names(Cost)[3]<-input$dataset3[3]
        Summary <- merge(Profit, CountSales, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all=TRUE)
        Cancellations <- my_data2[ which(my_data2$Cancellation=='Cancellation'),]
        if(nrow(Cancellations) >0){
          CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]]+ Cancellations[[input$dataset3[3]]], Cancellations, FUN=length)
          names(CountCancellations)[1]<-input$dataset3[1]
          names(CountCancellations)[2]<-input$dataset3[2]
          names(CountCancellations)[3]<-input$dataset3[3]
          Summary <- merge(Summary, CountCancellations, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all=TRUE)
        } else{Summary$Cancellations <- 0}
        Summary[is.na(Summary)] <- 0
        Summary[,7] <- percent(as.numeric(Summary[,6])/(as.numeric(Summary[,5])+as.numeric(Summary[,6])))
        Summary[,8] <- Summary[,4]/(as.numeric(Summary[,5])+as.numeric(Summary[,6]))
        names(Summary)[4]<-"Gross Profit (£)"
        names(Summary)[5]<-"Sales"
        names(Summary)[6]<-"Cancellations"
        names(Summary)[7]<-"Sales Cancellation Percentage"
        names(Summary)[8]<-"Average Gross Profit per Sale (£)"
        Summary <- merge(Summary, Cost, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all=TRUE)
        Summary[9][Summary[9]==""]<- 0.00
        names(Summary)[9]<-"Traffic Cost (£)"
        Summary[9][Summary[9]==""]<- 0 
        Summary$Y1Profit <- round(Summary[,4] + (Summary[,4]+Summary[,9])*0.5, 2)
        Summary$Y2Profit <- round(Summary[,4] + (Summary[,4]+Summary[,9])*0.5 + (Summary[,4]+Summary[,9])*0.25, 2)
        names(Summary)[10]<-"Profit inc. projected 1 year renewals (£)"
        names(Summary)[11]<-"Profit inc. projected 2 year renewals (£)"
        Summary <- Summary[order(Summary[4]),] 
        Summary
      }
  })
  
  data9 <- reactive({
    my_data9<- subset(my_data, my_data$BTXDatecreated > input$dateRange[1] & my_data$BTXDatecreated < input$dateRange[2] & BTXPaydt != "")
    my_data2 <- subset(my_data, my_data$BTXDatecreated > input$dateRange[1] & my_data$BTXDatecreated < input$dateRange[2])
      #if(input$filterName2 == 0){return()}
      #if(is.null(input$filterName2)) return(NULL)
      Profit <- aggregate(as.numeric(my_data9$TotalValue), by=list(Category=my_data9$BTXDatecreated), FUN=sum)
      Sales <- my_data9[ which(my_data9$Cancellation=='N'),]
      CountSales <- aggregate(as.numeric(Sales$TotalValue), by=list(Category=Sales$BTXDatecreated), FUN=length)
      names(CountSales)[2]<-"Count"
      Profit$Sales <- CountSales$Count[match(Profit$Category, CountSales$Category)]
      Cancellations <- data()[ which(data()$Cancellation=='Cancellation'),]
      if(nrow(Cancellations) >0){
      CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue), by=list(Category=Cancellations$BTXDatecreated), FUN=length)
      names(CountSales)[2]<-"Count"
      names(CountCancellations)[2]<-"Count"
      Profit$Sales <- CountSales$Count[match(Profit$Category, CountSales$Category)]
      Profit$Cancellations <- CountCancellations$Count[match(Profit$Category, CountCancellations$Category)]
      } else{Profit$Cancellations <- 0}
      Profit[is.na(Profit)] <- 0
      Profit[,5] <- percent(as.numeric(Profit[,4])/(as.numeric(Profit[,3])+as.numeric(Profit[,4])))
      Profit[,6] <- Profit[,2]/(as.numeric(Profit[,3])+as.numeric(Profit[,4]))
      #    Profit$CancellationPercentage <- as.numeric(Profit$Cancellations)/(as.numeric(Profit$Sales)+as.numeric(Profit$Cancellations))
      names(Profit)[1]<-"BTXDatecreated"
      names(Profit)[2]<-"Gross Profit (£)"
      names(Profit)[5]<-"Sales Cancellation Percentage"
      names(Profit)[6]<-"Average Gross Profit per Sale (£)"
      Profit <- Profit[order(Profit[1]),] 
      Profit[is.na(Profit)] <- 0
      Profit
  })
  
  output$dailyPlot2 <- renderPlotly({
    if(length(input$dataset3) == 0){
      plot_ly(
        x = data9()[,1],
        y = data9()[,input$plotFilter],
        name = "Performance",
        type = "bar"
      )}
    else if(length(input$dataset3) == 1){
    plot_ly(
      x = data8()[,1],
      y = data8()[,input$plotFilter],
      name = "Performance",
      type = "bar"
    )
    } else if(length(input$dataset3) > 1){
    plot_ly(data8()) %>%
      add_trace(data = data8(), type = "bar", x = data8()[,1], y = data8()[,input$plotFilter], color = data8()[,2]) %>%
      layout(barmode = "stack")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SaleData.csv' }, content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  output$my_output_data6 <- renderDataTable({data6()}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
#  output$my_output_data7 <- renderDataTable({data7()})
  output$my_output_data8 <- renderDataTable({data8()})
  
}
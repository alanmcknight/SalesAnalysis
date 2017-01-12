options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)
library(shiny)
library(plotly)

server = function(input, output, session) {
  
  my_data <- read.csv("ApricotSalesMaster2.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  my_data$BTXDatecreated <- as.Date( as.character(my_data$BTXDatecreated), "%d/%m/%Y")
  
  filterList1 <- colnames(my_data) 
  filterList2 <- c("All", sort(unique(my_data$Product)))
  percent <- function(x, digits = 2, format = "f") {
    paste0(formatC(100 * x, format = format, digits = digits), "%")
  }
  currency <- function(x) {
  paste("£",format(x, big.mark=","),sep="")
  }
  specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
  
  data <- reactive({
    my_data1 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
    if(input$filterName != "All"){
      my_data1 <- subset(my_data1, my_data1$Product == input$filterName)
    }
    my_data1
  })
  
  data1 <- reactive({
    my_data1 <- subset(my_data[, c(1:(which(colnames(my_data)=="UK.Residency.Years")), (ncol(my_data)-2):ncol(my_data))], my_data$BTXDatecreated >= input$dateRange[1] & my_data$BTXDatecreated <= input$dateRange[2])
    if(input$filterName != "All"){
      my_data1 <- subset(my_data1, my_data1$Product == input$filterName)
    }
    my_data1
  })
  
  data6 <- reactive({
    my_data1 <- data()
    my_data2 <- subset(my_data1, BTXPaydt != "")
    my_data2 <- my_data1[ which(my_data1$BTXPaydt != "" | my_data1$BTXTrantype == "New Business"),]
    Totals <- data.frame(matrix(NA, nrow = 1, ncol = 6))
    colnames(Totals) <- c("New Business", "Renewals", "Renewals Outstanding", "New Business Cancellations", "New Business Cancellation Percentage", "Profit")
    Totals[1,1] <- nrow(subset(my_data1, Cancellation == "N" & BTXTrantype == "New Business"))
    Totals[1,2] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "Renewal"))
    Totals[1,3] <- nrow(subset(my_data1, BTXPaydt == "" & BTXTrantype == "Renewal"))
    Totals[1,4] <- nrow(subset(my_data1, Cancellation != "N" & BTXTrantype == "New Business"))
    Totals[1,5] <- percent(as.numeric(Totals[1,4])/(as.numeric(Totals[1,1]) + as.numeric(Totals[1,4])))
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
    my_data1 <- data()
    my_data2 <- my_data1
    my_data1 <- subset(my_data1, BTXPaydt != "")
    if(length(input$dataset3) == 1){
      my_data3 <- my_data2[ which(my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business"),]
      Profit <- aggregate(my_data3$TotalValue~my_data3[[input$dataset3[1]]], my_data3, FUN = sum)
      Profit[,2] <- round(Profit[,2], 2)
      Sales <- my_data2[ which(my_data2$Cancellation=='N' & (my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business")),]
      Summary <- aggregate(cbind(Sales$TotalValue, Sales$TrafficCost)~Sales[[input$dataset3[1]]], Sales, FUN = length)
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake"){
        TrafficCostCount <- subset(Sales, Sales$TrafficCost != 0)
        TrafficCostCount <- aggregate(TrafficCostCount$TrafficCost~TrafficCostCount[[input$dataset3[1]]], TrafficCostCount, FUN = length)
        AddOnCount1 <- subset(Sales, Sales$AddOnCount != 0)
        AddOnCount1 <- aggregate(AddOnCount1$AddOnCount~AddOnCount1[[input$dataset3[1]]], AddOnCount1, FUN = length)
        FinanceValueCount <- subset(Sales, Sales$FinanceValue != 0)
        FinanceValueCount <- aggregate(FinanceValueCount$FinanceValue~FinanceValueCount[[input$dataset3[1]]], FinanceValueCount, FUN = length)
        names(TrafficCostCount)[1] <- "Subset1"
        names(AddOnCount1)[1] <- "Subset1"
        names(FinanceValueCount)[1] <- "Subset1"
        Summary2 <- merge(TrafficCostCount, AddOnCount1, by="Subset1", all=T)
        Summary2 <- merge(Summary2, FinanceValueCount, by="Subset1", all=T)
      }else{
      Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue)~Sales[[input$dataset3[1]]], Sales, FUN = sum)
      }
      names(Summary2)[1]<-input$dataset3[1]
      names(Profit)[1]<-input$dataset3[1]
      Profit$Sales <- Summary[,2][match(Profit[,1], Summary[,1])]
      Cancellations <- my_data2[ which(my_data2$Cancellation!="N"),]
      if(nrow(Cancellations) >0){
        CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue), by=list(Category=Cancellations[[input$dataset3[1]]]), FUN=length)
        names(CountCancellations)[2]<-"Count"
        Profit$Cancellations <- CountCancellations$Count[match(Profit[,1], CountCancellations$Category)]
      } else{Profit$Cancellations <- 0}
      Profit <- Profit[,c(1,3,4,2)]
      Profit <- merge(Profit,Summary2, by=input$dataset3[1], all.x=T)
      Profit[is.na(Profit)] <- 0
      if(input$dataset4 == "Mean"){
        Profit[,5:7] <- round(Profit[,5:7]/Profit[,2], 2)
        Profit[,4] <- round(Profit[,4]/(as.numeric(Profit[,2])+as.numeric(Profit[,2])), 2)
      }
      if(input$dataset4 == "% Uptake"){
        Profit[,5:7] <- round(Profit[,5:7]/Profit[,2]*100, 2)
      }
      Profit[,8] <- percent(as.numeric(Profit[,3])/(as.numeric(Profit[,2])+as.numeric(Profit[,3])))
      Profit$Y1Profit <- round(Profit[,4] + (Profit[,4]+Profit[,5])*0.5, 2)
      Profit$Y2Profit <- round(Profit[,4] + (Profit[,4]+Profit[,5])*0.5 + (Profit[,4]+Profit[,5])*0.25, 2)
      names(Profit)[4:8]<-c(paste(input$dataset4, "of Profit", sep = " ") , paste(input$dataset4, "of Traffic Cost", sep = " "), paste(input$dataset4, " of Add-Ons", sep = " "), paste(input$dataset4, "of Finance", sep = " "), "Sales Cancellation Percentage")
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake" ){
          #Profit[,c("Y1Profit", "Y2Profit")] <- NULL
          Profit <- Profit[ -c(4, 9:10) ]
      }
      if(input$dataset4 == "% Uptake"){
        names(Profit)[4]<-"% Paid Traffic"
      }
      if(input$dataset4 == "Mean"){
        Profit <- Profit[ -c(9:10) ]
      }
      Profit
    } else if(length(input$dataset3) == 2){
      my_data3 <- my_data2[ which(my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business"),]
    Profit <- aggregate(as.numeric(my_data3$TotalValue) ~ my_data3[[input$dataset3[1]]] + my_data3[[input$dataset3[2]]], my_data3, FUN=sum)
    Profit[,3] <- round(Profit[,3], 2)
    Sales <- my_data2[ which(my_data2$Cancellation=='N' & (my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business")),]
    Summary <- aggregate(Sales$TotalValue~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]], Sales, FUN = length)
    Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue)~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]], Sales, FUN = sum)
    names(Summary)[1]<-input$dataset3[1]
    names(Summary)[2]<-input$dataset3[2]
    names(Summary2)[1]<-input$dataset3[1]
    names(Summary2)[2]<-input$dataset3[2]
    names(Profit)[1]<-input$dataset3[1]
    names(Profit)[2]<-input$dataset3[2]
    Profit <- merge(Profit, Summary, by=c(input$dataset3[1],input$dataset3[2]), all.x=T)
    Cancellations <- my_data2[ which(my_data2$Cancellation!="N"),]
    if(nrow(Cancellations) >0){
      CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]], Cancellations, FUN=length)
      names(CountCancellations)[1]<-input$dataset3[1]
      names(CountCancellations)[2]<-input$dataset3[2]
      Profit <- merge(Profit, CountCancellations, by=c(input$dataset3[1],input$dataset3[2]), all.x=T)
    } else{Profit$Cancellations <- 0}
    Profit <- merge(Profit,Summary2, by=c(input$dataset3[1],input$dataset3[2]), all.x=T)
    names(Profit)[3]<-"Gross Profit (£)"
    Profit[is.na(Profit)] <- 0
    Profit[,9] <- percent(as.numeric(Profit[,5])/(as.numeric(Profit[,4])+as.numeric(Profit[,5])))
    Profit[,10] <- round(Profit[,3]/(as.numeric(Profit[,4])+as.numeric(Profit[,5])), 2)
    Profit$Y1Profit <- round(Profit[,3] + (Profit[,3]+Profit[,6])*0.5, 2)
    Profit$Y2Profit <- round(Profit[,3] + (Profit[,3]+Profit[,6])*0.5 + (Profit[,3]+Profit[,6])*0.25, 2)
    names(Profit)[4:10]<-c("Sales", "Cancellations", "Traffic Cost", "Add On Value", "Finance Value", "Sales Cancellation Percentage", "Gross Profit per Sale (£)")
    Profit
    } else if(length(input$dataset3) == 3){
      my_data3 <- my_data2[ which(my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business"),]
      Profit <- aggregate(as.numeric(my_data3$TotalValue) ~ my_data3[[input$dataset3[1]]] + my_data3[[input$dataset3[2]]]+ my_data3[[input$dataset3[3]]], my_data3, FUN=sum)
      Profit[,4] <- round(Profit[,4], 2)
      Sales <- my_data2[ which(my_data2$Cancellation=='N' & (my_data2$BTXPaydt != "" | my_data2$BTXTrantype == "New Business")),]
      Summary <- aggregate(Sales$TotalValue~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]]+ Sales[[input$dataset3[3]]], Sales, FUN = length)
      Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue)~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]]+ Sales[[input$dataset3[3]]], Sales, FUN = sum)
      names(Summary)[1]<-input$dataset3[1]
      names(Summary)[2]<-input$dataset3[2]
      names(Summary)[3]<-input$dataset3[3]
      names(Summary2)[1]<-input$dataset3[1]
      names(Summary2)[2]<-input$dataset3[2]
      names(Summary2)[3]<-input$dataset3[3]
      names(Profit)[1]<-input$dataset3[1]
      names(Profit)[2]<-input$dataset3[2]
      names(Profit)[3]<-input$dataset3[3]
      Profit <- merge(Profit, Summary, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all.x=T)
      Cancellations <- my_data2[ which(my_data2$Cancellation!="N"),]
      if(nrow(Cancellations) >0){
        CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]] + Cancellations[[input$dataset3[3]]], Cancellations, FUN=length)
        names(CountCancellations)[1]<-input$dataset3[1]
        names(CountCancellations)[2]<-input$dataset3[2]
        names(CountCancellations)[3]<-input$dataset3[3]
        Profit <- merge(Profit, CountCancellations, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all.x=T)
      } else{Profit$Cancellations <- 0}
      Profit <- merge(Profit,Summary2, by=c(input$dataset3[1],input$dataset3[2],input$dataset3[3]), all.x=T)
      names(Profit)[4]<-"Gross Profit (£)"
      Profit[is.na(Profit)] <- 0
      Profit[,10] <- percent(as.numeric(Profit[,6])/(as.numeric(Profit[,5])+as.numeric(Profit[,6])))
      Profit[,11] <- round(Profit[,4]/(as.numeric(Profit[,5])+as.numeric(Profit[,6])), 2)
      Profit$Y1Profit <- round(Profit[,4] + (Profit[,4]+Profit[,7])*0.5, 2)
      Profit$Y2Profit <- round(Profit[,4] + (Profit[,4]+Profit[,7])*0.5 + (Profit[,4]+Profit[,7])*0.25, 2)
      names(Profit)[5:11]<-c("Sales", "Cancellations", "Traffic Cost", "Add On Value", "Finance Value", "Sales Cancellation Percentage", "Gross Profit per Sale (£)")
      Profit
      }
  })
  
  data9 <- reactive({
    my_data9 <- data()
    my_data2 <- my_data9
    my_data9<- subset(my_data9,  BTXPaydt != "")
      #if(input$filterName2 == 0){return()}
      #if(is.null(input$filterName2)) return(NULL)
      Profit <- aggregate(as.numeric(my_data9$TotalValue), by=list(Category=my_data9$BTXDatecreated), FUN=sum)
      Sales <- my_data9[ which(my_data9$Cancellation=='N'),]
      CountSales <- aggregate(as.numeric(Sales$TotalValue), by=list(Category=Sales$BTXDatecreated), FUN=length)
      names(CountSales)[2]<-"Count"
      Profit$Sales <- CountSales$Count[match(Profit$Category, CountSales$Category)]
      Cancellations <- data()[ which(data()$Cancellation!='N'),]
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
      names(Profit)[6]<-"Gross Profit per Sale (£)"
      Profit <- Profit[order(Profit[1]),] 
      Profit[is.na(Profit)] <- 0
      Profit
  })
  
  output$selectUI2 <- renderUI({ 
    selectInput("filterName", "Select Product", filterList2)
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
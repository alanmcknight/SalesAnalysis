library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(stringr)
library(lubridate)
source("user.R")
source("admin.R")

my_username <- c("stephen.mccann", "helen.campbell", "alan","mark.walker", "greg.wilson", "admin")
my_password <- c("Stephen","Helen", "Alan", "Mark", "Greg", "123")
get_role=function(user){
  if(user!="admin") {
    return("TEST")
  }else{
    return("ADMIN")
  }
}

get_ui=function(role){
  if(role=="TEST"){
    return(list_field_user)
  }else{
    return(list_field_admin)
  }
}

shinyServer(function(input, output,session) {
  
  my_data <- read.csv("ApricotSalesMaster2.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  AdData <- read.csv("AdditionalReport.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
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
  
  ### REPORTING ###
  data2 <- reactive({
    if(input$reportSelect[1] == "Sales Report"){
    my_data1 <- subset(my_data[, c(1:(which(colnames(my_data)=="UK.Residency.Years")), (ncol(my_data)-2):ncol(my_data))], my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2])
    my_data1}else if(input$reportSelect[1] == "USwitch Report"){
      #my_data$BTXDatecreated <- as.Date( as.character(my_data$BTXDatecreated), "%d/%m/%Y")
      Data1 <- AdData
      USwitchData <- my_data[grep("APRUS", my_data$ECWebref),]
      SalesData<- subset(USwitchData, USwitchData$BTXDatecreated >= input$dateRange1[1] & USwitchData$BTXDatecreated <= input$dateRange1[2])
      CancellationData <- subset(USwitchData, USwitchData$Cancellation != "N")
      CancellationData <- subset(CancellationData, as.Date(CancellationData$Cancellation, "%d/%m/%Y") >= input$dateRange1[1] & as.Date(CancellationData$Cancellation, "%d/%m/%Y") <= input$dateRange1[2])
      USwitchData <- rbind(SalesData, CancellationData)
      USwitchData <- USwitchData [!duplicated(USwitchData ), ]
      
      USwitchData$recordtype <- "Sale"
      USwitchData$salesmonth <- cut(as.Date(USwitchData$BTXDtraised), "month")
      USwitchData$brand <- "Apricot"
      USwitchData$surname <- "NA"
      
      USwitchData1 <- merge(USwitchData, Data1, by = "BTXPolref", all.x=TRUE)
      
      USwitchData1 <- USwitchData1[c("recordtype", "salesmonth", "brand", "BCMEmail.x", "BCMPcode.x", "BCMName", "surname", "BCMDob.x", "CFReg", "BTXDtraised.x", "ECWebref.x", "BTXPolref", "BTXPaymethod.x", "BTXOrigdebt.x", "BTXDatecreated.x", "Cancellation", "Cancellation", "FinanceValue", "BTXInsurer.x")]
      
      USwitchData1$surname <- word(USwitchData1$BCMName, -1)
      USwitchData1$BCMName <- word(USwitchData1$BCMName, -2)
      
      colnames(USwitchData1) <- c("recordtype", "salesmonth", "brand", "emailaddress", "postcode", "firstname", "surname", "dob", "carregistrationnumber", "policystartdate", "policyquotereference",	"providerquotereference",	"purchasechannel",	"premium",	"policypurchasedate",	"cancellationreason",	"cancellationeffectivedate",	"purchasetype",	"insurerunderwritingpolicy")
      
      USwitchData1 <- USwitchData1[!duplicated(USwitchData1), ]
      
      USwitchData1$purchasechannel[USwitchData1$purchasechannel == "O"] <- "Online"
      USwitchData1$purchasechannel[USwitchData1$purchasechannel != "Online"] <- "Telephone"
      USwitchData1$cancellationreason[USwitchData1$cancellationreason != "N"] <- "NTU"
      
      USwitchData1$purchasetype[USwitchData1$purchasetype != "0"] <- "Monthly"
      USwitchData1$purchasetype[USwitchData1$purchasetype == "0"] <- "Annual"
      USwitchData1
    }else if(input$reportSelect[1] == "MIS Report"){
      MISReport <- AdData[AdData$BTXDtsettled == "" & AdData$BTXInsurer == "MIS Claims" & AdData$BTXPoltype != "HQ",]
      MISReport <- MISReport[,c("BTXPolref", "BCMName", "BCMAddr1", "BCMAddr2", "BCMAddr3", "BCMAddr4", "BCMPcode", "BCMTel", "BTXDtraised")]
      MISReport$BTXDtraised <- as.Date(MISReport$BTXDtraised, "%d/%m/%Y")
      year(MISReport$BTXDtraised) <- year(MISReport$BTXDtraised)+1
      MISReport$UserID <- substr(MISReport[,1], 1, 6)
      
      AdData$CFReg <- ifelse(AdData$CFReg == "", AdData$TW1Regmark, AdData$CFReg)
      #Data$CFReg[Data$CFReg == ""] <- Data$TW1Regmark[Data$CFReg == ""]
      
      VehicleReg <- AdData[AdData$CFReg != "",c("BTXPolref", "CFReg")]
      VehicleReg <- VehicleReg[!duplicated(VehicleReg), ]
      VehicleReg$UserID <- substr(VehicleReg$BTXPolref, 1, 6)
      
      MISReport <- merge(MISReport, VehicleReg, by = "UserID", all.x=TRUE)
      MISReport$UserID <- NULL
      MISReport$BTXPolref <- NULL
      
      MISReport <- MISReport[,c("BTXPolref.x", "BCMName", "BCMAddr1", "BCMAddr2", "BCMAddr3", "BCMAddr4", "BCMPcode", "BCMTel", "CFReg", "BTXDtraised")]
      
      colnames(MISReport) <- c("Broker Ref", "Name", "Address 1", "Address 2", "Address 3", "Address 4", "Postcode", "Phone Number", "Vehicle Registration", "Policy Renewal Date")
      
      MISReport <- MISReport[!duplicated(MISReport), ]
      MISReport
    }else{
      my_data2 <- 2
      my_data2
    }
  })
  
  #Summary Table Sales Tab
  data6 <- reactive({
    my_data1 <- data()
    my_data2 <- subset(my_data1, BTXPaydt != "")
    my_data2 <- my_data1[ which(my_data1$BTXPaydt != "" | my_data1$BTXTrantype == "New Business"),]
    Totals <- data.frame(matrix(NA, nrow = 1, ncol = 6))
    colnames(Totals) <- c("New Business", "Renewals", "Pending Renewals", "New Business Cancellations", "New Business Cancellation Percentage", "Profit")
    Totals[1,1] <- nrow(subset(my_data1, Cancellation == "N" & BTXTrantype == "New Business"))
    Totals[1,2] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "Renewal"))
    Totals[1,3] <- nrow(subset(my_data1, BTXTrantype == "Pending Renewal"))
    Totals[1,4] <- nrow(subset(my_data1, Cancellation != "N" & BTXTrantype == "New Business"))
    Totals[1,5] <- percent(as.numeric(Totals[1,4])/(as.numeric(Totals[1,1]) + as.numeric(Totals[1,4])))
    Totals[1,6] <- currency(sum(as.numeric(my_data2$TotalValue)))
    Totals
  })
  
  ##Sorting by Profit and adding £ symbol
  # data7 <- reactive({
  #   Profit <- data8()
  #   Profit[,6] <- currency(Profit[,2])
  #   Profit[,2] <- Profit[,6]
  #   Profit[,6] <- NULL
  #   Profit
  # })
  
  ## Main Graph and Table##
  data8 <- reactive({
    my_data1 <- data()
    my_data2 <- my_data1
    #my_data1 <- subset(my_data1, BTXPaydt != "")
    if(length(input$dataset3) == 1){
      Profit <- aggregate(my_data2$TotalValue~my_data2[[input$dataset3[1]]], my_data2, FUN = sum)
      Profit[,2] <- round(Profit[,2], 2)
      Sales <- my_data2[ which(my_data2$Cancellation=='N'),]
      Summary <- aggregate(cbind(Sales$TotalValue, Sales$TrafficCost)~Sales[[input$dataset3[1]]], Sales, FUN = length)
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake"){
        TrafficCostCount <- subset(Sales, Sales$TrafficCost != 0)
        TrafficCostCount <- aggregate(TrafficCostCount$TrafficCost~TrafficCostCount[[input$dataset3[1]]], TrafficCostCount, FUN = length)
        AddOnCount1 <- subset(Sales, Sales$AddOnCount != 0)
        AddOnCount1 <- aggregate(AddOnCount1$AddOnCount~AddOnCount1[[input$dataset3[1]]], AddOnCount1, FUN = length)
        FinanceValueCount <- subset(Sales, Sales$FinanceValue != 0)
        FinanceValueCount <- aggregate(FinanceValueCount$FinanceValue~FinanceValueCount[[input$dataset3[1]]], FinanceValueCount, FUN = length)
        DiscountCount <- subset(Sales, Sales$Discount < 0)
        DiscountCount <- aggregate(DiscountCount$Discount~DiscountCount[[input$dataset3[1]]], DiscountCount, FUN = length)
        names(TrafficCostCount)[1] <- "Subset1"
        names(AddOnCount1)[1] <- "Subset1"
        names(FinanceValueCount)[1] <- "Subset1"
        names(DiscountCount)[1] <- "Subset1"
        Summary2 <- merge(TrafficCostCount, AddOnCount1, by="Subset1", all=T)
        Summary2 <- merge(Summary2, FinanceValueCount, by="Subset1", all=T)
        Summary2 <- merge(Summary2, DiscountCount, by="Subset1", all=T)
      }else{
        Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue, Sales$Discount)~Sales[[input$dataset3[1]]], Sales, FUN = sum)
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
        Profit[,5:8] <- round(Profit[,5:8]/Profit[,2], 2)
        Profit[,4] <- round(Profit[,4]/(as.numeric(Profit[,2])+as.numeric(Profit[,2])), 2)
      }
      if(input$dataset4 == "% Uptake"){
        Profit[,5:8] <- round(Profit[,5:8]/Profit[,2]*100, 2)
      }
      Profit[,9] <- round((as.numeric(Profit[,3])/(as.numeric(Profit[,2])+as.numeric(Profit[,3])))*100, 2)
      Profit$Y1Profit <- round((Profit[,4]+Profit[,5])*0.75, 2)
      Profit$Y2Profit <- round((Profit[,4]+Profit[,5])*0.56, 2)
      names(Profit)[4:9]<-c(paste(input$dataset4, "of Profit", sep = " ") , paste(input$dataset4, "of Traffic Cost", sep = " "), paste(input$dataset4, " of Add-Ons", sep = " "), paste(input$dataset4, "of Finance", sep = " "), paste(input$dataset4, "of Discount", sep = " "), "Sales Cancellation Percentage")
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake" ){
        #Profit[,c("Y1Profit", "Y2Profit")] <- NULL
        Profit <- Profit[ -c(4, 10:11) ]
      }
      if(input$dataset4 == "% Uptake"){
        names(Profit)[4]<-"% Paid Traffic"
      }
      if(input$dataset4 == "Mean"){
        Profit <- Profit[ -c(9:11) ]
      }
      Profit2 <- aggregate(my_data2$TotalValue~my_data2[[input$dataset3[1]]], my_data2, FUN = sum)
      names(Profit2)[2]<-"Total Profit"
      Profit <- merge(Profit, Profit2, by=1, all.x=T)
      Profit[,ncol(Profit)+1] <- round(Profit[,ncol(Profit)]/(Profit[,2]+Profit[,3]), 2)
      names(Profit)[ncol(Profit)]<-"Average Profit"
      Profit
    }else if(length(input$dataset3) == 2){
      Profit <- aggregate(as.numeric(my_data2$TotalValue) ~ my_data2[[input$dataset3[1]]] + my_data2[[input$dataset3[2]]], my_data2, FUN=sum)
      Profit[,3] <- round(Profit[,3], 2)
      Sales <- my_data2[ which(my_data2$Cancellation=='N'),]
      Summary <- aggregate(Sales$TotalValue~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]], Sales, FUN = length)
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake"){
        TrafficCostCount <- subset(Sales, Sales$TrafficCost != 0)
        TrafficCostCount <- aggregate(TrafficCostCount$TrafficCost~TrafficCostCount[[input$dataset3[1]]]+TrafficCostCount[[input$dataset3[2]]], TrafficCostCount, FUN = length)
        AddOnCount1 <- subset(Sales, Sales$AddOnCount != 0)
        AddOnCount1 <- aggregate(AddOnCount1$AddOnCount~AddOnCount1[[input$dataset3[1]]]+AddOnCount1[[input$dataset3[2]]], AddOnCount1, FUN = length)
        FinanceValueCount <- subset(Sales, Sales$FinanceValue != 0)
        FinanceValueCount <- aggregate(FinanceValueCount$FinanceValue~FinanceValueCount[[input$dataset3[1]]]+FinanceValueCount[[input$dataset3[2]]], FinanceValueCount, FUN = length)
        DiscountCount <- subset(Sales, Sales$Discount < 0)
        DiscountCount <- aggregate(DiscountCount$Discount~DiscountCount[[input$dataset3[1]]]+DiscountCount[[input$dataset3[2]]], DiscountCount, FUN = length)
        names(TrafficCostCount)[1] <- "Subset1"
        names(TrafficCostCount)[2] <- "Subset2"
        names(AddOnCount1)[1] <- "Subset1"
        names(AddOnCount1)[2] <- "Subset2"
        names(FinanceValueCount)[1] <- "Subset1"
        names(FinanceValueCount)[2] <- "Subset2"
        names(DiscountCount)[1] <- "Subset1"
        names(DiscountCount)[2] <- "Subset2"
        Summary2 <- merge(TrafficCostCount, AddOnCount1, by=c("Subset1", "Subset2"), all=T)
        Summary2 <- merge(Summary2, FinanceValueCount, by=c("Subset1", "Subset2"), all=T)
        Summary2 <- merge(Summary2, DiscountCount, by=c("Subset1", "Subset2"), all=T)
      }else{
        Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue, Sales$Discount)~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]], Sales, FUN = sum)
      }
      names(Summary)[1]<-input$dataset3[1]
      names(Summary)[2]<-input$dataset3[2]
      names(Summary2)[1]<-input$dataset3[1]
      names(Summary2)[2]<-input$dataset3[2]
      names(Profit)[1]<-input$dataset3[1]
      names(Profit)[2]<-input$dataset3[2]
      Profit <- merge(Profit, Summary, by=c(input$dataset3[1],input$dataset3[2]), all.x=T)
      names(Profit)[4] <- "Sales"
      Cancellations <- my_data2[which(my_data2$Cancellation!="N"),]
      if(nrow(Cancellations) >0){
        CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]], Cancellations, FUN=length)
        names(CountCancellations)[1]<-input$dataset3[1]
        names(CountCancellations)[2]<-input$dataset3[2]
        Profit <- merge(Profit, CountCancellations, by=c(input$dataset3[1],input$dataset3[2]), all=T)
        names(Profit)[5] <- "Cancellations"
      }else{Profit$Cancellations <- 0}
      Profit <- Profit[,c(1,2,4,5,3)]
      Profit <- merge(Profit,Summary2, by=c(input$dataset3[1],input$dataset3[2]), all.x=T)
      Profit[is.na(Profit)] <- 0
      if(input$dataset4 == "Mean"){
        Profit[,6:9] <- round(Profit[,6:9]/Profit[,3], 2)
        Profit[,5] <- round(Profit[,5]/(as.numeric(Profit[,3])+as.numeric(Profit[,4])), 2)
      }
      if(input$dataset4 == "% Uptake"){
        Profit[,6:9] <- round(Profit[,6:9]/Profit[,3]*100, 2)
      }
      Profit[,10] <- round((as.numeric(Profit[,4])/(as.numeric(Profit[,3])+as.numeric(Profit[,4]))*100), 2)
      Profit$Y1Profit <- round((Profit[,5]+Profit[,6])*0.5+abs(Profit[,9]*0.5*-0.5), 2)
      Profit$Y2Profit <- round((Profit[,5]+Profit[,6])*0.25+abs(Profit[,9]*0.25*-0.5), 2)
      names(Profit)[5:10]<-c(paste(input$dataset4, "of Profit", sep = " ") , paste(input$dataset4, "of Traffic Cost", sep = " "), paste(input$dataset4, " of Add-Ons", sep = " "), paste(input$dataset4, "of Finance", sep = " "), paste(input$dataset4, "of Discount", sep = " "), "Sales Cancellation Percentage")
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake" ){
        #Profit[,c("Y1Profit", "Y2Profit")] <- NULL
        Profit <- Profit[ -c(5, 11:12) ]
      }
      if(input$dataset4 == "% Uptake"){
        names(Profit)[5]<-"% Paid Traffic"
      }
      if(input$dataset4 == "Mean"){
        Profit <- Profit[ -c(10:12) ]
      }
      Profit2 <- aggregate(my_data2$TotalValue~my_data2[[input$dataset3[1]]]+my_data2[[input$dataset3[2]]], my_data2, FUN = sum)
      names(Profit2)[3]<-"Total Profit"
      Profit <- merge(Profit, Profit2, by=c(1, 2), all.x=T)
      Profit[,ncol(Profit)+1] <- round(Profit[,ncol(Profit)]/(Profit[,3]+Profit[,4]), 2)
      names(Profit)[ncol(Profit)]<-"Average Profit"
      Profit
    } else if(length(input$dataset3) == 3){
      Profit <- aggregate(as.numeric(my_data2$TotalValue) ~ my_data2[[input$dataset3[1]]] + my_data2[[input$dataset3[2]]]+ my_data2[[input$dataset3[3]]], my_data2, FUN=sum)
      Profit[,4] <- round(Profit[,4], 2)
      Sales <- my_data2[ which(my_data2$Cancellation=='N'),]
      Summary <- aggregate(Sales$TotalValue~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]]+ Sales[[input$dataset3[3]]], Sales, FUN = length)
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake"){
        TrafficCostCount <- subset(Sales, Sales$TrafficCost != 0)
        TrafficCostCount <- aggregate(TrafficCostCount$TrafficCost~TrafficCostCount[[input$dataset3[1]]]+TrafficCostCount[[input$dataset3[2]]]+TrafficCostCount[[input$dataset3[3]]], TrafficCostCount, FUN = length)
        AddOnCount1 <- subset(Sales, Sales$AddOnCount != 0)
        AddOnCount1 <- aggregate(AddOnCount1$AddOnCount~AddOnCount1[[input$dataset3[1]]]+AddOnCount1[[input$dataset3[2]]]+AddOnCount1[[input$dataset3[3]]], AddOnCount1, FUN = length)
        FinanceValueCount <- subset(Sales, Sales$FinanceValue != 0)
        FinanceValueCount <- aggregate(FinanceValueCount$FinanceValue~FinanceValueCount[[input$dataset3[1]]]+FinanceValueCount[[input$dataset3[2]]]+FinanceValueCount[[input$dataset3[3]]], FinanceValueCount, FUN = length)
        DiscountCount <- subset(Sales, Sales$Discount < 0)
        DiscountCount <- aggregate(DiscountCount$Discount~DiscountCount[[input$dataset3[1]]]+DiscountCount[[input$dataset3[2]]]+DiscountCount[[input$dataset3[3]]], DiscountCount, FUN = length)
        names(TrafficCostCount)[1] <- "Subset1"
        names(TrafficCostCount)[2] <- "Subset2"
        names(TrafficCostCount)[3] <- "Subset3"
        names(AddOnCount1)[1] <- "Subset1"
        names(AddOnCount1)[2] <- "Subset2"
        names(AddOnCount1)[3] <- "Subset3"
        names(FinanceValueCount)[1] <- "Subset1"
        names(FinanceValueCount)[2] <- "Subset2"
        names(FinanceValueCount)[3] <- "Subset3"
        names(DiscountCount)[1] <- "Subset1"
        names(DiscountCount)[2] <- "Subset2"
        names(DiscountCount)[3] <- "Subset3"
        Summary2 <- merge(TrafficCostCount, AddOnCount1, by=c("Subset1", "Subset2", "Subset3"), all=T)
        Summary2 <- merge(Summary2, FinanceValueCount, by=c("Subset1", "Subset2", "Subset3"), all=T)
        Summary2 <- merge(Summary2, DiscountCount, by=c("Subset1", "Subset2", "Subset3"), all=T)
      }else{
        Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue, Sales$Discount)~Sales[[input$dataset3[1]]]+ Sales[[input$dataset3[2]]]+ Sales[[input$dataset3[3]]], Sales, FUN = sum)
      }
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
      names(Profit)[5] <- "Sales"
      Cancellations <- my_data2[ which(my_data2$Cancellation!="N"),]
      if(nrow(Cancellations) >0){
        CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue) ~ Cancellations[[input$dataset3[1]]] + Cancellations[[input$dataset3[2]]] + Cancellations[[input$dataset3[3]]], Cancellations, FUN=length)
        names(CountCancellations)[1]<-input$dataset3[1]
        names(CountCancellations)[2]<-input$dataset3[2]
        names(CountCancellations)[3]<-input$dataset3[3]
        Profit <- merge(Profit, CountCancellations, by=c(input$dataset3[1],input$dataset3[2], input$dataset3[3]), all=T)
        names(Profit)[6] <- "Cancellations"
      } else{Profit$Cancellations <- 0}
      Profit <- Profit[,c(1,2,3,5,6,4)]
      Profit <- merge(Profit,Summary2, by=c(input$dataset3[1],input$dataset3[2],input$dataset3[3]), all.x=T)
      Profit[is.na(Profit)] <- 0
      if(input$dataset4 == "Mean"){
        Profit[,7:10] <- round(Profit[,7:10]/Profit[,4], 2)
        Profit[,6] <- round(Profit[,6]/(as.numeric(Profit[,4])+as.numeric(Profit[,5])), 2)
      }
      if(input$dataset4 == "% Uptake"){
        Profit[,7:10] <- round(Profit[,7:10]/Profit[,4]*100, 2)
      }
      Profit[,11] <- round(as.numeric((Profit[,5])/(as.numeric(Profit[,4])+as.numeric(Profit[,5])))*100, 2)
      Profit$Y1Profit <- round((Profit[,6]+Profit[,7])*0.5+abs(Profit[,10]*0.5*-0.5), 2)
      Profit$Y2Profit <- round((Profit[,6]+Profit[,7])*0.25+abs(Profit[,10]*0.25*-0.5), 2)
      names(Profit)[6:11]<-c(paste(input$dataset4, "of Profit", sep = " ") , paste(input$dataset4, "of Traffic Cost", sep = " "), paste(input$dataset4, " of Add-Ons", sep = " "), paste(input$dataset4, "of Finance", sep = " "), paste(input$dataset4, "of Discount", sep = " "), "Sales Cancellation Percentage")
      if(input$dataset4 == "Count" | input$dataset4 == "% Uptake" ){
        #Profit[,c("Y1Profit", "Y2Profit")] <- NULL
        Profit <- Profit[ -c(6, 11:13) ]
      }
      if(input$dataset4 == "% Uptake"){
        names(Profit)[6]<-"% Paid Traffic"
      }
      if(input$dataset4 == "Mean"){
        Profit <- Profit[ -c(11:13) ]
      }
      Profit2 <- aggregate(my_data2$TotalValue~my_data2[[input$dataset3[1]]]+my_data2[[input$dataset3[2]]]+my_data2[[input$dataset3[3]]], my_data2, FUN = sum)
      names(Profit2)[4]<-"Total Profit"
      Profit <- merge(Profit, Profit2, by=c(1, 2, 3), all.x=T)
      Profit[,ncol(Profit)+1] <- round(Profit[,ncol(Profit)]/(Profit[,4]+Profit[,5]), 2)
      names(Profit)[ncol(Profit)]<-"Average Profit"
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
    Profit[,5] <- as.numeric(Profit[,4])/(as.numeric(Profit[,3])+as.numeric(Profit[,4]))*100
    Profit[,6] <- Profit[,2]/(as.numeric(Profit[,3])+as.numeric(Profit[,4]))
    #    Profit$CancellationPercentage <- as.numeric(Profit$Cancellations)/(as.numeric(Profit$Sales)+as.numeric(Profit$Cancellations))
    names(Profit)[1]<-"BTXDatecreated"
    names(Profit)[2]<-"Total Profit"
    names(Profit)[5]<-"Sales Cancellation Percentage"
    names(Profit)[6]<-"Average Profit"
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
  
  output$dailyPlot3 <- renderPlotly({
    if(length(input$dataset3) == 0){
      plot_ly(
        x = data9()[,1],
        y = data9()[,input$plotFilter],
        name = "Performance",
        type = "bar"
      )}
    else if(length(input$dataset3) == 1){
      # plot_ly(data8(), x = ~data8()[,1], y = ~data8()[,4], type = 'bar', name = 'Gross Profit') %>%
      #   add_trace(y = ~data8()[,10], name = 'Y1Profit') %>%
      #   add_trace(y = ~data8()[,11], name = 'Y2Profit') %>%
      #   layout(yaxis = list(title = 'Sum'), xaxis = list(title = ""), barmode = 'group')
      
      p <- plot_ly(data8(), x = ~data8()[,1], y = ~data8()[,4], type = 'scatter', mode = 'lines', name = 'Gross Profit') %>%
        add_trace(y = ~data8()[,10], name = 'Y1Profit', mode = 'lines+markers') %>%
        add_trace(y = ~data8()[,11], name = 'Y2Profit', mode = 'lines+markers') %>%
        layout(yaxis = list(title = 'Sum £'), xaxis = list(title = ""))
      p
      
      # x = data8()[,1],
      # y = data8()[,Sales],
      # name = "Performance",
      # type = "bar"
    } else if(length(input$dataset3) > 1){
      plot <- data8()
      plot <- subset(plot, plot[,2] == "New Business")
      p <- plot_ly(plot, x = ~plot[,1], y = ~plot[,5], type = 'scatter', mode = 'lines', name = 'Gross Profit') %>%
        add_trace(y = ~plot[,11], name = 'Y1Profit', mode = 'lines+markers') %>%
        add_trace(y = ~plot[,12], name = 'Y2Profit', mode = 'lines+markers') %>%
        layout(yaxis = list(title = 'Sum £'), xaxis = list(title = ""))
      p
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SaleData.csv' }, content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  output$reportDownload <- downloadHandler(
    filename = function() { 'SaleData.csv' }, content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    }
  )
  
  output$my_output_data6 <- renderDataTable({data6()}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
  output$my_output_data8 <- renderDataTable({
    if(length(input$dataset3) > 0){data8()[,1:(ncol(data8())-2)]}
  })
  
  USER <- reactiveValues(Logged = FALSE,role=NULL)
  
  ui1 <- function(){
    tagList(
      div(id = "login",
          wellPanel(textInput("userName", "Username"),
                    passwordInput("passwd", "Password"),
                    br(),actionButton("Login", "Log in")))
      ,tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -10px;margin-left: -150px;}")
    )}
  
  ui2 <- function(){list(tabPanel("Sales",get_ui(USER$role)[2:3]),get_ui(USER$role)[[1]])}
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
              USER$role=get_role(Username)
              
            }
          } 
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        box(
          div(class="outer",do.call(bootstrapPage,c("",ui1()))))
      })
    }
    if (USER$Logged == TRUE)    {
      output$page <- renderUI({
        box(width = 12,
            div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Apricot Dashboard",ui2())))
        )})
      #print(ui)
    }
  })
})
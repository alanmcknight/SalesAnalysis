options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)

server = function(input, output, session) {
  
    my_data3 <- read.csv("ApricotQuotesMasterMarch.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    my_data3 <- subset(my_data3, !is.na(my_data3$Apricot.Position))
    #my_data3 <- read.table("QuotesAugust161.csv", header = TRUE, nrows = 114898, sep=",", stringsAsFactors =FALSE, fileEncoding="latin1")
    
    data <- reactive({
      my_data <- subset(my_data3, my_data3$Insurer == input$filterName1)
      UploadColumnNumbers <- ncol(my_data)
      my_data[, UploadColumnNumbers+1] <- (as.numeric(my_data[, (UploadColumnNumbers-2)])+as.numeric(input$integer))*(1+((as.numeric(input$percentage))/100))
      my_data[, UploadColumnNumbers+2] <- my_data[, UploadColumnNumbers+1] - my_data[, UploadColumnNumbers-2]
      # pos<-nrow(my_data[my_data[,UploadColumnNumbers+2] > 0, ])
      my_data <- my_data[order(my_data[, UploadColumnNumbers+2]),]
      # my_data[, UploadColumnNumbers+2] <- NULL
      my_data
    })
    
    # positives <- reactive({
    #   my_data <- data()
    #   UploadColumnNumbers <- ncol(my_data)
    #   pos<- nrow(my_data[my_data[,UploadColumnNumbers+2] > 0, ])
    #   pos
    # })
    
    filterList1 <- colnames(my_data3[((which(colnames(my_data3)=="Price.Position.1"))-29):((which(colnames(my_data3)=="Price.Position.1"))-1)])
#    filterList2 <- sort(c(filterList1,  "Insurer", "Sale.Made", "Employment.Status", "Type.of.driving.licence", "Drivers.to.be.insured.", "Voluntary.excess.", "What.type.of.cover.would.you.like.", "How.many.years.no.claims.bonus..NCB..do.you.have.", "What.is.the.estimated.value.of.the.vehicle.", "Post.Code.Prefix", "Have.you.had.any.unspent.non.motoring.criminal.convictions.", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.", "Proposer.Claims.Count", "Proposer.Convictions.Count", "Email.Domain"))
    filterList1 <- sort(c(filterList1, "Age", "Age.Range",  "Insurer", "Sale.Made", "Employment.Status", "Type.of.driving.licence", "Drivers.to.be.insured.", "Voluntary.excess.", "What.type.of.cover.would.you.like.", "How.many.years.no.claims.bonus..NCB..do.you.have.", "What.is.the.estimated.value.of.the.vehicle.", "Adjusted.Price.Range", "Post.Code.Prefix", "Postcode.Area", "Postcode.Region", "Have.you.had.any.unspent.non.motoring.criminal.convictions.", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.", "Proposer.Claims.Count", "Proposer.Convictions.Count", "SOURCE.USER.WEBSITES", "Quote.Date"))
  
  # data4 <- reactive({
  #   if(input$filterName2 == 0){return()}
  #   if(is.null(input$filterName2)) return(NULL)
  #   my_data4 <- data()
  #   z <- my_data4[,c(1:(which(colnames(my_data4)=="Price.Position.1")+2), which(colnames(my_data4)=="Selected.Provider.Price"):which(colnames(my_data4)=="Selected.Provider.Position"))]
  #   z1 <- count(z, c(input$filterName2, "Selected.Provider.Position"))
  #   ProviderInsurer <- dcast(z1, get(input$filterName2) ~ Selected.Provider.Position, value = "freq")
  #   ProviderInsurer[is.na(ProviderInsurer)] <- 0
  #   colnames(ProviderInsurer) <- paste("Position", colnames(ProviderInsurer), sep=" ")
  #   colnames(ProviderInsurer)[1] <- "Apricot Agg (OGI standalone)"
  #   ProviderInsurer
  # })
  
  data5 <- reactive({
    my_data5 <- data()
    #my_data5 <- my_data5[!is.na(my_data5$Apricot.Position),]
#    my_data5 <- subset(my_data5, Apricot.Position != "NA")
#    pos <- positives()
    pos <- nrow(my_data5[my_data5[,ncol(my_data5)] < 0, ])
    Length <- nrow(my_data5)
    UploadColumnNumbers <- ncol(my_data5)
    Position1 <- which( colnames(my_data5)=="Price.Position.1")

    #my_data5[, UploadColumnNumbers+1] <- (as.numeric(my_data5[, (UploadColumnNumbers-2)])+as.numeric(input$integer))*(1+((as.numeric(input$percentage))/100))
    if(pos > 0){
   my_data5[1:pos, UploadColumnNumbers+1] <- as.numeric(my_data5[1:pos, UploadColumnNumbers-1]) - as.numeric(my_data5[1:pos,Position1])
   my_data5[1:pos, UploadColumnNumbers+2] <- as.numeric(my_data5[1:pos, UploadColumnNumbers-1]) - as.numeric(my_data5[1:pos,(Position1+3)])
   my_data5[1:pos, UploadColumnNumbers+3] <- as.numeric(my_data5[1:pos, UploadColumnNumbers-1]) - as.numeric(my_data5[1:pos,(Position1+6)])
    }
   if(pos < Length){
     my_data5[(pos+1):Length, UploadColumnNumbers+1] <- as.numeric(my_data5[(pos+1):Length, UploadColumnNumbers-1]) - as.numeric(my_data5[(pos+1):Length,(Position1+3)])
     my_data5[(pos+1):Length, UploadColumnNumbers+2] <- as.numeric(my_data5[(pos+1):Length, UploadColumnNumbers-1]) - as.numeric(my_data5[(pos+1):Length,(Position1+6)]) 
     my_data5[(pos+1):Length, UploadColumnNumbers+3] <- as.numeric(my_data5[(pos+1):Length, UploadColumnNumbers-1]) - as.numeric(my_data5[(pos+1):Length,(Position1+9)])
   }
    Top3s <- data.frame(matrix(NA, nrow = 2, ncol = 4))
    Top3s[1,1] <- "Recorded Quotes"
    #Top3s[1,1] <- "Recorded"
    Top3s[2,1] <- paste0(input$integer, "(£) price adjustment and ", input$percentage, "% adjustment")
    Top3s[1,2] <- sum(my_data5$Site.Name.1 == "Apricot Agg (OGI standalone)", NA, na.rm = TRUE)
    Top3s[1,3] <- sum(my_data5$Site.Name.2 == "Apricot Agg (OGI standalone)", NA, na.rm = TRUE)
    Top3s[1,4] <- sum(my_data5$Site.Name.3 == "Apricot Agg (OGI standalone)", NA, na.rm = TRUE)
    Top3s[2,2] <- nrow(my_data5[my_data5[, UploadColumnNumbers+1]<0,])
    Top3s[2,3] <- nrow(my_data5[my_data5[, UploadColumnNumbers+2]<0,]) - nrow(my_data5[my_data5[, UploadColumnNumbers+1]<0,])
    Top3s[2,4] <- nrow(my_data5[my_data5[, UploadColumnNumbers+3]<0,]) - nrow(my_data5[my_data5[, UploadColumnNumbers+2]<0,])
    colnames(Top3s) <- c("", "Position 1", "Position 2", "Position 3")
    Top3s
  })
  
  my_data6 <- reactive({
      data6 <- data()
      data6 <- subset(data6[,c(1:(which(colnames(data6)=="Price.Position.1")), ncol(data6)-2)], data6$Apricot.Position <4 & data6$Latitude != "")
      data6
  })
  
  labels=c("Pos. 1","Pos. 2","Pos. 3")
  colors<-c(rgb(253,117,103,maxColorValue=256)
            ,rgb(255, 153, 0,maxColorValue=256)
            ,rgb(253,245,105,maxColorValue=256))
  
  data7 <- reactive({
    my_data7 <- data()
#    my_data7$Adjusted.Price.Range <- 10
    bspot <- which(names(my_data7)=="Price.Position.1")-3
    UploadColumnNumbers <- ncol(my_data7)
    Length <- nrow(my_data7)
    pos <- nrow(my_data7[my_data7[,ncol(my_data7)] < 0, ])
    #my_data7[, UploadColumnNumbers+1] <- (as.numeric(my_data7[, (UploadColumnNumbers-2)])+as.numeric(input$integer))*(1+((as.numeric(input$percentage))/100))
    my_data7 <- data.frame(my_data7[1:(bspot-1)], Adjusted.Price.Range = cut(my_data7[, UploadColumnNumbers-1],
                                                                             breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000,Inf), 
                                                                             labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000+"), 
                                                                         right = FALSE), my_data7[(bspot):ncol(my_data7)])
   UploadColumnNumbers <- UploadColumnNumbers+1
    Position1 <- which( colnames(my_data7)=="Price.Position.1")
    Position2 <- which( colnames(my_data7)=="Price.Position.2")
    
    if(pos > 0){
      my_data7[1:pos, UploadColumnNumbers+1] <- as.numeric(my_data7[1:pos, UploadColumnNumbers-1]) - as.numeric(my_data7[1:pos,Position1])
      my_data7[1:pos, UploadColumnNumbers+2] <- ifelse(my_data7[1:pos, UploadColumnNumbers+1] <= 0, 1, 0)
    }
    if(pos < Length){
      my_data7[pos+1:Length, UploadColumnNumbers+1] <- as.numeric(my_data7[pos+1:Length, UploadColumnNumbers-1]) - as.numeric(my_data7[pos+1:Length,Position2])
      my_data7[pos+1:Length, UploadColumnNumbers+2] <- ifelse(my_data7[pos+1:Length, UploadColumnNumbers+1] < 0, 1, 0)
    }
    colnames(my_data7)[UploadColumnNumbers+2] <- "Adjusted.Position.1"
    filterSummary <- count(my_data7, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
    filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
    filterSummary3 <- count(filterSummary3, input$filterName)
    filterSummary <- filterSummary[ which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"),]
    filterSummary2 <- count(filterSummary, input$filterName)
    filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
    colnames(filterSummary4)[2] <- "Position1"
    colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment and ", input$percentage, "% adjustment")
    filterSummary4[filterSummary4=="NA"]<-0
    filterSummary4
    
  })
  
  data8 <- reactive({
    my_data8 <- data()
    UploadColumnNumbers <- ncol(my_data8)
    my_data8$Price.Position.2 <- gsub("-", Inf, my_data8$Price.Position.2)
    my_data8$Price.Position.3 <- gsub("-", Inf, my_data8$Price.Position.3)
    for(j in 1:nrow(my_data8)){	
      if(my_data8[j, UploadColumnNumbers-1] - as.numeric(my_data8$Price.Position.1[j]) <= 0){
        my_data8[j, UploadColumnNumbers+1] = 1
      } else if (my_data8[j, UploadColumnNumbers-1] - as.numeric(my_data8$Price.Position.2[j]) <= 0){
        my_data8[j, UploadColumnNumbers+1] = 2
      } else if (my_data8[j, UploadColumnNumbers-1] - as.numeric(my_data8$Price.Position.3[j]) <= 0){
        my_data8[j, UploadColumnNumbers+1] = 3
      } else my_data8[j, UploadColumnNumbers+1] = "4+"
    }
    
    colnames(my_data8)[ncol(my_data8)] <- "Adjusted.Position"
    z <- my_data8[,c(1:(which(colnames(my_data8)=="Price.Position.1")+2), which(colnames(my_data8)=="Selected.Provider.Price"):ncol(my_data8))]
    z1 <- count(z, c(input$filterName2, "Adjusted.Position"))
    ProviderGroup <- dcast(z1, get(input$filterName2) ~ Adjusted.Position, value = "freq")
    ProviderGroup[is.na(ProviderGroup)] <- 0
    colnames(ProviderGroup) <- paste("Position", colnames(ProviderGroup), sep=".")
    if(!("Position.1" %in% colnames(ProviderGroup)))
    {
      ProviderGroup$Position.1 <- 0
      ProviderGroup <- subset(ProviderGroup, select=c(1, 5, 2, 3, 4))
    }
    ProviderGroup
  })
  
  data10 <- reactive({
    my_data10 <- data()
    my_data10 <- my_data10[my_data10$SYSTEM.NAME =="car",]
    my_data10 <- my_data10[!is.na(my_data10$BTXPolref),]
    #my_data10 <- subset(my_data10, my_data10$Sale.Made != "No")
    #    my_data10$Adjusted.Price.Range <- 10
    bspot <- which(names(my_data10)=="Price.Position.1")-3
    Length <- nrow(my_data10)
    UploadColumnNumbers <- ncol(my_data10)
    pos <- nrow(my_data10[my_data10[,ncol(my_data10)] < 0, ])
    my_data10 <- data.frame(my_data10[1:(bspot-1)], Adjusted.Price.Range = cut(my_data10[, UploadColumnNumbers-1],
                                                                               breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000,Inf), 
                                                                               labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000+"), 
                                                                             right = FALSE), my_data10[(bspot):ncol(my_data10)])
    UploadColumnNumbers <- UploadColumnNumbers+1
    Position1 <- which( colnames(my_data10)=="Price.Position.1")
    Position2 <- which( colnames(my_data10)=="Price.Position.2")
    
    if(pos > 0){
      my_data10[1:pos, UploadColumnNumbers+1] <- as.numeric(my_data10[1:pos, UploadColumnNumbers-1]) - as.numeric(my_data10[1:pos,Position1])
      my_data10[1:pos, UploadColumnNumbers+2] <- ifelse(my_data10[1:pos, UploadColumnNumbers+1] <= 0, 1, 0)
    }
    if(pos < Length){
      my_data10[pos+1:Length, UploadColumnNumbers+1] <- as.numeric(my_data10[pos+1:Length, UploadColumnNumbers-1]) - as.numeric(my_data10[pos+1:Length,Position2])
      my_data10[pos+1:Length, UploadColumnNumbers+2] <- ifelse(my_data10[pos+1:Length, UploadColumnNumbers+1] < 0, 1, 0)
    }
    my_data10$TotalValueAdjusted <- my_data10$TotalValue + my_data10$V124
    my_data10 <- my_data10[order(my_data10$Apricot.Position, my_data10$Selected.Provider.Price), ] 
    my_data10 <- my_data10[!duplicated(my_data10$BTXPolref),]

    colnames(my_data10)[UploadColumnNumbers+2] <- "Adjusted.Position.1"
    filterSummary <- count(my_data10, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
    filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
    filterSummary3 <- count(filterSummary3, input$filterName)
    filterSummary <- filterSummary[ which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"),]
    filterSummary2 <- count(filterSummary, input$filterName)
    filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
    colnames(filterSummary4)[2] <- "Position1"
    colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment and ", input$percentage, "% adjustment")
    filterSummary4[is.na(filterSummary4)] <- 0
    filterSummary5 <-aggregate(my_data10$TotalValue, by=list(my_data10[[input$filterName]]), FUN=sum, na.rm=TRUE)
    filterSummary6 <-aggregate(my_data10$TotalValueAdjusted, by=list(my_data10[[input$filterName]]), FUN=sum, na.rm=TRUE)
    filterSummary7 <- merge(x= filterSummary5, y= filterSummary6, by= 'Group.1', all.x= T)
    colnames(filterSummary7)[1] <- input$filterName
    colnames(filterSummary7)[2] <- "Total Value"
    colnames(filterSummary7)[3] <- "Total Value Adjusted"
    filterSummary7[2] <- round(filterSummary7[2],2)
    filterSummary8 <- merge(x= filterSummary7, y= filterSummary4, by= input$filterName, all.x= T)
    filterSummary8
  })
  
  ##Top 1s tab
  data11 <- reactive({
    my_data11 <- subset(my_data3, my_data3$Site.Name.1 == "Apricot Agg (OGI standalone)" & my_data3$Site.Name.2 != "-")
    my_data11$Differnce.To.2nd <- as.numeric(my_data11$Price.Position.2) - as.numeric(my_data11$Price.Position.1)
    my_data11$Price.Difference.Range = cut(my_data11$Differnce.To.2nd, breaks = c(-Inf, 10, 20, 50, 80, 100, 200, 500, 800, 1000, 2000, Inf), labels = c("Less than £10", "£10-£20", "£20-£50", "£50-£80", "£80-100", "£100-£200", "£200-£500", "£500-£800", "£800-1000", "£1000-£2000", "Over £2000"), right = FALSE)
    my_data11$Price.Range = cut(as.numeric(my_data11$Price.Position.1), breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1700, 2000, 2500, 3000, 5000, 10000, Inf),
                                                                      labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-500", "£500-£600", "£600-£700", "£700-£800", "£800-900", "£900-£1000", "£1000-£1100", "£1100-£1200", "£1200-£1300", "£1300-£1400", "£1400-£1500", "£1500-£1700", "£1700-£2000", "£2000-£2500", "£2500-£3000", "£3000-£5000", "£5000-£10000", "Over £10,000"), 
                                                                      right = FALSE)
    if(length(input$dataset) > 0){
    if(length(input$dataset) == 1){
    Summary <- aggregate(as.numeric(my_data11$Differnce.To.2nd), by=list(Category=my_data11[[input$dataset[1]]]), FUN=length)
    Summary2 <- aggregate(as.numeric(my_data11$Differnce.To.2nd), by=list(Category=my_data11[[input$dataset[1]]]), FUN=sum)
    Summary3 <- aggregate(as.numeric(my_data11$Differnce.To.2nd), by=list(Category=my_data11[[input$dataset[1]]]), FUN=max)
    Summary <- merge(Summary, Summary2,by="Category")
    Summary <- merge(Summary, Summary3,by="Category")
    colnames(Summary )[2:4] <- c("Position1s", "Sum.of.Difference", "Maximum")
    Summary$Average.Difference.To.2nd <- round(Summary$Sum.of.Difference/Summary$Position1s,digits=2)
    }
    else if(length(input$dataset) == 2) {
      Summary <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]], my_data11, FUN=length)
      Summary2 <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]], my_data11, FUN=sum)
      Summary3 <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]], my_data11, FUN=max)
      colnames(Summary)[1:2] <- c(input$dataset[1], input$dataset[2])
      colnames(Summary2)[1:2] <- c(input$dataset[1], input$dataset[2])
      colnames(Summary3)[1:2] <- c(input$dataset[1], input$dataset[2])
      Summary <- merge(Summary, Summary2,by=c(input$dataset[1], input$dataset[2]))
      Summary <- merge(Summary, Summary3,by=c(input$dataset[1], input$dataset[2]))
      colnames(Summary)[3:5] <- c("Position1s", "Sum.of.Difference", "Mamimum")
      Summary$Average.Difference.To.2nd <- round(Summary$Sum.of.Difference/Summary$Position1s,digits=2)
    }
    else if(length(input$dataset) == 3) {
      Summary <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]]+ my_data11[[input$dataset[3]]], my_data11, FUN=length)
      Summary2 <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]]+ my_data11[[input$dataset[3]]], my_data11, FUN=sum)
      Summary3 <- aggregate(as.numeric(my_data11$Differnce.To.2nd) ~ my_data11[[input$dataset[1]]] + my_data11[[input$dataset[2]]]+ my_data11[[input$dataset[3]]], my_data11, FUN=max)
      colnames(Summary)[1:3] <- c(input$dataset[1], input$dataset[2], input$dataset[3])
      colnames(Summary2)[1:3] <- c(input$dataset[1], input$dataset[2], input$dataset[3])
      colnames(Summary3)[1:3] <- c(input$dataset[1], input$dataset[2], input$dataset[3])
      Summary <- merge(Summary, Summary2,by=c(input$dataset[1], input$dataset[2], input$dataset[3]))
      Summary <- merge(Summary, Summary3,by=c(input$dataset[1], input$dataset[2], input$dataset[3]))
      colnames(Summary)[4:6] <- c("Position1s", "Sum.of.Difference", "Maximum")
      Summary$Average.Difference.To.2nd <- round(Summary$Sum.of.Difference/Summary$Position1s,digits=2)
    }
    Summary <- Summary[order(-Summary$Average.Difference.To.2nd),]
    Summary
    }
  })
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE)
 
  # output$downloadData4 <- downloadHandler(
  #   filename = function() { 'RankData.csv' }, content = function(file) {
  #     write.csv(data4(), file, row.names = FALSE)
  #   }
  # )
  
  output$downloadData8 <- downloadHandler(
    filename = function() { 'ProjectedRankData.csv' }, content = function(file) {
      write.csv(data8(), file, row.names = FALSE)
    }
  )
  
  # output$main_plot1 <- renderPlot({
  #   counts <- t(data4()[,1:4][-1])
  #   colnames(counts) <- data4()[, 1]
  #   barplot(counts,
  #           main= paste0("Quote Results Position by ", input$filterName2),
  #           ylab="Position 1s",
  #           ylim=c(0,max(apply(data8()[,2:4], 2, function(x) max(x, na.rm = TRUE)))),
  #           xlab=input$filterName2,
  #           legend=c("Position 1", "Position 2", "Position 3"),
  #           col=heat.colors(3),
  #           cex.names=0.7,
  #           beside=TRUE)
  # })
  
  # output$main_plot2 <- renderPlot({
  #   
  #   counts <- t(data8()[,1:4][-1])
  #   colnames(counts) <- data8()[, 1]
  #   barplot(counts,
  #           main= paste0("Quote Results Position by ", input$filterName2, " with Price Adjustment and ", input$percentage, "% adjustment"),
  #           ylab="Position 1s",
  #           ylim=c(0,max(apply(data8()[,2:4], 2, function(x) max(x, na.rm = TRUE)))),
  #           xlab=input$filterName2,
  #           legend=c("Position 1", "Position 2", "Position 3"),
  #           col=heat.colors(3),
  #           cex.names=0.7,
  #           beside=TRUE)
  # })
  
  output$selectUI <- renderUI({ 
    selectInput("filterName", "Select Filter Criteria", filterList1)
  })
  output$selectUI2 <- renderUI({ 
    selectInput("filterName2", "Select Filter Criteria", filterList2)
  })
  
  output$my_output_data5 <- renderDataTable({data5()}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
  output$my_output_data7 <- renderDataTable({data7()})
  output$my_output_data10 <- renderDataTable({data10()})
  output$my_output_data11 <- renderDataTable({data11()})
  
  output$mymap <- renderLeaflet({
    data <- my_data6()
    leafIcons <- icons(
      iconUrl = ifelse(my_data6()$Apricot.Position < 2,
                       "red.png", ifelse(my_data6()$Apricot.Position < 3, "orange.png", "yellow.png")
      ),
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 19, iconAnchorY = 38
    )
    leaflet(data = my_data6()) %>%
      setView(lng = -5, lat = 53.5, zoom = 6) %>%
      addTiles() %>%
#      addProviderTiles("Esri.WorldImagery")  %>%
      addMarkers(~Longitude, ~Latitude, icon = leafIcons, popup = ~as.character(QUOTE.REFERENCE))%>%
      addLegend("topright", colors = colors, labels =labels ,
              title = "Position",
              opacity = 1
    )
  })
}
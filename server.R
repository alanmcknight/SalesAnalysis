options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)

server = function(input, output, session) {
  
    my_data3 <- read.csv("QuotesJuly16.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    
    data <- reactive({
      my_data <- subset(my_data3, my_data3$Insurer == input$filterName1)
      my_data
    })
    
    filterList1 <- colnames(my_data3[((which(colnames(my_data3)=="Price.Position.1"))-11):((which(colnames(my_data3)=="Price.Position.1"))-1)])
    filterList2 <- sort(c(filterList1,  "Insurer", "X.Sale.Made", "Employment.Status", "Type.of.driving.licence", "Drivers.to.be.insured.", "Voluntary.excess.", "What.type.of.cover.would.you.like.", "How.many.years.no.claims.bonus..NCB..do.you.have.", "What.is.the.estimated.value.of.the.vehicle.", "Post.Code.Prefix", "Have.you.had.any.motoring.convictions..including.fixed.penalty.endorsements...or.anything.pending..in.the.last.5.years.", "Have.you.had.any.unspent.non.motoring.criminal.convictions."))
    filterList1 <- sort(c(filterList1,  "Insurer", "X.Sale.Made", "Employment.Status", "Type.of.driving.licence", "Drivers.to.be.insured.", "Voluntary.excess.", "What.type.of.cover.would.you.like.", "How.many.years.no.claims.bonus..NCB..do.you.have.", "What.is.the.estimated.value.of.the.vehicle.", "Adjusted.Price.Range", "Post.Code.Prefix", "Have.you.had.any.motoring.convictions..including.fixed.penalty.endorsements...or.anything.pending..in.the.last.5.years.", "Have.you.had.any.unspent.non.motoring.criminal.convictions."))
  
  data4 <- reactive({
    if(input$filterName2 == 0){return()}
    if(is.null(input$filterName2)) return(NULL)
    my_data4 <- data()
    z <- my_data4[,c(1:(which(colnames(my_data4)=="Price.Position.1")+2), which(colnames(my_data4)=="Selected.Provider.Price"):which(colnames(my_data4)=="Selected.Provider.Position"))]
    z1 <- count(z, c(input$filterName2, "Selected.Provider.Position"))
    ProviderInsurer <- dcast(z1, get(input$filterName2) ~ Selected.Provider.Position, value = "freq")
    ProviderInsurer[is.na(ProviderInsurer)] <- 0
    colnames(ProviderInsurer) <- paste("Position", colnames(ProviderInsurer), sep=" ")
    colnames(ProviderInsurer)[1] <- "Apricot Agg (OGI standalone)"
    ProviderInsurer
  })
  
  data5 <- reactive({
    my_data5 <- data()
    UploadColumnNumbers <- ncol(my_data5)
   Position1 <- which( colnames(my_data5)=="Price.Position.1")

    my_data5[, UploadColumnNumbers+1] <- as.numeric(my_data5[, (UploadColumnNumbers-2)])+as.numeric(input$integer)
    if(as.numeric(input$integer<=0)){
      my_data5[, UploadColumnNumbers+2] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,Position1])
      my_data5[, UploadColumnNumbers+3] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+3)])
      my_data5[, UploadColumnNumbers+4] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+6)])
    }else {
      my_data5[, UploadColumnNumbers+2] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,Position1+3])
      my_data5[, UploadColumnNumbers+3] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+6)])
      my_data5[, UploadColumnNumbers+4] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+9)])
    }
    Top3s <- data.frame(matrix(NA, nrow = 2, ncol = 4))
    Top3s[1,1] <- "Recorded"
    Top3s[2,1] <- paste0(input$integer, "(£) price adjustment")
    Top3s[1,2] <- sum(my_data5$Site.Name.1 == "Apricot Agg (OGI standalone)")
    Top3s[1,3] <- sum(my_data5$Site.Name.2 == "Apricot Agg (OGI standalone)")
    Top3s[1,4] <- sum(my_data5$Site.Name.3 == "Apricot Agg (OGI standalone)")
    Top3s[2,2] <- nrow(my_data5[my_data5[, UploadColumnNumbers+2]<=0,])
    Top3s[2,3] <- nrow(my_data5[my_data5[, UploadColumnNumbers+3]<=0,]) - nrow(my_data5[my_data5[, UploadColumnNumbers+2]<=0,])
    Top3s[2,4] <- nrow(my_data5[my_data5[, UploadColumnNumbers+4]<=0,]) - nrow(my_data5[my_data5[, UploadColumnNumbers+3]<=0,])
    colnames(Top3s) <- c("", "Position 1", "Position 2", "Position 3")
    Top3s
  })
  
  my_data6 <- reactive({
      data6 <- data()
      data6 <- subset(data6[,c(1:(which(colnames(data6)=="Site.Name.1")), ncol(data6))], Selected.Provider.Position <4)
      data6
  })
  
  labels=c("Position 1","Position 2","Position 3")
  colors<-c(rgb(253,117,103,maxColorValue=256)
            ,rgb(255, 153, 0,maxColorValue=256)
            ,rgb(253,245,105,maxColorValue=256))
  
  data7 <- reactive({
    my_data7 <- data()
#    my_data7$Adjusted.Price.Range <- 10
    bspot <- which(names(my_data7)=="Price.Position.1")-3
    UploadColumnNumbers <- ncol(my_data7)
    my_data7[, UploadColumnNumbers+1] <- as.numeric(my_data7[, (UploadColumnNumbers-2)])+as.numeric(input$integer)
my_data7 <- data.frame(my_data7[1:(bspot-1)], Adjusted.Price.Range = cut(my_data7[, UploadColumnNumbers+1],
                                                                         breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, Inf),
                                                                         labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-500", "£500-£600", "£600-£700", "£700-£800", "£800-900", "£900-£1000", "£1000-£1100", "£1100-£1200", "Over £1200"),
                                                                         right = FALSE), my_data7[(bspot):ncol(my_data7)])
   UploadColumnNumbers <- UploadColumnNumbers+1
    Position1 <- which( colnames(my_data7)=="Price.Position.1")
    Position2 <- which( colnames(my_data7)=="Price.Position.2")
    if(input$integer <= 0){
      my_data7[, UploadColumnNumbers+2] <- as.numeric(my_data7[, UploadColumnNumbers+1]) - as.numeric(my_data7[,Position1])
      my_data7[, UploadColumnNumbers+3] <- ifelse(my_data7[, UploadColumnNumbers+2] <= 0, 1, 0)
      colnames(my_data7)[UploadColumnNumbers+3] <- "Adjusted.Position.1"
      filterSummary <- count(my_data7, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
      filterSummary <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- count(filterSummary3, input$filterName)
      filterSummary <- filterSummary[ which(filterSummary$Site.Name.1 == "Apricot Agg (OGI standalone)"),]
      filterSummary2 <- count(filterSummary, input$filterName)
      filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
      colnames(filterSummary4)[2] <- "Position1"
      colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment")
      filterSummary4[filterSummary4=="NA"]<-0
      filterSummary4
    } else {
      my_data7[, UploadColumnNumbers+2] <- as.numeric(my_data7[, UploadColumnNumbers+1]) - as.numeric(my_data7[,Position2])
      my_data7[, UploadColumnNumbers+3] <- ifelse(my_data7[, UploadColumnNumbers+2] <= 0, 1, 0)
      colnames(my_data7)[UploadColumnNumbers+3] <- "Adjusted.Position.1"
      filterSummary <- count(my_data7, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
      filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- count(filterSummary3, input$filterName)
      filterSummary <- filterSummary[ which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"),]
      filterSummary2 <- count(filterSummary, input$filterName)
      filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
      colnames(filterSummary4)[2] <- "Position1"
      colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment")
      filterSummary4[filterSummary4=="NA"]<-0
      filterSummary4
    }
    
  })
  
  data8 <- reactive({
    my_data8 <- data()
    UploadColumnNumbers <- ncol(my_data8)
    for(j in 1:nrow(my_data8)){	
      if((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - as.numeric(my_data8$Price.Position.1[j]) <= 0){
        my_data8[j, UploadColumnNumbers+1] = 1
      } else if ((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - as.numeric(my_data8$Price.Position.2[j]) < 0){
        my_data8[j, UploadColumnNumbers+1] = 2
      } else if ((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - as.numeric(my_data8$Price.Position.3[j]) < 0){
        my_data8[j, UploadColumnNumbers+1] = 3
      } else my_data8[j, UploadColumnNumbers+1] = 4
    }
    
    colnames(my_data8)[ncol(my_data8)] <- "Adjusted.Position"
    z <- my_data8[,c(1:(which(colnames(my_data8)=="Price.Position.1")+2), which(colnames(my_data8)=="Selected.Provider.Price"):ncol(my_data8))]
    z1 <- count(z, c(input$filterName2, "Adjusted.Position"))
    ProviderGroup <- dcast(z1, get(input$filterName2) ~ Adjusted.Position, value = "freq")
    ProviderGroup[is.na(ProviderGroup)] <- 0
    colnames(ProviderGroup) <- paste("Position", colnames(ProviderGroup), sep=" ")
    colnames(ProviderGroup)[1] <- "Apricot Agg (OGI standalone)"
    colnames(ProviderGroup)[5] <- "Position 4 +"
    ProviderGroup
  })
  
  data10 <- reactive({
    my_data10 <- data()
    my_data10 <- subset(my_data10, my_data10$X.Sale.Made == "Yes")
    #    my_data10$Adjusted.Price.Range <- 10
    bspot <- which(names(my_data10)=="Price.Position.1")-3
    UploadColumnNumbers <- ncol(my_data10)
    my_data10[, UploadColumnNumbers+1] <- as.numeric(my_data10[, (UploadColumnNumbers-2)])+as.numeric(input$integer)
    my_data10 <- data.frame(my_data10[1:(bspot-1)], Adjusted.Price.Range = cut(my_data10[, UploadColumnNumbers+1],
                                                                             breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, Inf),
                                                                             labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-500", "£500-£600", "£600-£700", "£700-£800", "£800-900", "£900-£1000", "£1000-£1100", "£1100-£1200", "Over £1200"),
                                                                             right = FALSE), my_data10[(bspot):ncol(my_data10)])
    UploadColumnNumbers <- UploadColumnNumbers+1
    Position1 <- which( colnames(my_data10)=="Price.Position.1")
    Position2 <- which( colnames(my_data10)=="Price.Position.2")
    if(input$integer <= 0){
      my_data10[, UploadColumnNumbers+2] <- as.numeric(my_data10[, UploadColumnNumbers+1]) - as.numeric(my_data10[,Position1])
      my_data10[, UploadColumnNumbers+3] <- ifelse(my_data10[, UploadColumnNumbers+2] <= 0, 1, 0)
      colnames(my_data10)[UploadColumnNumbers+3] <- "Adjusted.Position.1"
      filterSummary <- count(my_data10, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
      filterSummary <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- count(filterSummary3, input$filterName)
      filterSummary <- filterSummary[ which(filterSummary$Site.Name.1 == "Apricot Agg (OGI standalone)"),]
      filterSummary2 <- count(filterSummary, input$filterName)
      filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
      colnames(filterSummary4)[2] <- "Position1"
      colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment")
      filterSummary4[filterSummary4=="NA"]<-0
      filterSummary4
    } else {
      my_data10[, UploadColumnNumbers+2] <- as.numeric(my_data10[, UploadColumnNumbers+1]) - as.numeric(my_data10[,Position2])
      my_data10[, UploadColumnNumbers+3] <- ifelse(my_data10[, UploadColumnNumbers+2] <= 0, 1, 0)
      colnames(my_data10)[UploadColumnNumbers+3] <- "Adjusted.Position.1"
      filterSummary <- count(my_data10, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
      filterSummary3 <- filterSummary[ which(filterSummary$Adjusted.Position.1 == 1),]
      filterSummary3 <- count(filterSummary3, input$filterName)
      filterSummary <- filterSummary[ which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"),]
      filterSummary2 <- count(filterSummary, input$filterName)
      filterSummary4 <- merge(filterSummary2,filterSummary3,by=input$filterName, all=TRUE)
      colnames(filterSummary4)[2] <- "Position1"
      colnames(filterSummary4)[3] <-paste0( "Position1: ", input$integer, "(£) price adjustment")
      filterSummary4[filterSummary4=="NA"]<-0
      filterSummary4
    }
    
  })
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE)
 
  output$downloadData4 <- downloadHandler(
    filename = function() { 'RankData.csv' }, content = function(file) {
      write.csv(data4(), file, row.names = FALSE)
    }
  )
  
  output$downloadData8 <- downloadHandler(
    filename = function() { 'ProjectedRankData.csv' }, content = function(file) {
      write.csv(data8(), file, row.names = FALSE)
    }
  )
  
  output$main_plot1 <- renderPlot({
    counts <- t(data4()[,1:4][-1])
    colnames(counts) <- data4()[, 1]
    barplot(counts,
            main= paste0("Quote Results Position by ", input$filterName2),
            ylab="Position 1s",
            ylim=c(0,max(apply(data8()[,2:4], 2, function(x) max(x, na.rm = TRUE)))),
            xlab=input$filterName2,
            legend=c("Position 1", "Position 2", "Position 3"),
            col=heat.colors(3),
            cex.names=0.7,
            beside=TRUE)
  })
  
  output$main_plot2 <- renderPlot({
    
    counts <- t(data8()[,1:4][-1])
    colnames(counts) <- data8()[, 1]
    barplot(counts,
            main= paste0("Quote Results Position by ", input$filterName2, " with Price Adjustment"),
            ylab="Position 1s",
            ylim=c(0,max(apply(data8()[,2:4], 2, function(x) max(x, na.rm = TRUE)))),
            xlab=input$filterName2,
            legend=c("Position 1", "Position 2", "Position 3"),
            col=heat.colors(3),
            cex.names=0.7,
            beside=TRUE)
  })
  
  output$selectUI <- renderUI({ 
    selectInput("filterName", "Select Filter Criteria", filterList1)
  })
  output$selectUI2 <- renderUI({ 
    selectInput("filterName2", "Select Filter Criteria", filterList2)
  })
  
  output$my_output_data5 <- renderDataTable({data5()}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
  output$my_output_data7 <- renderDataTable({data7()})
  output$my_output_data10 <- renderDataTable({data10()})
  
  output$mymap <- renderLeaflet({
    leafIcons <- icons(
      iconUrl = ifelse(my_data6()$Selected.Provider.Position < 2,
                       "red.png", ifelse(my_data6()$Selected.Provider.Position < 3, "orange.png", "yellow.png")
      ),
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 19, iconAnchorY = 38
    )
    leaflet(data = my_data6()) %>%
      setView(lng = -5, lat = 53.5, zoom = 6) %>%
      addTiles() %>%
#      addProviderTiles("Esri.WorldImagery")  %>%
      addMarkers(~Longitude, ~Latitude, icon = leafIcons, popup = ~as.character(Quote.Reference))%>%
      addLegend("topright", colors = colors, labels =labels ,
              title = "Position",
              opacity = 1
    )
  })
}
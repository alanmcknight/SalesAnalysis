options(shiny.maxRequestSize=50*1024^2)
library(plyr)
library(reshape2)
library(leaflet)

server = function(input, output, session) {
  
    my_data3 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    my_data2 <- subset(my_data3[,c(1, which(colnames(my_data3)=="Price.Position.1"):ncol(my_data3))], Site.Name.1 == "Apricot Agg (OGI standalone)")
    my_data2 <- my_data2[,1:7]
    my_data2$Price.To.2nd <- as.numeric(my_data2[,5])- as.numeric(my_data2[,2])
    my_data2 <- my_data2[order(-my_data2$Price.To.2nd),]
    colnames(my_data2)[1] <- "ID"
    my_data2
  
  data4 <- reactive({

    my_data4 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")

    z <- my_data4[,c(1:(which(colnames(my_data4)=="Price.Position.1")+2), which(colnames(my_data4)=="Selected.Provider.Price"):which(colnames(my_data4)=="Selected.Provider.Position"))]
    z1 <- count(z, c(input$filterName2, "Selected.Provider.Position"))
    ProviderInsurer <- dcast(z1, get(input$filterName2) ~ Selected.Provider.Position, value = "freq")
    ProviderInsurer[is.na(ProviderInsurer)] <- 0
    colnames(ProviderInsurer) <- paste("Position", colnames(ProviderInsurer), sep=" ")
    colnames(ProviderInsurer)[1] <- "Apricot Agg (OGI standalone)"
    ProviderInsurer
  })
  
  searchResult<- reactive({
    isolate({ 
      my_data7 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    })
    filterList <- colnames(my_data7[((which(colnames(my_data7)=="Price.Position.1"))-4):((which(colnames(my_data7)=="Price.Position.1"))-1)])
    filterList <- sort(c(filterList,  "Insurer"))
    filterList
  })
  
  searchResult2<- reactive({
    isolate({ 
      my_data7 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    })
    filterList <- colnames(my_data7[((which(colnames(my_data7)=="Price.Position.1"))-3):((which(colnames(my_data7)=="Price.Position.1"))-1)])
    filterList <- sort(c(filterList,  "Insurer"))
    filterList
  })
  
  data5 <- reactive({
    my_data5 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    UploadColumnNumbers <- ncol(my_data5)
   Position1 <- which( colnames(my_data5)=="Price.Position.1")

    my_data5[, UploadColumnNumbers+1] <- as.numeric(my_data5[, (UploadColumnNumbers-2)])+as.numeric(input$integer)
    my_data5[, UploadColumnNumbers+2] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,Position1])
    my_data5[, UploadColumnNumbers+3] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+3)])
    my_data5[, UploadColumnNumbers+4] <- as.numeric(my_data5[, UploadColumnNumbers+1]) - as.numeric(my_data5[,(Position1+6)])
    
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
  
  my_data6 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  my_data6 <- subset(my_data6[,c(1:(which(colnames(my_data6)=="Site.Name.1")), ncol(my_data6))], Selected.Provider.Position <4)
  my_data6
  
  labels=c("Position 1","Position 2","Position 3")
  colors<-c(rgb(253,117,103,maxColorValue=256)
            ,rgb(255, 153, 0,maxColorValue=256)
            ,rgb(253,245,105,maxColorValue=256))
  
  data7 <- reactive({
    my_data7 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    UploadColumnNumbers <- ncol(my_data7)
    Position1 <- which( colnames(my_data7)=="Price.Position.1")
    my_data7[, UploadColumnNumbers+1] <- as.numeric(my_data7[, (UploadColumnNumbers-2)])+as.numeric(input$integer)
    my_data7[, UploadColumnNumbers+2] <- as.numeric(my_data7[, UploadColumnNumbers+1]) - as.numeric(my_data7[,Position1])
    my_data7[, UploadColumnNumbers+3] <- ifelse(my_data7[, UploadColumnNumbers+2] <= 0, 1, 0)
    colnames(my_data7)[UploadColumnNumbers+3] <- "Adjusted.Position.1"
    filterSummary <- count(my_data7, c("Site.Name.1", c(input$filterName, "Adjusted.Position.1")))
    filterSummary <- filterSummary[ which(filterSummary$Adjusted.Position.1==1.00),]
    filterSummary2 <- count(filterSummary, input$filterName)
    filterSummary2[,3] <- format(filterSummary[which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"),4][ match(filterSummary2[,1], filterSummary[which(filterSummary$Site.Name.1=="Apricot Agg (OGI standalone)"), 2])], nsmall=0)
    filterSummary2[,2] <- format(filterSummary2[,2], nsmall=0)
    colnames(filterSummary2)[2] <-paste0( "Position1: ", input$integer, "(£) price adjustment")
    colnames(filterSummary2)[3] <- "Position1"
    filterSummary2 <- filterSummary2[c(1,3,2)]
    filterSummary2[filterSummary2=="NA"]<-0
    filterSummary2
  })
  
  data8 <- reactive({
    
    my_data8 <- read.csv("ApricotAnalysis.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
    for(j in 1:nrow(my_data8)){	
      if((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - my_data8$Price.Position.1[j] <= 0){
        my_data8[j, UploadColumnNumbers+1] = 1
      } else if ((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - my_data8$Price.Position.2[j] < 0){
        my_data8[j, UploadColumnNumbers+1] = 2
      } else if ((my_data8$Selected.Provider.Price[j] +as.numeric(input$integer2)) - my_data8$Price.Position.3[j] < 0){
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
  
  output$my_output_data3 <- renderTable({data3()},include.rownames=FALSE)
 
  output$downloadData4 <- downloadHandler(
    filename = function() { 'InsurerRankData.csv' }, content = function(file) {
      write.csv(data4(), file, row.names = FALSE)
    }
  )
  
  output$downloadData8 <- downloadHandler(
    filename = function() { 'InsurerRankData.csv' }, content = function(file) {
      write.csv(data8(), file, row.names = FALSE)
    }
  )
  
  output$main_plot <- renderPlot({
    
    hist(as.numeric(my_data2[,2]),
         probability = TRUE,
         breaks = 50,
         xlab = "Policy Price",
         main = "Policy Price (£)")
  })
  
  output$main_plot1 <- renderPlot({
    
    counts <- t(data4()[,1:4][-1])
    colnames(counts) <- data4()[, 1]
    barplot(counts,
            main= paste0("Quote Results Position by ", input$filterName2),
            ylab="Position 1s",
            xlab=input$filterName2,
            legend=c("Position 1", "Position 2", "Position 3"),
            col=heat.colors(3),
            cex.names=0.8,
            beside=TRUE)
  })
  
  output$main_plot2 <- renderPlot({
    
    counts <- t(data8()[,1:4][-1])
    colnames(counts) <- data8()[, 1]
    barplot(counts,
            main= paste0("Quote Results Position by ", input$filterName2, " with Price Adjustment"),
            ylab="Position 1s",
            xlab=input$filterName2,
            legend=c("Position 1", "Position 2", "Position 3"),
            col=heat.colors(3),
            cex.names=0.8,
            beside=TRUE)
  })
  
  output$selectUI <- renderUI({ 
    selectInput("filterName", "Select Filter Criteria", searchResult())
  })
  output$selectUI2 <- renderUI({ 
    selectInput("filterName2", "Select Filter Criteria", searchResult2())
  })
  
  output$my_output_data5 <- renderDataTable({data5()}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
  output$my_output_data7 <- renderDataTable({data7()})
  
  output$mymap <- renderLeaflet({
    leafIcons <- icons(
      iconUrl = ifelse(my_data6$Selected.Provider.Position < 2,
                       "red.png", ifelse(my_data6$Selected.Provider.Position < 3, "orange.png", "yellow.png")
      ),
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 19, iconAnchorY = 38
    )
    leaflet(data = my_data6) %>%
      setView(lng = -5, lat = 53.5, zoom = 7) %>%
      addTiles() %>%
#      addProviderTiles("Esri.WorldImagery")  %>%
      addMarkers(~Longitude, ~Latitude, icon = leafIcons, popup = ~as.character(Quote.Reference))%>%
      addLegend("topright", colors = colors, labels =labels ,
              title = "Position",
              opacity = 1
    )
  })
}
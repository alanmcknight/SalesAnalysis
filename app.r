library('devtools')
library('googleAuthR')
library('googleID')
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(stringr)
#library(googlesheets)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/plus.me")
options("googleAuthR.webapp.client_id" = "769713801246-qk2qhqpqt1k0g8rurm0jkomg73kggj1i.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "VTMwnOGWKame7JZPFlV4G7v0")
options(shiny.port = 1221)

#shiny::runApp( launch.browser=T, port=1221)
ui <- navbarPage(
  title = div(img(src="Apricot3.png"), "Apricot Dashboard"),
  #title = "Apricot Dashboard",
  windowTitle = "Apricot Dashboard",
  tabPanel("Sales Analysis",
           #img(src='Apricot.png', align = "right"),
  fluidRow(column(1, gar_auth_jsUI("auth_demo", login_text = "Log In")), column(2, p("Logged in as: ", textOutput("user_name")))
           ),
br(),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    dateRangeInput('dateRange',
                   label = 'Select Sale Date range',
                   start = "2017-06-01", end = Sys.Date()
    ),
    htmlOutput("selectUI2"),
    dataTableOutput(outputId ="my_output_data6"),
  fluidRow(column(4, selectizeInput("dataset3", "Select data breakdown criteria", choices = c("AddOnCount", "AddOnValue", "Age", "Age.Range", "AGREGATOR", "Apricot.Position", "BPYNotes2", "BTXCommamt", "BTXDatecreated", "BTXDtraised", "BTXExec","BTXInsurer", "BTXOrigdebt.Range", "BTXOrigdebt", "BTXPolref", "BTXPoltype", "BTXTrantype", "Cancellation", "CancellationDate", "Day.of.Month", "Day.of.Week", "Discount", "Do.you.normally.pay.for.your.insurance.monthly.", "Drivers.to.be.insured.", "ECWebref", "Email.Domain", "Employment.Status", "Executive", "FinanceValue", "Has.the.vehicle.been.modified.in.any.way.e.g..alloy.wheels..tow.bar.etcâ..", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "Have.you.had.any.accidents.or.losses..whether.you.have.claimed.or.not.and.regardless.of.blame..in.the.last.5.years.", "Have.you.had.any.motoring.convictions..including.fixed.penalty.endorsements...or.anything.pending..in.the.last.5.years.", "Hour.Of.Day", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.", "How.many.years.no.claims.bonus..NCB..do.you.have.",  "Is.the.vehicle.a.grey.or.parallel.import.", "Licence.Years", "Month", "MonthStartDate", "PaymentMethod", "Postcode.Area", "Post.Code.District", "Post.Code.Prefix", "Postcode.Region", "Price.Position.1", "Price.Position.2", "Price.Returned.Range", "Product", "Proposer.Claims.Count", "Proposer.Convictions.Count", "QAActivequotedate", "QAActivequotetime", "Quote.Date", "Quote.Reference", "Selected.Provider.Price", "Source","SOURCE.TYPE.y", "TrafficCost", "Type.of.driving.licence", "Vehicle.Value.Range", "Vehicle.Year.of.Manufacture", "Voluntary.excess.", "Week.of.Year",  "What.is.the.estimated.value.of.the.vehicle.", "What.type.of.cover.would.you.like.", "Year"),
                                    multiple = TRUE, options = list(maxItems = 3))), column(4, selectInput("plotFilter", "Select Performance Metric", choices = c("Total Profit", "Average Profit", "Cancellations", "Sales Cancellation Percentage", "Sales"), selected = "Sales"))),
  fluidRow(column(
    plotlyOutput("dailyPlot2"),
    selectizeInput("dataset4", "Table Summary Type", choices = c("Count", "Mean", "Sum", "% Uptake"), selected = "Sum"),
    dataTableOutput(outputId ="my_output_data8"), width = 12), width = 14),
  br(),
  downloadButton("downloadData", "Download Sales Report"),           
  br(),
  plotlyOutput("dailyPlot3"),
  br(),
  br()
  #verbatimTextOutput('session_info')
)),
tabPanel("Reports",
         dateRangeInput('dateRange1',
                        label = 'Sale Date range: ',
                        start = Sys.Date()-1, end = Sys.Date()-1),
         selectInput("reportSelect", "Select Report", choices = c("ALPS LE Report", "Call Connections Report", "Daily Report", "MIS Report", "Sales Report", "USwitch Report", "Pending Renewals", "Quotezone Report", "XS Cover Report"), selected = "Daily Report"),
         downloadButton("reportDownload", "Download Report"),
         downloadButton("report", "Sales Report"),
         uiOutput("newWindowContent", style = "display: none;"),
         tags$script(HTML("
      $(document).ready(function() {
        if(window.location.hash != '') {
          $('div:not(#newWindowContent)').hide();
          $('#newWindowContent').show();
          $('#newWindowContent').appendTo('body');
        }
      })
    ")),
         a(href = "http://secure.apricotinsurance.co.uk/app/broker/buyonline/report?media_type=text/htm", target = "_blank",
           actionButton("Buy Online Sales Report", "Buy Online Sales Report")
         ),
         br(),
         br(),
         dataTableOutput(outputId ="my_output_data2")
), 
tabPanel("Executive Performance",
         dateRangeInput('dateRange2',
                        label = 'Sale Date range: ',
                        start = Sys.Date()-7, end = Sys.Date()-1), 
         htmlOutput("selectUI3"),
         selectInput("plotFilter2", "Select Performance Metric", choices = c("Add-Ons", "Cancellations", "Sales", "Total Profit"), selected = "Sales"),
         plotlyOutput("dailyPlot10"),
         dataTableOutput(outputId ="my_output_data10")),
tabPanel(title=HTML("<li><a href='https://docs.google.com/spreadsheets/d/17UgYlEAt5rChM9_diD5833MtiBviLuTtfCKFC_gt_Ng/edit#gid=0' target='_blank'>Updates & Suggestions"))
)


server <- shinyServer(function(input, output, session) {
  access_token <- callModule(gar_auth_js, "auth_demo")
  
  ## to use in a shiny app:
  user_details <- reactive({
    validate(
      need(access_token(), "Please Log In")
    )
    with_shiny(get_user_info, shiny_access_token = access_token())
  })
  
  users <- c("104921241849775714986", "101460119938975360500", "101914728899788440454", "102124089845388212018", "106112761530234792467", "109094592148669236498")
  
  #my_data <- reactive({ if(user_details()$id %in% users){
  #   my_data <- read.csv("https://www.dropbox.com/s/t949ydw2al7esy5/ApricotSalesMasked.csv?dl=1", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  # }
  #   shiny::runApp( launch.browser=T, port=1221)
  # })
    
  my_data <- read.csv("https://www.dropbox.com/s/t949ydw2al7esy5/ApricotSalesMasked.csv?dl=1", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  my_data$BTXDatecreated <- as.Date( as.character(my_data$BTXDatecreated), "%Y-%m-%d")
  my_data$CancellationDate <- as.Date( as.character(my_data$CancellationDate), "%Y-%m-%d")
  
  AdData <- read.csv("stats.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  endorsements <- read.csv("https://www.dropbox.com/s/siqmo27yg0dbpc6/Endorsements.csv?dl=1", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
#  endorsements <- read.csv("Endorsements.csv", header = TRUE, stringsAsFactors =FALSE, fileEncoding="latin1")
  #endorsements$BTXDatecreated.x <- as.Date( as.character(endorsements$BTXDatecreated.x), "%d/%m/%Y")
  
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
  
  data3 <- reactive({
    my_data1 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange2[1] & my_data$BTXDatecreated <= input$dateRange2[2])
    if(input$filterName3 != "All"){
      my_data1 <- subset(my_data1, my_data1$Product == input$filterName3)
    }
    my_data1
  })
  
  ## Reporting ##
  data2 <- reactive({
    if(input$reportSelect[1] == "Sales Report"){
      my_data1 <- subset(my_data[, c(1:(which(colnames(my_data)=="Price.Position.1")), which(colnames(my_data)=="Price.Position.2"), which(colnames(my_data)=="Price.Position.3"), (ncol(my_data)-2):ncol(my_data))], my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2])
      my_data1}else if(input$reportSelect[1] == "USwitch Report"){
        Data1 <- AdData
        USwitchData <- my_data[grep("APRUS", my_data$ECWebref),]
        SalesData<- subset(USwitchData, USwitchData$BTXDatecreated >= input$dateRange1[1] & USwitchData$BTXDatecreated <= input$dateRange1[2])
        CancellationData <- subset(USwitchData, USwitchData$Cancellation != "N")
        CancellationData <- subset(CancellationData, as.Date(CancellationData$CancellationDate, "%Y-%m-%d") >= input$dateRange1[1] & as.Date(CancellationData$CancellationDate, "%Y-%m-%d") <= input$dateRange1[2])
        USwitchData <- rbind(SalesData, CancellationData)
        USwitchData <- USwitchData [!duplicated(USwitchData ), ]
        
        USwitchData$recordtype <- "Sale"
        USwitchData$salesmonth <- cut(as.Date(USwitchData$BTXDtraised), "month")
        USwitchData$brand <- "Apricot"
        USwitchData$surname <- "NA"
        
        USwitchData1 <- merge(USwitchData, Data1, by = "BTXPolref", all.x=TRUE)
        
        USwitchData1 <- USwitchData1[c("recordtype", "salesmonth", "brand", "BCMEmail.x", "BCMPcode.x", "BCMName", "surname", "BCMDob.x", "CFReg", "BTXDtraised.x", "ECWebref.x", "BTXPolref", "BTXPaymethod.x", "BTXOrigdebt.x", "BTXDatecreated.x", "Cancellation", "CancellationDate", "FinanceValue", "BTXInsurer.x")]
        
        USwitchData1$surname <- word(USwitchData1$BCMName, -1)
        USwitchData1$BCMName <- word(USwitchData1$BCMName, -2)
        
        colnames(USwitchData1) <- c("recordtype", "salesmonth", "brand", "emailaddress", "postcode", "firstname", "surname", "dob", "carregistrationnumber", "policystartdate", "policyquotereference",	"providerquotereference",	"purchasechannel",	"premium",	"policypurchasedate",	"cancellationreason",	"cancellationeffectivedate",	"purchasetype",	"insurerunderwritingpolicy")
        
        USwitchData1 <- USwitchData1[!duplicated(USwitchData1), ]
        #USwitchData1
        
        USwitchData1$cancellationreason[USwitchData1$cancellationreason == "N"] <- ""
        USwitchData1$cancellationeffectivedate <- as.character(USwitchData1$cancellationeffectivedate)
        USwitchData1$cancellationeffectivedate[USwitchData1$cancellationreason == ""] <- ""
        #USwitchData1$cancellationeffectivedate[USwitchData1$cancellationreason == "N"] <- ""
        
        USwitchData1$purchasechannel[USwitchData1$purchasechannel == "O"] <- "Online"
        USwitchData1$purchasechannel[USwitchData1$purchasechannel != "Online"] <- "Telephone"
        USwitchData1$cancellationreason[USwitchData1$cancellationreason != ""] <- "NTU"
         
        USwitchData1$purchasetype[USwitchData1$purchasetype != "0"] <- "Monthly"
        USwitchData1$purchasetype[USwitchData1$purchasetype == "0"] <- "Annual"
        USwitchData1
      }else if(input$reportSelect[1] == "Call Connections Report"){
        #my_data$BTXDatecreated <- as.Date( as.character(my_data$BTXDatecreated), "%d/%m/%Y")
        Data1 <- AdData
        CCData <- my_data[grep("APRCC", my_data$ECWebref),]
        SalesData<- subset(CCData, CCData$BTXDatecreated >= input$dateRange1[1] & CCData$BTXDatecreated <= input$dateRange1[2] & CCData$BTXTrantype == "New Business")
        CancellationData <- subset(CCData, CCData$Cancellation != "N" & CCData$BTXTrantype == "New Business")
        CancellationData <- subset(CancellationData, as.Date(CancellationData$CancellationDate, "%Y-%m-%d") >= input$dateRange1[1] & as.Date(CancellationData$Cancellation, "%Y-%m-%d") <= input$dateRange1[2])
        CCData <- rbind(SalesData, CancellationData)
        CCData <- CCData [!duplicated(CCData ), ]
        
        CCData$brand <- "Apricot"
        CCData$surname <- "NA"
        #        CCData$title <- "NA"
        
        CCData1 <- merge(CCData, Data1, by = "BTXPolref", all.x=TRUE)
        
        CCData1 <- CCData1[c( "BCMTitle", "BCMName", "surname", "BCMDob.x", "BCMPcode.x", "BCMEmail.x",  "CFReg", "BTXPolref", "ECWebref.x", "BTXDatecreated.x", "BTXDtraised.x",   "BTXOrigdebt.x", "brand", "Cancellation")]
        
        CCData1$BCMTitle <- word(CCData1$BCMTitle, 1)
        CCData1$surname <- word(CCData1$BCMName, -1)
        CCData1$BCMName <- word(CCData1$BCMName, -2)
        
        colnames(CCData1) <- c("Title",	"FirstName",	"Surname",	"DateOfBirth",	"PostCode",	"Email",	"CarReg",	"PartnerCustomerReference",	"PartnerQuoteReference",	"QuoteDate",	"PolicyInceptionDate",	"Premium",	"Brand", "Cancellation")
        
        CCData1 <- CCData1[!duplicated(CCData1), ]
        CCData1$Cancellation[CCData1$Cancellation == "N"] <- ""
        # CCData1$cancellationeffectivedate[CCData1$cancellationeffectivedate == "N"] <- ""
        # 
        # CCData1$purchasechannel[CCData1$purchasechannel == "O"] <- "Online"
        # CCData1$purchasechannel[CCData1$purchasechannel != "Online"] <- "Telephone"
        # CCData1$cancellationreason[CCData1$cancellationreason != ""] <- "NTU"
        # 
        # CCData1$purchasetype[CCData1$purchasetype != "0"] <- "Monthly"
        # CCData1$purchasetype[CCData1$purchasetype == "0"] <- "Annual"
        CCData1
      }else if(input$reportSelect[1] == "ALPS LE Report"){
        ALPSReport <- AdData[AdData$BTXDtsettled == "" & AdData$BTXInsurer == "Auto Legal Protection Services" & AdData$BTXPoltype == "LE",]
        
        ALPSReport$BCMTitle <- word(ALPSReport$BCMTitle, 1)
        ALPSReport$surname <- word(ALPSReport$BCMName, -1)
        ALPSReport$BCMName <- word(ALPSReport$BCMName, -2)
        ALPSReport$No..of.Units <- 0
        ALPSReport$PriceSoldFor <- 0
        ALPSReport$IPT <- 0.91
        
        
        ALPSReport <- ALPSReport[,c("BTXPolref", "BCMTitle", "BCMName", "surname", "BTXDtraised", "BCMAddr1", "BCMAddr2", "BCMAddr3", "BCMAddr4", "BCMPcode", "No..of.Units", "BTXOrigdebt", "IPT")]
        colnames(ALPSReport) <- c("Broker Policy Number", "Title",	"FirstName",	"Surname / Company Name",	"Startdate",	"Address 1",	"Address 2",	"Address 3",	"Address 4",	"Post Code",	"No. of Units",	"PriceSoldFor",	"IPT")
        ALPSReport
      }else if(input$reportSelect[1] == "MIS Report"){
        MISReport <- AdData[AdData$BTXDtsettled == "" & AdData$BTXInsurer == "MIS Claims" & AdData$BTXPoltype != "HQ",]
        MISReport <- MISReport[,c("BTXPolref", "BCMName", "BCMAddr1", "BCMAddr2", "BCMAddr3", "BCMAddr4", "BCMPcode", "BCMTel", "BTXDtraised")]
        MISReport$BTXDtraised <- as.Date(MISReport$BTXDtraised, "%d/%m/%Y")
        year(MISReport$BTXDtraised) <- year(MISReport$BTXDtraised)+1
        MISReport$UserID <- substr(MISReport[,1], 1, 6)
        
        AdData$CFReg <- ifelse(AdData$CFReg == "", AdData$TW1Regmark, AdData$CFReg)
        
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
      }else if(input$reportSelect[1] == "XS Cover Report"){
        XSReport <- AdData[AdData$BTXDtsettled == "" & AdData$BTXInsurer == "XS Cover" & AdData$BTXPoltype == "XS",]
        XSReport$BTXDtraised1 <- as.Date(XSReport$BTXDtraised, "%d/%m/%Y")
        year(XSReport$BTXDtraised1) <- year(XSReport$BTXDtraised1)+1
        XSReport$Cover <- 0
        XSReport <- XSReport[,c("BTXPolref", "BTXTrantype", "BTXDtraised", "BTXDtraised1", "BCMName", "BCMAddr1", "BCMAddr2", "BCMAddr3", "BCMAddr4", "BCMPcode", "BCMTel", "CFReg", "Cover", "BTXOrigdebt")]
        XSReport$BTXTrantype[XSReport$BTXTrantype == "Renewal"] <- "REN"
        XSReport$BTXTrantype[XSReport$BTXTrantype == "New Business"] <- "NB"
        XSReport$UserID <- substr(XSReport[,1], 1, 6)
        
        VehicleReg <- AdData[AdData$CFReg != "",c("BTXPolref", "CFReg")]
        VehicleReg <- VehicleReg[!duplicated(VehicleReg), ]
        VehicleReg$UserID <- substr(VehicleReg$BTXPolref, 1, 6)
        
        XSReport <- merge(XSReport, VehicleReg, by = "UserID", all.x=TRUE)
        XSReport$CFReg.x <- XSReport$CFReg.y
        XSReport$CFReg.y <- NULL
        XSReport$BTXPolref.y <- NULL
        XSReport$UserID <- NULL
        colnames(XSReport) <- c("Reference",	"Reason for Issue",	"Inception Date",	"Termination Date",	"Assured",	"Address1",	"Address2",	"Address3",	"Address4",	"Postcode",	"Telno",	"Vehicle Reg",	"Cover",	"Premium inc IPT")
        XSReport
      }else if(input$reportSelect[1] == "Quotezone Report"){
        QZData <- subset(my_data, (my_data$Source == "Quotezone" | !is.na(my_data$Quote.Reference)) & BTXTrantype == "New Business")
        SalesData<- subset(QZData, QZData$BTXDatecreated >= input$dateRange1[1] & QZData$BTXDatecreated <= input$dateRange1[2])
        SalesData$Cancellation <- "N"

        CancellationsData<- subset(QZData, as.Date(QZData$CancellationDate,  "%Y-%m-%d") >= input$dateRange1[1] & as.Date(QZData$CancellationDate,  "%Y-%m-%d") <= input$dateRange1[2] & QZData$Cancellation != "N")
        QuotezoneData <- rbind(SalesData, CancellationsData)
        QuotezoneData$Brand.ID[grepl("PC", QuotezoneData$BTXPoltype, ignore.case=FALSE)] <- "2475"
        QuotezoneData$Brand.ID[grepl("HQ", QuotezoneData$BTXPoltype, ignore.case=FALSE)] <- "3489"
        QuotezoneData$Brand.ID[grepl("MC", QuotezoneData$BTXPoltype, ignore.case=FALSE)] <- "3350"
        QuotezoneData$Brand.ID[grepl(c("CV|TW"), QuotezoneData$BTXPoltype, ignore.case=FALSE)] <- "3181"
        QuotezoneData$SingleCombined[grepl("HQ", QuotezoneData$BTXPoltype, ignore.case=FALSE)] <- QuotezoneData$TrafficCost[grepl("HQ", QuotezoneData$BTXPoltype, ignore.case=FALSE)]
        QuotezoneData <- QuotezoneData[c("Brand.ID", "FIRST.NAME", "LAST.NAME", "BCMDob", "BCMPcode", "BCMEmail", "Quote.Date", "BTXDtraised", "BTXDatecreated", "SingleCombined", "BTXOrigdebt", "Cancellation", "BTXPolref")]
        colnames(QuotezoneData) <- c("Brand ID", "First Name", "Sur-name", "DOB", "Post-code", "Email", "Quote Date", "Inception Date", "Quote Sale Date", "SingleCombined", "Price Paid", "Cancelled", "BTXPolref" )

        QuotezoneData$Cancelled <- gsub('Cancellation', 'Y', QuotezoneData$Cancelled)
        rownames(QuotezoneData) <- NULL
        QuotezoneData[is.na(QuotezoneData)] <- ""
        QuotezoneData[QuotezoneData$Cancelled != "N", "Cancelled"] <- "Y"
        QuotezoneData[QuotezoneData$SingleCombined == "36.5", "SingleCombined"] <- "S"
        QuotezoneData[QuotezoneData$SingleCombined == "42.5", "SingleCombined"] <- "C"
        QuotezoneData
        
      }else if(input$reportSelect[1] == "Daily Report"){
        report <- subset(my_data, (my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2])| (my_data$CancellationDate >= input$dateRange1[1] & my_data$CancellationDate <= input$dateRange1[2]))
        #reportCancellations <- subset(my_data, as.Date(my_data$Cancellation,  "%d/%m/%Y") == (Sys.Date()-1))
        report <- report [!duplicated(report), ]
        report <- report[order(report$Cancellation, report$BTXTrantype),]
        report <- report[c("BTXPolref", "BTXDatecreated", "Product", "Executive", "BTXTrantype", "TotalValue", "TrafficCost", "AddOnValue", "FinanceValue", "Discount", "Cancellation")]
        report
      }else if(input$reportSelect[1] == "Pending Renewals"){
        report <- subset(my_data, (my_data$BTXTrantype == "Pending Renewal"))
        #reportCancellations <- subset(my_data, as.Date(my_data$Cancellation,  "%d/%m/%Y") == (Sys.Date()-1))
        report <- report[order(report$BTXDatecreated, report$TotalValue),]
        report <- report[c("BTXPolref", "BTXDatecreated", "Product", "Executive", "TotalValue", "Discount")]
        report$Value.Rating <- round((report$TotalValue + report$Discount)/ (sum(report$TotalValue) + sum(report$Discount))*100, 2)
        report$Max.Discount <- report$TotalValue - report$Discount
        colnames(report)[colnames(report) == 'Discount'] <- 'Previous.Discount'
        report <- report[order(-report$Value.Rating),]
        report <- report[c("Value.Rating", "BTXPolref", "BTXDatecreated", "Product", "Executive", "Previous.Discount", "Max.Discount")]
        report
      }
  })
  
  
  #Daily Report Content
  data4 <- reactive({
    #report <- subset(my_data, (my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2]) | (as.Date(my_data$Cancellation,  "%Y-%m-%d") >= input$dateRange1[1] & as.Date(my_data$Cancellation,  "%Y-%m-%d") <= input$dateRange1[2]))
    report <- subset(my_data, (my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2])| (my_data$CancellationDate >= input$dateRange1[1] & my_data$CancellationDate <= input$dateRange1[2]))
    #reportCancellations <- subset(my_data, as.Date(my_data$Cancellation,  "%d/%m/%Y") == (Sys.Date()-1))
    report <- report [!duplicated(report), ]
    report <- report[order(report$Cancellation, report$BTXTrantype),]
    report <- report[c("BTXPolref", "BTXDatecreated", "Product", "Executive", "BTXTrantype", "TrafficCost", "AddOnValue", "FinanceValue", "Discount", "TotalValue", "Cancellation")]
    report1 <- report[1, ]
    report1[1,] <- NA
    report <- rbind(report, report1)
    report[nrow(report),6:10] <- colSums(report[1:(nrow(report)-1),6:10], dims = 1)
    #report[nrow(report),c(1:4,10)] <- NULL
    report <- sapply(report, as.character)
    report[is.na(report)] <- " "
    report
  })
  
  data5 <- reactive({
    my_data1 <- subset(my_data, my_data$BTXDatecreated >= input$dateRange1[1] & my_data$BTXDatecreated <= input$dateRange1[2])
    Profit <- aggregate(as.numeric(my_data1$TotalValue) ~ my_data1$Product + my_data1$Executive, my_data1, FUN=sum)
    Profit[,3] <- round(Profit[,3], 2)
    Sales <- my_data1[ which(my_data1$Cancellation=='N'),]
    Summary <- aggregate(Sales$TotalValue~Sales$Product+ Sales$Executive, Sales, FUN = length)
    #Summary
    Summary2 <- aggregate(cbind(Sales$TrafficCost, Sales$AddOnValue, Sales$FinanceValue, Sales$Discount)~Sales$Product+ Sales$Executive, Sales, FUN = sum)
    names(Summary)[1]<- "Product"
    names(Summary)[2]<- "Executive"
    names(Summary2)[1]<- "Product"
    names(Summary2)[2]<- "Executive"
    names(Profit)[1]<- "Product"
    names(Profit)[2]<-"Executive"
    Profit <- merge(Profit, Summary, by=c("Product","Executive"), all.x=T)
    names(Profit)[4] <- "Sales"
    Profit
  })
  
  data7 <- reactive({
    my_data1 <- subset(endorsements, endorsements$BTXDatecreated.x >= input$dateRange1[1] & endorsements$BTXDatecreated.x <= input$dateRange1[2])
    my_data2 <- my_data1[1, ]
    my_data2[1,] <- NA 
    report <- rbind(my_data1, my_data2)
    report[nrow(report),3] <- sum(report[1:(nrow(report)-1),3])
    row.names(report) <- NULL
    report <- sapply(report, as.character)
    report[is.na(report)] <- " "
    report
    })
  
  #Summary Table Sales Tab
  data6 <- reactive({
    my_data1 <- data()
    my_data2 <- subset(my_data1, BTXPaydt != "")
    my_data2 <- my_data1[ which(my_data1$BTXTrantype == "Renewal" | my_data1$BTXTrantype == "New Business"),]
    Totals <- data.frame(matrix(NA, nrow = 1, ncol = 6))
    colnames(Totals) <- c("New Business", "Renewals", "Pending Renewals", "New Business Cancellations", "New Business Cancellation Percentage", "Profit")
    Totals[1,1] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "New Business"))
    Totals[1,2] <- nrow(subset(my_data2, Cancellation == "N" & BTXTrantype == "Renewal"))
    Totals[1,3] <- nrow(subset(my_data1, BTXTrantype == "Pending Renewal"))
    Totals[1,4] <- nrow(subset(my_data2, Cancellation != "N" & BTXTrantype == "New Business"))
    Totals[1,5] <- percent(as.numeric(Totals[1,4])/(as.numeric(Totals[1,1]) + as.numeric(Totals[1,4])))
    Totals[1,6] <- currency(sum(as.numeric(my_data2$TotalValue)))
    Totals
  })
  
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
      Cancellations <- my_data2[ which(my_data2$Cancellation=="Y"),]
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
      Profit$Y1Profit <- round((Profit[,4]+Profit[,5])*0.5, 2)
      Profit$Y2Profit <- round((Profit[,4]+Profit[,5])*0.25, 2)
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
    names(Profit)[1]<-"BTXDatecreated"
    names(Profit)[2]<-"Total Profit"
    names(Profit)[5]<-"Sales Cancellation Percentage"
    names(Profit)[6]<-"Average Profit"
    Profit <- Profit[order(Profit[1]),]
    Profit[is.na(Profit)] <- 0
    Profit
  })
  
  ### Staff Performance
  data10 <- reactive({
    
    my_data10 <- data3()
    #my_data11 <- my_data10
    my_data10 <- subset(my_data10,  BTXPaydt != "")
    
    Staff <- aggregate(as.numeric(my_data10$TotalValue), by=list(Category=my_data10$Executive), FUN=sum)
    Sales <- my_data10[ which(my_data10$Cancellation=='N'),]
    CountSales <- aggregate(as.numeric(Sales$TotalValue), by=list(Category=Sales$Executive), FUN=length)
    names(CountSales)[2]<-"Count"
    Staff$Sales <- CountSales$Count[match(Staff$Category, CountSales$Category)]
    Cancellations <- my_data10[ which(my_data10$Cancellation!='N'),]
    if(nrow(Cancellations) >0){
      CountCancellations <- aggregate(as.numeric(Cancellations$TotalValue), by=list(Category=Cancellations$Executive), FUN=length)
      names(CountCancellations)[2]<-"Count"
      Staff$Sales <- CountSales$Count[match(Staff$Category, CountSales$Category)]
      Staff$Cancellations <- CountCancellations$Count[match(Staff$Category, CountCancellations$Category)]
    } else{Staff$Cancellations <- 0}
    Staff[is.na(Staff)] <- 0
    Staff[,5] <- round(as.numeric(Staff[,4])/(as.numeric(Staff[,3])+as.numeric(Staff[,4]))*100, 2)
    Staff[,6] <- round(Staff[,2]/(as.numeric(Staff[,3])+as.numeric(Staff[,4])), 2)
    AddOnCount1 <- subset(Sales, Sales$AddOnCount != 0)
    if(nrow(AddOnCount1) >0){
      CountAddOns <- aggregate(as.numeric(AddOnCount1$AddOnCount), by=list(Category=AddOnCount1$Executive), FUN=sum)
      names(CountAddOns)[2]<-"Count"
      Staff$AddOns <- CountAddOns$Count[match(Staff$Category, CountAddOns$Category)]
    } else{Staff$AddOns <- 0}
    #    Profit$CancellationPercentage <- as.numeric(Profit$Cancellations)/(as.numeric(Profit$Sales)+as.numeric(Profit$Cancellations))
    Staff[,2] <- round(Staff[,2], 2)
    names(Staff)[1]<-"BTXDatecreated"
    names(Staff)[2]<-"Total Profit"
    names(Staff)[5]<-"Sales Cancellation Percentage"
    names(Staff)[6]<-"Average Profit"
    names(Staff)[7]<-"Add-Ons"
    Staff <- Staff[order(Staff[1]),]
    Staff[is.na(Staff)] <- 0
    Staff
  })

    output$selectUI2 <- renderUI({
      if(user_details()$id %in% users){
        selectInput("filterName", "Select Product", filterList2)
      }
    })
    
    output$selectUI3 <- renderUI({
      if(user_details()$id %in% users){
        selectInput("filterName3", "Select Product", filterList2)
      }
    })

    output$my_output_data6 <- renderDataTable({
      if(user_details()$id %in% users){
      data6()}}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
    
    output$dailyPlot2 <- renderPlotly({
      if(user_details()$id %in% users){
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
      }}
    })
    
    output$dailyPlot3 <- renderPlotly({
      if(user_details()$id %in% users){
      if(length(input$dataset3) == 0){
        plot_ly(
          x = data9()[,1],
          y = data9()[,input$plotFilter],
          name = "Performance",
          type = "bar"
        )}
      else if(length(input$dataset3) == 1){
        p <- plot_ly(data8(), x = ~data8()[,1], y = ~data8()[,4], type = 'scatter', mode = 'lines', name = 'Gross Profit') %>%
          add_trace(y = ~data8()[,10], name = 'Y1Profit', mode = 'lines+markers') %>%
          add_trace(y = ~data8()[,11], name = 'Y2Profit', mode = 'lines+markers') %>%
          layout(yaxis = list(title = 'Sum'), xaxis = list(title = ""))
        p
      } else if(length(input$dataset3) > 1){
        plot <- data()
        p <- plot_ly(data = plot, type = "scatter", x = ~plot[[input$dataset3[1]]], y = ~plot$TotalValue, color = ~plot[[input$dataset3[2]]])%>%
          layout(                        # all of layout's properties: /r/reference/#layout
            title = "Individual Policy View", # layout's title: /r/reference/#layout-title
            xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
              title = input$dataset3[1],      # xaxis's title: /r/reference/#layout-xaxis-title
              showgrid = F), 
            yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
              title = "Total Value")
            )
        p
      }
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { 'SaleData.csv' }, content = function(file) {
        if(user_details()$id %in% users){
        write.csv(data1(), file, row.names = FALSE)
      }}
    )
    
    output$reportDownload <- downloadHandler(
      filename = function() { paste0(input$reportSelect[1],".csv") }, content = function(file) {
        if(user_details()$id %in% users){
        write.csv(data2(), file, row.names = FALSE)
        }
      }
    )
    
    output$my_output_data6 <- renderDataTable({
      if(user_details()$id %in% users){
      data6()}}, options =list(paging = FALSE, searching = FALSE, info = FALSE))
    output$my_output_data8 <- renderDataTable({
      if(user_details()$id %in% users){
      if(length(input$dataset3) > 0){data8()[,1:(ncol(data8())-2)]}}
    })
    
    
    output$my_output_data2 <- renderDataTable({
      if(user_details()$id %in% users){
      data2()}}, options =list(searching = T, info = T))
    
    output$my_output_data10 <- renderDataTable({
      if(user_details()$id %in% users){
      data10()}}, options =list(searching = T, info = T))
    
    output$dailyPlot10 <- renderPlotly({
      if(user_details()$id %in% users){
      plot_ly(
        labels = data10()[,1],
        values = data10()[,input$plotFilter2],
        type = "pie") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }})
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        if(user_details()$id %in% users){
        params <- list(n = input$filterName, m = data4(), o = data5(), p = data7())
        }
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  output$user_name <- renderText({
    validate(
      need(user_details(), "getting user details")
    )
    as.character(user_details()$id)
  })
  
  output$session_info <- renderPrint(session_info(), width = 120)
})

shinyApp(ui = ui, server = server)
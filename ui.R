library(leaflet)
library(shiny)
library(plotly)

ui = fluidPage(
  
navbarPage(
    title = "Sales Analysis Tool",
    tabPanel('Apricot Sales Performance',   
             sidebarPanel(
               dateRangeInput('dateRange',
                              label = 'Sale Date range (yyyy-mm-dd): ',
                              start = "2016-08-01", end = Sys.Date()
               ),
               br(), width = 12),
             mainPanel(
               dataTableOutput(outputId ="my_output_data6"),
               selectizeInput("dataset3", "Select data breakdown criteria", choices = c("Age", "Age.Range", "Apricot.Position", "BTXDatecreated", "BTXInsurer", "BTXPoltype", "BTXTrantype", "Day.of.Week", "Do.you.normally.pay.for.your.insurance.monthly.", "Drivers.to.be.insured.", "Email.Domain", "Employment.Status", "Executive", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.", "How.many.years.no.claims.bonus..NCB..do.you.have.",  "Licence.Years", "Price.Returned.Range", "Postcode.Area", "Postcode.Region", "Proposer.Claims.Count", "Proposer.Convictions.Count", "Quote.Day", "Quote.Hour", "Source","SOURCE.TYPE.y", "TrafficCost", "Type.of.driving.licence", "Vehicle.Value.Range", "Vehicle.Year.of.Manufacture", "Voluntary.excess.", "What.is.the.estimated.value.of.the.vehicle.", "What.type.of.cover.would.you.like."),
                 multiple = TRUE, options = list(maxItems = 3)),
                        fluidRow(column(
                          selectInput("plotFilter", "Select Performance Metric", choices = c("Average Gross Profit per Sale (£)", "Cancellations", "Gross Profit (£)", "Sales Cancellation Percentage", "Sales")),
                          plotlyOutput("dailyPlot2"),
                          dataTableOutput(outputId ="my_output_data8"), width = 12), width = 14),
               br(),
               downloadButton("downloadData", "Download Sales Report"),           
               br(),
               br(),
               br(),
               br(),
               br(),
               br())

    )
)
)
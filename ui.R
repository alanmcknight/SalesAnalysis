library(leaflet)
ui = fluidPage(
  
navbarPage(
    title = "Sales Analysis Tool",
    tabPanel('Apricot Sales Performance',   
             sidebarPanel(
               dateRangeInput('dateRange',
                              label = 'Sale Date range (yyyy-mm-dd): ',
                              start = "2016-04-01", end = Sys.Date()
               ),
               tableOutput(outputId ="my_output_data6"),width = 6),
             mainPanel(
               selectInput("dataset", "Aggregate data by:", choices = c("Insurer", "Cancellation.Reason", "Employment.Status", "Age.Range", "Postcode.Region", "Quote.Day.of.Week", "What.type.of.cover.would.you.like.", "Type.of.driving.licence", "Aggregator.Results.Position", "Source.Type", "Aggregator", "Traffic.Type", "Type.of.driving.licence", "Have.you.had.any.accidents.or.losses..whether.you.have.claimed.or.not.and.regardless.of.blame..in.the.last.5.years.", "Have.you.had.any.motoring.convictions..including.fixed.penalty.endorsements...or.anything.pending..in.the.last.5.years.", "Have.you.had.any.unspent.non.motoring.criminal.convictions.", "Has.the.vehicle.been.modified.in.any.way.e.g..alloy.wheels..tow.bar.etcâ..", "Is.the.vehicle.a.grey.or.parallel.import.", "Quote.sale.date")),
                        fluidRow(column(dataTableOutput(outputId ="my_output_data7"), width =8), width = 12))
                        
    )
)
)
list_field_admin = list(sidebarPanel(
                          dateRangeInput('dateRange',
                                         label = 'Sale Date range (yyyy-mm-dd): ',
                                         start = "2017-01-01", end = Sys.Date()
                          ),
                          htmlOutput("selectUI2"),
                          br(), width = 12),
                        mainPanel(
                          dataTableOutput(outputId ="my_output_data6"),
                          selectizeInput("dataset3", "Select data breakdown criteria", choices = c("AddOnValue", "Age", "Age.Range", "Apricot.Position", "BPYNotes2", "BTXCommamt", "BTXDatecreated", "BTXDtraised", "BTXExec","BTXInsurer", "BTXOrigdebt.Range", "BTXOrigdebt", "BTXPolref", "BTXPoltype", "BTXTrantype", "Cancellation", "Day.of.Week", "Discount", "Do.you.normally.pay.for.your.insurance.monthly.", "Drivers.to.be.insured.", "ECWebref", "Email.Domain", "Employment.Status", "Executive", "FinanceValue", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.", "How.many.years.no.claims.bonus..NCB..do.you.have.",  "Licence.Years", "Month", "PaymentMethod", "Price.Returned.Range", "Postcode.Area", "Post.Code.District", "Post.Code.Prefix", "Postcode.Region", "Product", "Proposer.Claims.Count", "Proposer.Convictions.Count", "Quote.Date", "Quote.Day", "Quote.Hour", "Quote.Reference", "Source","SOURCE.TYPE.y", "TrafficCost", "Type.of.driving.licence", "Vehicle.Value.Range", "Vehicle.Year.of.Manufacture", "Voluntary.excess.", "What.is.the.estimated.value.of.the.vehicle.", "What.type.of.cover.would.you.like.", "Year"),
                                         multiple = TRUE, options = list(maxItems = 3)),
                          fluidRow(column(
                            selectInput("plotFilter", "Select Performance Metric", choices = c("Total Profit", "Average Profit", "Cancellations", "Sales Cancellation Percentage", "Sales"), selected = "Sales"),
                            plotlyOutput("dailyPlot2"),
                            selectizeInput("dataset4", "Table Summary Type", choices = c("Count", "Mean", "Sum", "% Uptake"), selected = "Sum"),
                            dataTableOutput(outputId ="my_output_data8"), width = 12), width = 14),
                          br(),
                          downloadButton("downloadData", "Download Sales Report"),           
                          br(),
                          plotlyOutput("dailyPlot3"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br())
                        
)

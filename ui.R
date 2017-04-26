library(leaflet)
ui = fluidPage(
  
navbarPage(
    title = "Pricing Analysis Tool",
    tabPanel('Top 3 Projections',   
             sidebarPanel(
               selectInput("filterName1", "Select Insurer to review", choices = c("ABC Insurance", "AXA Insurance", "Chaucer Insurance", "Covea", "Highway Insurance", "Sabre Insurance", "Zenith Marque")),
               sliderInput("integer", "Adjust price by (£):", 
                           min=-75, max=75, value=0, step= 5), 
               sliderInput("percentage", "Adjust price by (%):", 
                           min=-25, max=25, value=0, step= 1), 
               htmlOutput("selectUI"), width = 8), 
             mainPanel(h3("Overall"),
                       fluidRow(column(dataTableOutput(outputId ="my_output_data5"), width =8)), 
                       br(),
                       h3("Top 1s by filter criteria"),
                       fluidRow(column(dataTableOutput(outputId ="my_output_data7"), width =8)),
                       br(),
                       h3("Panel Sales by filter criteria"),
                       fluidRow(column(dataTableOutput(outputId ="my_output_data10"),width =8)))
    ),
    # tabPanel(
    #   'Graphs',  
    #   sidebarPanel(
    #     sliderInput("integer2", "Adjust price by (£):", 
    #                 min=-100, max=0, value=0, step= 5), 
    #     sliderInput("percentage2", "Adjust price by (%):", 
    #                 min=-25, max=0, value=0, step= 1), 
    #     htmlOutput("selectUI2"), width = 8), 
    #   mainPanel(
    #          fluidPage( 
    #           downloadButton("downloadData4", "Download Quote Results Position Report"),
    #           plotOutput(outputId = "main_plot1", width = "100%", height = "300px" ),
    #           br(),
    #           downloadButton("downloadData8", "Download Adjusted Price Quote Results Position Report"),
    #           plotOutput(outputId = "main_plot2", width = "100%", height = "300px")
    #          ))),
    tabPanel('Top 1s',  mainPanel(
      selectizeInput("dataset", "Select data breakdown criteria", choices = c("Age", "Age.Range", "AGREGATOR","Day.of.Week.Quote", "Drivers.to.be.insured.", "Email.Domain.1", "Employment.Status", "Has.the.vehicle.been.modified.in.any.way.e.g..alloy.wheels..tow.bar.etcâ..", "Have.you.had.any.unspent.non.motoring.criminal.convictions.", "Have.you.or.any.driver.ever.had.insurance.declined..cancelled.or.special.terms.imposed.", "How.many.years.no.claims.bonus..NCB..do.you.have.", "Have.you.been.regularly.driving.a.car.not.insured.by.you.", "How.many.years.claim.free.driving.do.you.have.on.the.car.not.insured.by.you.",  "Insurer", "Is.the.vehicle.a.grey.or.parallel.import.", "Postcode.Area", "Postcode.Region", "Post.Code.Prefix", "Price.Range", "Proposer.Claims.Count", "Proposer.Convictions.Count", "QUOTE.REFERENCE", "SYSTEM.NAME", "Type.of.driving.licence", "UK.Residency.Years", "Vehicle.Value.Range", "Vehicle.Year.of.Manufacture", "Voluntary.excess.", "What.is.the.estimated.value.of.the.vehicle.", "What.type.of.cover.would.you.like."),
                     multiple = TRUE, options = list(maxItems = 3)),
      fluidRow(column(dataTableOutput(outputId ="my_output_data11"), width =10), width = 12)
    )),
    tabPanel('Maps',  fluidPage(
      leafletOutput("mymap", height = "800px"),
      p()
    ))
)
)
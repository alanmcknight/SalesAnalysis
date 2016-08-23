library(leaflet)
ui = fluidPage(
  
navbarPage(
    title = "Pricing Analysis Tool",
    tabPanel('Top 3 Projections',   
             sidebarPanel(
               selectInput("filterName1", "Select Insurer to review", choices = c("ABC Insurance", "AXA Insurance", "Chaucer Insurance", "Covea", "Highway Insurance", "Sabre Insurance", "Zenith Marque")),
               sliderInput("integer", "Adjust price by (£):", 
                           min=-100, max=50, value=-20, step= 5), 
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
    tabPanel(
      'Graphs',  
      sidebarPanel(
        sliderInput("integer2", "Adjust price by (£):", 
                    min=-100, max=-5, value=0, step= 5), 
        htmlOutput("selectUI2"), width = 8), 
      mainPanel(
             fluidPage( 
              downloadButton("downloadData4", "Download Quote Results Position Report"),
              plotOutput(outputId = "main_plot1", width = "100%", height = "300px" ),
              br(),
              downloadButton("downloadData8", "Download Adjusted Price Quote Results Position Report"),
              plotOutput(outputId = "main_plot2", width = "100%", height = "300px")
             ))),
    tabPanel('Maps',  fluidPage(
      leafletOutput("mymap", height = "800px"),
      p()
    ))
)
)
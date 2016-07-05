#insurance.providers <- read.csv('ApricotJan151.csv')
ui = fluidPage(
  
navbarPage(
    title = 'Pricing Analysis Tool',
    tabPanel('Data Upload and Filtering',   sidebarPanel(
      h3("Step 1: Select file to upload"),
      p('Please view our sample .csv file,',
        a(href = 'ApricotJan151.csv', 'SamplePricing.csv')
      ),
      fileInput('file1', NULL,accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
      h3("Step 2: View Insurance Providers summary"),
      actionButton("Load", "View Upload Summary"), 
      h3("Step 3: Select the Insurance Provider to review"),
      textInput("insurance.provider", "Insurance Provider:", "Insurance Provider as shown in the Upload Summary"),
      #tableOutput("my_output_data"),
      #checkboxInput('header', 'Header', TRUE),
      #actionButton("Load", "View Data"), 
      actionButton("Load3", "View Position 1 Analysis"),
      downloadButton('downloadData', 'Download Summary Report'),
      width = 12
    ), 
    mainPanel(tableOutput("my_output_data3"), tableOutput("my_output_data"))),
    tabPanel('Graphs',  
             sidebarPanel(
               selectInput(inputId = "n_breaks",
                         label = "Number of Groups:",
                         choices = c(10, 20, 50, 100),
                         selected = 20)
             ),
             mainPanel( plotOutput(outputId = "main_plot", height = "300px"))),
    tabPanel('Maps',  NULL)
  )
)
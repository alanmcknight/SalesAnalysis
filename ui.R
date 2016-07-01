ui = fluidPage(
  sidebarPanel(
    fileInput('file1', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
    p('If you want a sample .csv file to upload,',
      'you can first download the sample',
      a(href = 'ApricotJan151.csv', 'ExampleBroker.csv'),' and then try adding to and uploading this.'
    ),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote'),
    actionButton("Load", "View Data"), 
    downloadButton('downloadData', 'Download Summary Report'),width = 5
), 
  mainPanel(tableOutput("my_output_data"))
)
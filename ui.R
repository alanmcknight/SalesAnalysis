shinyUI(fluidPage(
  titlePanel("Upload File"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),

      tags$hr(),
      p('If you want a sample .csv file to upload,',
        'you can first download the sample',
        a(href = 'ApricotInsuranceTemplate.csv', 'ApricotInsuranceTemplate.csv'),' and then try adding to and uploading this.'
      )
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
))
library(shiny)
library(shinydashboard)
shinyUI( 
  dashboardPage(
    dashboardHeader(title = "Apricot Dashboard"),
    dashboardSidebar(disable = T),
    dashboardBody(
      tags$head(includeScript("GTM.js")),
      tags$head(includeScript("GTMDataLayer.js")),
      uiOutput("page")
    )
  )
  
)
library(shiny)
library(shinydashboard)
shinyUI( 
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(disable = T),
    dashboardBody(
      tags$head(includeScript("GTMDataLayer.js")),
      tags$head(includeScript("GTM.js")),
      uiOutput("page")
    )
  )
  
)
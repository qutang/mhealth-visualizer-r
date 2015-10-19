TITLE = "mHealthFormat: interactive visualizer"

library(shiny)
library(shinydashboard)
library(shinyFiles)
print(getwd())
source(file.path(getwd(),"uielements.R"))

shinyUI(
  dashboardPage(
    header = dashboardHeader(title = TITLE,
                             titleWidth = "30%"),
    sidebar = dashboardSidebar(fluidRow(
      .datasetBox(),
      .helpBox()
    )),
    body = dashboardBody(fluidRow(
      .summaryPlotBox()
    ),
    verbatimTextOutput("info"))
  )
)

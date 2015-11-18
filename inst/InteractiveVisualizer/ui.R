TITLE = "mHealthFormat: interactive visualizer"

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)

if(exists("appFolder")){
  source(file.path(appFolder, "uielements.R"), local = TRUE)
}else{
  source("uielements.R", local = TRUE)
}

ui = dashboardPage(
    useShinyjs(),
    header = dashboardHeader(title = TITLE,
                             titleWidth = "30%"),
    sidebar = dashboardSidebar(
      .datasetBox(),
      .helpBox(),
      fluidRow(column(width = 10, offset = 1, bsAlert("alert"))
    )),
    body = dashboardBody(fluidRow(
      .dateControlBox(),
      .dataControlBox(),
      .summaryDataControlBox(),
      .summaryPlotBox(),
      .rawPlotBox()
    ),
    verbatimTextOutput("info"))
  )

require("shiny")
require("shinydashboard")
require("shinyjs")

.helpBox = function(){
  return(box(
    tags$ul(
      tags$li("Hold and drag to zoom in"),
      tags$li("Double click: go back to original view"),
      tags$li("Hover: show annotation name"),
      tags$li("Click \" Refresh\" button to redraw summary plot")
    ),
    width = 12, background = "black", title = "Help"
  ))
}

.datasetBox = function(){
  return(box(
    shinyDirButton(id = "datasetFolder",
                   title = "Choose mHealth Directory",
                   label = "Choose Directory",buttonType = "primary"),
    helpText("Note: directory should be mhealth format compatible subject directory or a flat directory contains mhealth format compatible files"),
    tags$p(id = "subjectId", "Subject ID: "),
    width = 12, background = "black", title = "Dataset"
  ))
}

.summaryDataControlBox = function(){
  return(
    box(
      selectInput("summaryMethod", label = "Choose summary method", choices = list("mean", "AUC"),
                  selected = NULL, multiple = FALSE, width = "100%"),
      selectInput("summaryValue", label = "Choose summary value", choices = list("magnitude", "axis"),
                  selected = NULL, multiple = FALSE, width = "100%"),
      selectInput("summaryInterval", label = "Choose summary interval", choices = list("sec", "min"),
                  selected = NULL, multiple = FALSE , width = "100%"),
      bsButton("computeSummary", label = "Compute summary data", style="primary"),
      width = 4, collapsible = TRUE, title = "Summary data control"
    )
  )
}

.summaryPlotBox = function(){
  return(
    box(
    fluidRow(
      column(width = 3, downloadButton("saveSummary", label = "Save summary data as CSV")),
      column(width = 3, offset = 1, downloadButton("saveImage", label = "Save current image as PDF")),
      column(width = 1, offset = 4, bsButton("refreshPlot", label = "", style="primary", icon = icon("refresh")))
    ),
    plotOutput(
      "plot1",
      brush = brushOpts(
        id = "plot_brush", direction = "x", resetOnNew = TRUE
      ),
      dblclick = "plot_dblclick",
      hover = hoverOpts(
        id = "plot_hover",
        delay = 150,
        delayType = "throttle",
        nullOutside = TRUE,
        clip = FALSE
      )
    ),
    width = 12, collapsible = TRUE, title = "Summary data view"
  ))
}

.annotationBox = function(){
  infoBoxOutput(outputId = "annotationBox", width = 12)
}

.dateControlBox = function(){
  return(box(
    fluidRow(
      column(width = 3, selectInput("yearSelect",
                  label = "Choose year",
                  choices = list(),
                  selected = FALSE,
                  multiple = FALSE,
                  width = "100%")),
      column(width = 3, selectInput("monthSelect",
                  label = "Choose month",
                  choices = list(),
                  selected = FALSE,
                  multiple = FALSE,
                  width = "100%")),
      column(width = 3, selectInput("daySelect",
                  label = "Choose day",
                  choices = list(),
                  selected = FALSE,
                  multiple = FALSE,
                  width = "100%")),
      column(width = 3, selectInput("hourSelect",
                  label = "Choose hour",
                  choices = list(),
                  selected = FALSE,
                  multiple = FALSE,
                  width = "100%"))
      ),
      title = "Date selection", width = 12, collapsible = TRUE))
}

.dataControlBox = function(){
  return(box(
    fluidRow(
      column(width = 4, selectInput("sensorSelect",
                                    label = "Choose sensors",
                                    choices = list(),
                                    selected = FALSE,
                                    multiple = TRUE,
                                    width = "100%")),
      column(width = 4, selectInput("annotationSelect",
                                    label = "Choose annotation sets",
                                    choices = list(),
                                    selected = FALSE,
                                    multiple = TRUE,
                                    width = "100%")),
      column(width = 4, selectInput("eventSelect",
                                    label = "Choose events",
                                    choices = list(),
                                    selected = FALSE,
                                    multiple = TRUE,
                                    width = "100%"))
    ),
    title = "Data selection", width = 12, collapsible = TRUE))
}

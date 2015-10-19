require("shiny")
require("shinydashboard")

.helpBox = function(){
  return(box(
    tags$ul(
      tags$li("Hold and drag to zoom in"),
      tags$li("Double click: go back to original view"),
      tags$li("Hover: show annotation name")
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
    width = 12, background = "black", title = "Dataset"
  ))
}

.summaryPlotBox = function(){
  return(box(
    plotOutput(
      "plot1",
      brush = brushOpts(
        id = "plot_brush", direction = "x", resetOnNew = TRUE
      ),
      dblclick = "plot_dblclick"
    ),
    width = 12, collapsible = TRUE, title = "Summary data view"
  ))
}

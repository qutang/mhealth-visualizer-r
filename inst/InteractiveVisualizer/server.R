library(shiny)
library(shinyFiles)
library(ggplot2)

shinyServer(function(input, output, session) {
  shinyDirChoose(
    input, 'datasetFolder', roots = c(root = '~/Desktop/'), hidden = TRUE, session = session
  )
  begin_xrange = c(min(auc14sec[,1], ann[,2]), max(auc14sec[,1], ann[,3]))
  xrange = reactiveValues(xlim = begin_xrange)
  output$plot1 <- renderPlot({
    p = SummaryData.ggplot(auc14sec)
    p = AnnotationData.addToGgplot(p, ann)
    p = p + coord_cartesian(xlim = xrange$xlim) + scale_x_datetime()
    p
  })

  output$info <- renderText({
    paste0(
      "begin=", as.POSIXct(xrange$xlim[1], origin = "1970-01-01"),
      "\nend=", as.POSIXct(xrange$xlim[2], origin = "1970-01-01")
    )
  })

  observe({
    brush = input$plot_brush
    if (!is.null(brush)) {
      xrange$xlim = c(
        as.POSIXct(brush$xmin, origin = "1970-01-01"),
        as.POSIXct(brush$xmax, origin = "1970-01-01")
      )
    }
  })

  observeEvent(input$plot_dblclick, {
    xrange$xlim = begin_xrange
  })
})

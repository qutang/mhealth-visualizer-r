library(shiny)
library(shinyFiles)
library(ggplot2)
library(shinyjs)


shinyServer(function(input, output, session) {
  switch(Sys.info()[['sysname']],
         Windows= {root = "C:\\"},
         Linux  = {root = "~"},
         Darwin = {root = file.path("~", "Desktop")})
  volumes <- c(root = file.path(root))
  shinyDirChoose(
    input, 'datasetFolder', roots = volumes, hidden = TRUE, session = session
  )

  rValues <<- reactiveValues(xlim = NULL,
                             currentAnnotations = NULL,
                             masterFolder = NULL,
                             summaryData = NULL,
                             summaryPlot = NULL,
                             annotationData = NULL)
  source(file.path(getwd(),"serverhandler.R"))
  output$plot1 <- renderPlot({
    input$refreshPlot
    withProgress(message = "Generate summary plot", value = 0.1, {
      Sys.sleep(0.25)
      if(!is.null(rValues$summaryData)){
        p = SummaryData.ggplot(rValues$summaryData)
        if(!is.null(rValues$xlim)){
          p = p + coord_cartesian(xlim = rValues$xlim)
        }
        if(!is.null(rValues$annotationData)){
          p = AnnotationData.addToGgplot(p, rValues$annotationData)
        }
        rValues$summaryPlot = p
        rValues$summaryPlot
      }
    })
  })

  output$info <- renderText({
    paste0(
      "begin=", as.POSIXct(rValues$xlim[1], origin = "1970-01-01"),
      "\nend=", as.POSIXct(rValues$xlim[2], origin = "1970-01-01")

    )
  })

  output$annotationBox = renderInfoBox({
    infoBox(
      title = "Current Annotations",
      value = paste(rValues$currentAnnotations, sep = ","),
      icon = icon("tag")
      )
  })

  observe(x = generalHandler(input))

  handlePlotDoubleClick(input)

  handlePlotHover(input)

  handlePlotBrush(input)

  handleDatasetFolderChosen(input, session, volumes = volumes)

  handleYearSelection(input, session)

  handleMonthSelection(input, session)

  handleDaySelection(input, session)

  handleHourSelection(input, session)

  handleComputeSummaryClicked(input, session)

  handleAnnotationSelect(input, session)
})

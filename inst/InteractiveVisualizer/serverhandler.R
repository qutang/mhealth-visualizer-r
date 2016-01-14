generalHandler = function(input) {
  shinyjs::disable("monthSelect")
  shinyjs::disable("daySelect")
  shinyjs::disable("hourSelect")
  shinyjs::disable("sensorSelect")
  shinyjs::disable("annotationSelect")
  shinyjs::disable("eventSelect")
  shinyjs::hide("raw_plot_box")
  if (is.null(input$datasetFolder)) {
    shinyjs::hide("subjectId")
    shinyjs::disable("yearSelect")
  }
}

handlePlotHover = function(input) {
  observeEvent(input$plot_hover,{
    currentTime = as.POSIXct(input$plot_hover$x, origin = "1970-01-01")
    if (!is.null(rValues$annotationData)) {
      labelNames = AnnotationData.getLabelNames(rValues$annotationData, currentTime)
    }else{
      labelNames = NULL
    }
    if(!is.null(rValues$summaryData) || !is.null(rValues$annotationData)){
      rValues$currentAnnotations = paste(currentTime, paste(str_trim(labelNames), collapse = ","), sep = ":")
    }
  })

  observeEvent(input$plot_hover2,{
    currentTime = as.POSIXct(input$plot_hover2$x, origin = "1970-01-01")
    if (!is.null(rValues$annotationData)) {
      labelNames = AnnotationData.getLabelNames(rValues$annotationData, currentTime)
    }else{
      labelNames = NULL
    }
    if(!is.null(rValues$rawData) || !is.null(rValues$annotationData)){
      rValues$currentAnnotations2 = paste(currentTime, paste(str_trim(labelNames), collapse = ","), sep = ":")
    }
  })
}

handlePlotDoubleClick = function(input) {
  observeEvent(input$plot_dblclick, {
    rValues$xlim = rValues$begin_xrange
  })

  observeEvent(input$plot_dblclick2, {
    rValues$raw_xlim = rValues$begin_xrange_raw
  })
}

handlePlotBrush = function(input) {
  observeEvent(input$plot_brush, {
    brush = input$plot_brush
    if (!is.null(brush)) {
      rValues$xlim = c(
        as.POSIXct(brush$xmin, origin = "1970-01-01"),
        as.POSIXct(brush$xmax, origin = "1970-01-01")
      )
      rValues$rawData = NULL
      shinyjs::hide("raw_plot_box")
    }
  })

  observeEvent(input$plot_brush2, {
    brush = input$plot_brush2
    if (!is.null(brush)) {
      rValues$raw_xlim = c(
        as.POSIXct(brush$xmin, origin = "1970-01-01"),
        as.POSIXct(brush$xmax, origin = "1970-01-01")
      )
    }
  })
}

handleDatasetFolderChosen = function(input, session, volumes) {
  observeEvent(input$datasetFolder, {
    rValues$annotationData = NULL
    rValues$summaryData = NULL
    rValues$xlim = NULL
    rValues$begin_xrange = NULL
    rValues$rawData = NULL
    shinyjs::hide("raw_plot_box")
    folder = parseDirPath(volumes, input$datasetFolder)
    rValues$masterFolder = file.path(folder, "MasterSynced")
    if (is.null(folder)) {
      shinyjs::hide("subjectId")
    }else if (dir.exists(rValues$masterFolder)) {
      # mhealth subject directory
      shinyjs::text("subjectId", paste0("Subject ID:", basename(folder)))
      shinyjs::show("subjectId")
      years = list.dirs(rValues$masterFolder, full.names = FALSE, recursive = FALSE)
      updateSelectInput(session = session, inputId = "yearSelect", choices = years)
      shinyjs::enable("yearSelect")
    }else{

    }
  })
}

handleYearSelection = function(input, session) {
  observeEvent(input$yearSelect, {
    rValues$annotationData = NULL
    rValues$summaryData = NULL
    rValues$xlim = NULL
    rValues$begin_xrange = NULL
    rValues$rawData = NULL
    shinyjs::hide("raw_plot_box")
    if (!is.null(input$yearSelect) &&
        !is.null(rValues$masterFolder)) {
      yearFolder = file.path(rValues$masterFolder, input$yearSelect)
      months = list.dirs(yearFolder, full.names = FALSE, recursive = FALSE)
      updateSelectInput(session = session, inputId = "monthSelect", choices = months)
      shinyjs::enable("monthSelect")
    }else{
      print(rValues$masterFolder)
      print(input$yearSelect)
    }
  })
}

handleMonthSelection = function(input, session) {
  observeEvent(input$monthSelect, {
    rValues$annotationData = NULL
    rValues$summaryData = NULL
    rValues$xlim = NULL
    rValues$begin_xrange = NULL
    rValues$rawData = NULL
    shinyjs::hide("raw_plot_box")
    if (!is.null(input$monthSelect) &&
        !is.null(rValues$masterFolder) && !is.null(input$yearSelect)) {
      monthFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect)
      days = list.dirs(monthFolder, full.names = FALSE, recursive = FALSE)
      updateSelectInput(session = session, inputId = "daySelect", choices = days)
      shinyjs::enable("daySelect")
    }else{
      print(rValues$masterFolder)
      print(input$yearSelect)
      print(input$monthSelect)
    }
  })
}
handleDaySelection = function(input, session) {
  observeEvent(input$daySelect, {
    rValues$annotationData = NULL
    rValues$summaryData = NULL
    rValues$xlim = NULL
    rValues$begin_xrange = NULL
    rValues$rawData = NULL
    shinyjs::hide("raw_plot_box")
    if (!is.null(input$monthSelect) &&
        !is.null(rValues$masterFolder)
        && !is.null(input$yearSelect) && !is.null(input$daySelect)) {
      dayFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect)
      hours = list.dirs(dayFolder, full.names = FALSE, recursive = FALSE)
      hours = c(hours, "All")
      updateSelectInput(session = session, inputId = "hourSelect", choices = hours)
      shinyjs::enable("hourSelect")
    }else{
      print(rValues$masterFolder)
      print(input$yearSelect)
      print(input$monthSelect)
      print(input$hourSelect)
    }
  })
}


handleHourSelection = function(input, session) {
  observeEvent(input$hourSelect, {
    rValues$annotationData = NULL
    rValues$summaryData = NULL
    rValues$xlim = NULL
    rValues$begin_xrange = NULL
    rValues$rawData = NULL
    shinyjs::hide("raw_plot_box")
    if (!is.null(input$monthSelect) &&
        !is.null(rValues$masterFolder)
        && !is.null(input$yearSelect) && !is.null(input$daySelect) &&
        !is.null(input$hourSelect)) {
      if (input$hourSelect != "All") {
        hourFolder = file.path(
          rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect
        )
        recursive = FALSE
      }else{
        hourFolder = file.path(
          rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect
        )
        recursive = TRUE
      }
      sensors = list.files(
        hourFolder, full.names = TRUE, recursive = recursive, pattern = "*.sensor.csv*"
      )
      annotations = list.files(
        hourFolder, full.names = TRUE, recursive = recursive, pattern = "*.annotation.csv*"
      )
      events = list.files(
        hourFolder, full.names = TRUE, recursive = recursive, pattern = "*.event.csv*"
      )

      if (length(sensors) > 0) {
        updateSelectInput(
          session = session, inputId = "sensorSelect", choices = .getDisplayNames(sensors)
        )
        shinyjs::enable("sensorSelect")
      }else{
        updateSelectInput(
          session = session, inputId = "sensorSelect", choices = list(), selected = FALSE
        )
        shinyjs::disable("sensorSelect")
      }
      if (length(annotations) > 0) {
        updateSelectInput(session, "annotationSelect", choices = .getDisplayNames(annotations))
        shinyjs::enable("annotationSelect")
      }else{
        updateSelectInput(
          session = session, inputId = "annotationSelect", choices = list(), selected = FALSE
        )
        shinyjs::disable("annotationSelect")
      }
      if (length(annotations) > 0) {
        updateSelectInput(session, "eventSelect", choices = .getDisplayNames(events))
        shinyjs::enable("eventSelect")
      }else{
        updateSelectInput(
          session = session, inputId = "eventSelect", choices = list(), selected = FALSE
        )
        shinyjs::disable("eventSelect")
      }
    }else{
      print(rValues$masterFolder)
      print(input$yearSelect)
      print(input$monthSelect)
      print(input$hourSelect)
    }
  })
}

library("foreach")
handleComputeSummaryClicked = function(input, session) {
  observeEvent(input$computeSummary, {
    if (is.null(input$sensorSelect)) {
      createAlert(
        session, "alert", alertId = "no sensor selected",
        title = "No sensor data selected",
        content = "Please select at least one sensor data file", style = "warning"
      )
    }else{
      if (input$hourSelect != "All") {
        hourFolder = file.path(
          rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect
        )
        recursive = FALSE
      }else{
        hourFolder = file.path(
          rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect
        )
        recursive = TRUE
      }

      sensorFiles = list.files(path = hourFolder, full.names = TRUE, recursive = recursive)

      selectedFiles = sensorFiles[str_detect(sensorFiles, input$sensorSelect)]
      withProgress(message = "Import sensor data", value = 0.1, {
        dataList = foreach(filename = selectedFiles, .export = "incProgress") %do% {
          incProgress(message = paste("Importing", basename(filename)))
          SensorData.importCsv(filename)
        }
        incProgress(message = "Merge imported data")
        merged = SensorData.merge(dataList)
        merged = SensorData.cleanup(merged)
        merged = SensorData.offset(merged, as.numeric(input$rawDataOffset))
        withProgress(message = "Compute summary data", value = 0.6, {
          method = input$summaryMethod
          valueType = input$summaryValue
          interval = input$summaryInterval
          switch(valueType,
                 magnitude = {
                   merged = Magnitude.compute(merged)
                 })
          switch(method,
                 mean = {
                   rValues$summaryData = SummaryData.simpleMean(merged, breaks = interval)
                 },
                 AUC = {
                   rValues$summaryData = SummaryData.auc(merged, breaks = interval)
                 },
                 SamplingRate = {
                   rValues$summaryData = SamplingRate.summary(merged, breaks = interval)
                 })
          rValues$begin_xrange = c(
            min(
              rValues$summaryData[,1], rValues$annotationData[,2], na.rm = TRUE
            ),
            max(
              rValues$summaryData[,1], rValues$annotationData[,3], na.rm = TRUE
            )
          )
          setProgress(value = 1, message = "Summary data is ready")
          createAlert(session, "alert", "summaryData", title = "Summary Data is ready", style = "info")
        })
      })
    }
  })
}

handleAnnotationSelect = function(input, session) {
  observeEvent(input$annotationSelect, {
    if (input$hourSelect == "All") {
      inputFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect)
      recursive = TRUE
    }else{
      inputFolder = file.path(
        rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect
      )
      recursive = FALSE
    }

    annotationFiles = list.files(path = inputFolder, full.names = TRUE, recursive = recursive)

    selectedFiles = annotationFiles[str_detect(annotationFiles, input$annotationSelect)]

    withProgress(message = "Import annotation data", value = 0.1, {
      annotationList = foreach(filename = selectedFiles) %do% {
        AnnotationData.importCsv(filename)
      }
      mergedAnnotation = AnnotationData.merge(annotationList)
      rValues$annotationData = mergedAnnotation
      rValues$begin_xrange = c(
        min(rValues$summaryData[,1], rValues$annotationData[,2], na.rm = TRUE),
        max(rValues$summaryData[,1], rValues$annotationData[,3], na.rm = TRUE)
      )
      rValues$xlim = as.POSIXct(rValues$begin_xrange, origin = "1970-01-01")
    })
  })
}

handleSaveImageAsPdf = function(input, output){
    output$saveImage = downloadHandler(.getPdfFilename("summary_data"), content = function(filename){
        ggsave(filename = filename, plot = rValues$summaryPlot, scale = 2, units = "in", width = 7, height = 3)
    })
    output$saveRawImage = downloadHandler(.getPdfFilename("raw_data"), content = function(filename){
        ggsave(filename = filename, plot = rValues$rawPlot, scale = 2, units = "in", width = 7, height = 3)
    })
}

handleSaveSummaryDataAsCsv = function(input, output){

    output$saveSummary = downloadHandler(.getCsvFilename("summary_data"), content = function(filename){
      clipped = SensorData.clip(rValues$summaryData, rValues$xlim[1], rValues$xlim[2])
      write.csv(clipped, filename, sep = ",", quote = FALSE, row.names = FALSE)
    })

    output$saveClippedRawData = downloadHandler(.getCsvFilename("raw_data"), content = function(filename){
      clipped = SensorData.clip(rValues$rawData, rValues$raw_xlim[1], rValues$raw_xlim[2])
      clipped = SensorData.offset(clipped, offsetValue = as.numeric(input$rawDataOffset))
      write.csv(clipped, filename, sep = ",", quote = FALSE, row.names = FALSE)
    })
}

handleShowRawPlot = function(input, session){
  observeEvent(input$showRawPlot, {
    tdiff = as.numeric(rValues$xlim[2]) - as.numeric(rValues$xlim[1])
    if(tdiff > 5 * 60){#greater than five minutes
      createAlert(
        session, "alert", alertId = "too long for raw plot",
        title = "Raw plot is not available",
        content = "Raw plot will only be available for period within 5 minutes", style = "warning"
      )
    }else{
      # extract the hourly folder
      startHour = format(rValues$xlim[1], "%H")
      endHour = format(rValues$xlim[2], "%H")
      inputFolder = file.path(
        rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, unique(c(startHour, endHour)))
      sensorFiles = list.files(path = inputFolder, full.names = TRUE, recursive = TRUE)
      selectedFiles = sensorFiles[str_detect(sensorFiles, input$sensorSelect)]
      withProgress(message = "Import selected sensor data", value = 0.1, {
        dataList = foreach(filename = selectedFiles, .export = "incProgress") %do% {
          incProgress(message = paste("Importing", basename(filename)))
          SensorData.importCsv(filename)
        }
        incProgress(message = "Merge imported data")
        merged = SensorData.merge(dataList)
        merged = SensorData.cleanup(merged)
        merged = SensorData.offset(merged, offsetValue = as.numeric(input$rawDataOffset))
        incProgress(message = "Clip merged data")
        cliped = SensorData.clip(merged, rValues$xlim[1], rValues$xlim[2])
        rValues$rawData = cliped
        rValues$begin_xrange_raw = c(
          min(rValues$rawData[,1], na.rm = TRUE),
          max(rValues$rawData[,1], na.rm = TRUE)
        )
        rValues$raw_xlim = as.POSIXct(rValues$begin_xrange_raw, origin = "1970-01-01")
        shinyjs::show("raw_plot_box")
      })
    }
  })
}

.getPdfFilename = function(filename){
  paste(filename,Sys.Date(),"pdf", sep = ".")
}

.getCsvFilename = function(filename){
  paste(filename,Sys.Date(),"csv", sep = ".")
}

library(stringr)
library(plyr)
.getDisplayNames = function(listOfFilenames) {
  displayableNames = llply(listOfFilenames, function(name) {
    name = basename(name)
    tokens = str_split(name, "\\.")
    return(paste(tokens[[1]][1],str_split(tokens[[1]][2], "-")[[1]][1], sep = "."))
  })
  displayableNames = unique(displayableNames)
  return(displayableNames)
}

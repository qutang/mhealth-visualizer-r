generalHandler = function(input){


  shinyjs::disable("monthSelect")
  shinyjs::disable("daySelect")
  shinyjs::disable("hourSelect")
  shinyjs::disable("sensorSelect")
  shinyjs::disable("annotationSelect")
  shinyjs::disable("eventSelect")
  if(is.null(input$datasetFolder)){
    shinyjs::hide("subjectId")
    shinyjs::disable("yearSelect")
  }
}

handlePlotHover = function(input){
  observeEvent(input$plot_hover,{
    currentTime = as.POSIXct(input$plot_hover$x, origin = "1970-01-01")
    if(!is.null(rValues$annotationData)){
      labelNames = AnnotationData.getLabelNames(rValues$annotationData, currentTime)
    }else{
      labelNames = NULL
    }
    rValues$currentAnnotations = labelNames
  })
}

handlePlotDoubleClick = function(input){
  observeEvent(input$plot_dblclick, {
    rValues$xlim = rValues$begin_xrange
  })
}

handlePlotBrush = function(input){
  observeEvent(input$plot_brush, {
    brush = input$plot_brush
    if (!is.null(brush)) {
      rValues$xlim = c(
        as.POSIXct(brush$xmin, origin = "1970-01-01"),
        as.POSIXct(brush$xmax, origin = "1970-01-01")
      )
    }
  })
}

handleDatasetFolderChosen = function(input, session, volumes){
  observeEvent(input$datasetFolder, {
    folder = parseDirPath(volumes, input$datasetFolder)
    rValues$masterFolder = file.path(folder, "MasterSynced")
    if(is.null(folder)){
      shinyjs::hide("subjectId")
    }else if(dir.exists(rValues$masterFolder)){ # mhealth subject directory
      shinyjs::text("subjectId", paste0("Subject ID:", basename(folder)))
      shinyjs::show("subjectId")
      years = list.dirs(rValues$masterFolder, full.names = FALSE, recursive = FALSE)
      updateSelectInput(session = session, inputId = "yearSelect", choices = years)
      shinyjs::enable("yearSelect")
    }else{

    }
  })
}

handleYearSelection = function(input, session){
  observeEvent(input$yearSelect, {
    if(!is.null(input$yearSelect) && !is.null(rValues$masterFolder)){
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

handleMonthSelection = function(input, session){
  observeEvent(input$monthSelect, {
    if(!is.null(input$monthSelect) && !is.null(rValues$masterFolder) && !is.null(input$yearSelect)){
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
handleDaySelection = function(input, session){
  observeEvent(input$daySelect, {
    if(!is.null(input$monthSelect) && !is.null(rValues$masterFolder)
       && !is.null(input$yearSelect) && !is.null(input$daySelect)){
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


handleHourSelection = function(input, session){
  observeEvent(input$hourSelect, {
    if(!is.null(input$monthSelect) && !is.null(rValues$masterFolder)
       && !is.null(input$yearSelect) && !is.null(input$daySelect) &&
       !is.null(input$hourSelect)){
      if(input$hourSelect != "All"){
        hourFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect)
      }
      sensors = list.files(hourFolder, full.names = FALSE, recursive = FALSE, pattern = "*.sensor.csv*")
      annotations = list.files(hourFolder, full.names = FALSE, recursive = FALSE, pattern = "*.annotation.csv*")
      events = list.files(hourFolder, full.names = FALSE, recursive = FALSE, pattern = "*.event.csv*")

      if(length(sensors) > 0){
        updateSelectInput(session = session, inputId = "sensorSelect", choices = .getDisplayNames(sensors))
        shinyjs::enable("sensorSelect")
      }
      if(length(annotations) > 0){
        updateSelectInput(session, "annotationSelect", choices = .getDisplayNames(annotations))
        shinyjs::enable("annotationSelect")
      }
      if(length(annotations) > 0){
        updateSelectInput(session, "eventSelect", choices = .getDisplayNames(events))
        shinyjs::enable("eventSelect")
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
handleComputeSummaryClicked = function(input, session){
  observeEvent(input$computeSummary, {
    if(is.null(input$sensorSelect)){
      createAlert(session, "alert", alertId = "no sensor selected",
                  title = "No sensor data selected",
                  content = "Please select at least one sensor data file", style = "warning")
    }else{
      inputFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect)
      sensorFiles = list.files(path = inputFolder, full.names = TRUE, recursive = FALSE)

      selectedFiles = sensorFiles[str_detect(sensorFiles, input$sensorSelect)]

      if(length(selectedFiles) >= 4){
          #do in parrallel
      }else{
        withProgress(message = "Import sensor data", value = 0.1, {
          merged <<- foreach(filename = selectedFiles, .combine = "SensorData.merge", .export = "incProgress") %do% {
            SensorData.importCsv(filename)
          }
          withProgress(message = "Compute summary data", value = 0.6, {
            method = input$summaryMethod
            valueType = input$summaryValue
            switch(valueType,
                   magnitude = {merged = Magnitude.compute(merged)})
            switch(method,
                   mean = {rValues$summaryData = SummaryData.simpleMean(merged, breaks = "sec")},
                   AUC ={rValues$summaryData = SummaryData.auc(merged, breaks = "sec")})
            rValues$begin_xrange= c(min(rValues$summaryData[,1], rValues$annotationData[,2]),
                               max(rValues$summaryData[,1], rValues$annotationData[,3]))
            setProgress(value = 1, message = "Summary data is ready")
            createAlert(session, "alert", "summaryData", title = "Summary Data is ready", style = "info")
          })
        })
      }
    }
  })
}

handleAnnotationSelect = function(input, session){
  observeEvent(input$annotationSelect, {
    inputFolder = file.path(rValues$masterFolder, input$yearSelect, input$monthSelect, input$daySelect, input$hourSelect)
    annotationFiles = list.files(path = inputFolder, full.names = TRUE, recursive = FALSE)

    selectedFiles = annotationFiles[str_detect(annotationFiles, input$annotationSelect)]

    withProgress(message = "Import annotation data", value = 0.1, {
      mergedAnnotation = foreach(filename = selectedFiles, .combine = "SensorData.merge", .export = "incProgress") %do% {
        AnnotationData.importCsv(filename)
      }
      rValues$annotationData = mergedAnnotation
    })
  })
}

library(stringr)
library(plyr)
.getDisplayNames = function(listOfFilenames){
  displayableNames= llply(listOfFilenames, function(name){
    tokens = str_split(name, "\\.")
    return(paste(tokens[[1]][1],str_split(tokens[[1]][2], "-")[[1]][1], sep = "."))
  })
  return(displayableNames)
}

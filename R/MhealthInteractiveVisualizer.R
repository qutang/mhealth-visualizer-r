#' @name MhealthInteractiveVisualizer.run
#' @title An interactive visualizer for mhealth format data based on shiny
#' @import shiny
#' @export
MhealthInteractiveVisualizer.run = function(volumes) {
  if(missing(volumes)){
    runApp(MhealthInteractiveVisualizer.getApp(), port = 10001)
  }else{
    runApp(MhealthInteractiveVisualizer.getApp(volumes), port = 10001)
  }
}

#' @name MhealthInteractiveVisualizer.getApp
#' @title The shiny app object of interactive visualizer for mhealth format data
#' @description This can be used if you want to embed the interactive visualizer in something else
#' @import shiny
#' @export
MhealthInteractiveVisualizer.getApp = function(volumes = NULL){
  appFolder = system.file('InteractiveVisualizer', package='mHealthVisualizer')
  source(file.path(appFolder, "server.R"), local = TRUE)
  source(file.path(appFolder, "ui.R"), local = TRUE)
  app = shinyApp(ui = ui, server = server)
  return(app)
}

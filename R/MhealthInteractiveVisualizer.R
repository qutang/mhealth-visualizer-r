#' @name MhealthInteractiveVisualizer.run
#' @title An interactive visualizer for mhealth format data based on shiny
#' @import shiny
#' @export
MhealthInteractiveVisualizer.run = function() {
  shiny::runApp(system.file('InteractiveVisualizer', package='mhealthformatsupportr', mustWork=T))
}

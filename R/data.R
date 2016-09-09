#' Walking and running accelerometer data collected using Actigraph GT9X at three/four different body locations
#'
#' A dataset containing raw segments of accelerometer data collected using GT9X at three/four different body locations for different speeds. GT9X has G range +/-8G.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{timestamp for data sample, POXIct}
#'   \item{X_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on x axis in g force, double}
#'   \item{Y_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on y axis in g force, double}
#'   \item{Z_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on z axis in g force, double}
#'   \item{LOCATION}{body locations in Camel style, factor}
#'   \item{SR}{sampling rate, factor}
#'   \item{MPH}{speed in mile per hour, double}
#'   \item{WEIGHT}{carried weight in pounds, factor}
#'   \item{SUBJECT}{subject id, factor}
#'   \item{SESSION}{trial session for a subject, factor}
#' }
#' @source \url{http://qutang.github.io}
"walkrun1"

#' Walking and running accelerometer actigraph count data collected using Actigraph GT9X at three/four different body locations
#'
#' A dataset containing actigraph count of accelerometer data collected using GT9X at three/four different body locations for different speeds. 
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{timestamp for data sample, POXIct}
#'   \item{X_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on x axis in g force, double}
#'   \item{Y_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on y axis in g force, double}
#'   \item{Z_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on z axis in g force, double}
#'   \item{EPOCH}{epoch length for current actigraph count value in second, factor}
#'   \item{LOCATION}{body locations in Camel style, factor}
#'   \item{SR}{sampling rate, factor}
#'   \item{MPH}{speed in mile per hour, double}
#'   \item{WEIGHT}{carried weight in pounds, factor}
#'   \item{SUBJECT}{subject id, factor}
#'   \item{SESSION}{trial session for a subject, factor}
#' }
#' @source \url{http://qutang.github.io}
"walkrun1_actigraph"

#' Walking and running accelerometer data collected using Actigraph GT3X at dominant waist
#'
#' A dataset containing raw segments of accelerometer data collected using GT3X at dominant waist for different speeds. GT3X has G range +/-3G.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{timestamp for data sample, POXIct}
#'   \item{X_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on x axis in g force, double}
#'   \item{Y_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on y axis in g force, double}
#'   \item{Z_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on z axis in g force, double}
#'   \item{LOCATION}{body locations in Camel style, factor}
#'   \item{SR}{sampling rate, factor}
#'   \item{MPH}{speed in mile per hour, double}
#'   \item{WEIGHT}{carried weight in pounds, factor}
#'   \item{SUBJECT}{subject id, factor}
#'   \item{SESSION}{trial session for a subject, factor}
#' }
#' @source \url{http://qutang.github.io}
"walkrun2"

#' Accelerometer data collected using Actigraph GT3XBT and GT9X on a ecliptical shaker ranging from 240 RPM to 480RPM
#'
#' A dataset containing raw segments of accelerometer data collected using GT3XBT (3 devices) and GT9X (1 device) on an ecliptical shaker ranging from 60RPM to 480RPM at sampling rate from 30Hz to 100Hz. The g range for both devices are +/- 8G. z-axis is facing up on the shaker.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{timestamp for data sample, POXIct}
#'   \item{X_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on x axis in g force, double}
#'   \item{Y_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on y axis in g force, double}
#'   \item{Z_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on z axis in g force, double}
#'   \item{LOCATION}{"Shaker", char}
#'   \item{SR}{sampling rate, char}
#'   \item{RPM}{RPM of the shaker in Hz, double}
#'   \item{DEVICE}{device id: MOS is for GT3XBT, TAS is for GT9X, char}
#'   \item{GRANGE}{G range of the device, char}
#' }
#' @source \url{http://qutang.github.io}
"shaker1"

#' Accelerometer data collected using Actigraph GT3X, GT3X+, GT3XBT and GT9X on a ecliptical shaker ranging from 240 RPM to 480RPM
#'
#' A dataset containing raw segments of accelerometer data collected using GT3X, GT3X+, GT3XBT and GT9X on an ecliptical shaker ranging from 60RPM to 480RPM each sampling rate 30Hz, 80Hz, 80Hz and 100Hz. The g range for each is +/-3g, +/-6g, +/-8g and +/-16g.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{timestamp for data sample, POXIct}
#'   \item{X_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on x axis in g force, double}
#'   \item{Y_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on y axis in g force, double}
#'   \item{Z_ACCELERATION_METERS_PER_SECOND_SQUARED}{calibrated acceleration value on z axis in g force, double}
#'   \item{LOCATION}{"Shaker", char}
#'   \item{SR}{sampling rate, char}
#'   \item{RPM}{RPM of the shaker in Hz, double}
#'   \item{DEVICE}{device id: MAT is for GT3X, CLE is for GT3X+, MOS is for GT3XBT, TAS is for GT9X, char}
#'   \item{GRANGE}{G range of the device, char}
#' }
#' @source \url{http://qutang.github.io}
"shaker2"
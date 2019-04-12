#' View data laid out in microtitre plate.
#' Tooltip enabled by d3 and r2d3.
#'
#' @param tbl table of data to use which must have well col in 'A01' style.
#' @param fillVar name of variable in tbl to use to map the fill of each well to
#' @param fillOpacityVar name of variable in tbl to use to map the fill opacity to
#' @param width width of the resulting mtp
#' @param height height of the resulting mtp
#'
#' @export
mtp_view <- function(tbl, fillVar = NULL, fillOpacityVar = NULL, width = NULL, height = NULL) {
    options <- list(fillVar = fillVar, fillOpacityVar = fillOpacityVar)
    r2d3::r2d3(
        data = tbl,
        script = system.file("mtpview.js", package = "mtpview"),
        width = width,
        height = height,
        options = options)
}


mtp_view2 <- function(tbl, fillVar = NULL, fillOpacityVar = NULL, width = NULL, height = NULL) {

    options <- list(nRows = 8,
                    nCols = 12,
                    wellShape = "rect",
                    fillVar = fillVar,
                    fillOpacityVar = fillOpacityVar,
                    borderStroke = "",
                    labelStroke = "")
    r2d3::r2d3(
        data = tbl,
        script = system.file("mtpview.js", package = "mtpview"),
        width = width,
        height = height,
        options = options)
}

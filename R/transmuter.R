#' return a one-row-per-well tibble of a microtitre plate with n rows and n cols.
#'
#' @param nrow number of rows in microtitre plate
#' @param ncol number of columns in microtitre plate
#'
#' @examples
#'     mtp_wells_tbl(6, 8)
#'     mtp_wells_tbl(8, 12)
#'     mtp_wells_tbl(16, 24)
#'     mtp_wells_tbl(32, 48)
#'     mtp_wells_tbl(64, 96)
#' @export
mtp_wells_tbl <- function(nrow, ncol) {
    stopifnot(is.numeric(nrow), is.numeric(ncol))
    n <- nrow * ncol
    tibble::tibble(
        id = 1:n,
        label = well_labels_from_dims(nrow, ncol),
        row = rep(1:nrow, each = ncol),
        col = rep(1:ncol, nrow))
}

well_labels_from_dims <- function(nrow, ncol, col_nchar = nchar(ncol)) {
    stopifnot(is.numeric(nrow), is.numeric(ncol))
    fmt <- paste0("%s%0",nchar(ncol),"d")
    sprintf(fmt, rep(row_labels(nrow), each = ncol), rep(1:ncol, nrow))
}

row_labels <- function(nrow) {
    labels <- c(LETTERS, paste0(LETTERS[1], LETTERS), paste0(LETTERS[2], LETTERS))
    labels[1:nrow]
}

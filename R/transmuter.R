# this file contains all the utility / transmuting function for messing around
# with microtire plates. if I figure out a design i like i can export it to the
# pkg's api and have the functions be available for use by others.


mtp_wells_from_dims <- function(nrow, ncol) {
    paste0(rep(LETTERS[1:nrow], each = ncol), sprintf("%02d",rep(1:ncol, nrow)))
}




#' given the dimensions in rows and cols output
#' a tibble with row, col, well, well_alias vars
#'
#' @param n_rows <int> n_rows
#' @param n_cols <int> n_cols
#'
#' @examples
#'   empty_plate_with_dims(4, 6)
#'   empty_plate_with_dims(6, 8)
#'   empty_plate_with_dims(8, 12)
#'   empty_plate_with_dims(16, 24)
empty_plate_with_dims <- function(n_rows, n_cols){
    d <- expand.grid(row = LETTERS[seq_len(n_rows)], col = seq_len(n_cols), stringsAsFactors = F)
    d <- tibble::as_tibble(d)
    dplyr::mutate(d, well = dplyr::row_number(row),
                  well_alias = sprintf("%s%02.f", row, col))
}

#' given the number of wells in a plate output
#' a tibble with row, col, well, well_alias vars
#'
#'@param n_wells <int> number of wells
#'
#' @examples
#'  empty_n_well_plate(96)
empty_n_well_plate <- function(n_wells) {
    message("not implemented yet.")
}

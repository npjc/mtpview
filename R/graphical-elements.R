#' footprint of microtitre plate from width and height
#'
#' @param w <num> width of plate
#' @param h <num> height of plate
#' @param r <num> radius of circle used to round corners. currently not used.
#'
#' @return A [tibble()] of length 1. Each row is a rect with variables: [x],
#'   [y], [w], [h], [rx], [ry], [xmin], [xmax], [ymin], [ymax].
#'
#' @examples
#'     make_footprint(127.76, 87.45) # SBS plate footprint.
make_footprint <- function(w, h, r = 3) {
    tibble::tibble(
        x = 0,
        y = 0,
        w = w,
        h = h,
        rx = r,
        ry = r,
        xmin = x,
        xmax = x + w,
        ymin = y,
        ymax = y + h
        )
}

make_footprint_poly <- function(w, h) {
    tibble::tribble(
        ~x, ~y,
        0, 0,
        w,0,
        w,h,
        0,h
    )
}


#' notched well border of microtitre plate from width and height
#'
#' @param w <num> width of plate
#' @param h <num> height of plate
#'
#' @return A [tibble()] of length 7. Each row is a point with coordinates: [x],
#'   [y].
#'
#' @examples
#'     make_notched_border(127.76, 87.45) # SBS plate footprint.
make_notched_border <- function(w, h) {
    h_margin <- 2
    v_margin <- 2
    notch_offset_x <- 5
    notch_offset_y <- 5
    start_point <- c(h_margin + notch_offset_x, v_margin)
    top_right_corner <- c(w - h_margin, v_margin)
    btm_right_corner <- c(w - h_margin, h - v_margin)
    btm_left_corner <- c(h_margin + notch_offset_x, h - v_margin)
    btm_left_corner_notch_offset <- c(h_margin, h - v_margin - notch_offset_y)
    top_left_corner_notch_offset <- c(h_margin, v_margin + notch_offset_y)
    back_to_start <- start_point
    l <- list(start_point,
              top_right_corner,
              btm_right_corner,
              btm_left_corner,
              btm_left_corner_notch_offset,
              top_left_corner_notch_offset,
              start_point)
    tibble::tibble(
        x = unlist(lapply(l, `[`, 1)),
        y = unlist(lapply(l, `[`, 2))
    )
}

#' Make rectangular wells of a microtire plate from trained scales.
#'
#' @param xs <list> bandwidth scale trained on plate height and n cols
#' @param ys <list> bandwidth scale trained on plate width and n rows
#'
#' @return A [tibble()] with variables: [xmin], [xmax], [ymin], [ymax].
#'
make_wells <- function(xs, ys) {
    d <- wells_tbl(nrow = ys$n, ncol = xs$n)
    dplyr::mutate(d,
                       xmin = xs$values[col],
                       xmax = xmin + xs$bandwidth,
                       ymin = ys$values[row],
                       ymax = ymin + ys$bandwidth)
}

make_wells_y_reverse <- function(xs, ys) {
    d <- wells_tbl(nrow = ys$n, ncol = xs$n)

    dplyr::mutate(d,
                  xmin = xs$values[col],
                  xmax = xmin + xs$bandwidth,
                  ymin = rev(ys$values)[row],
                  ymax = ymin - ys$bandwidth)
}

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
#'
wells_tbl <- function(nrow, ncol, col_nchar = nchar(ncol)) {
    stopifnot(is.numeric(nrow), is.numeric(ncol))
    n <- nrow * ncol
    tibble::tibble(
        id = seq_len(n),
        well = well_labels(nrow, ncol, col_nchar),
        row = rep(seq_len(nrow), each = ncol),
        col = rep(seq_len(ncol), nrow))
}

well_labels <- function(nrow, ncol, col_nchar = nchar(ncol)) {
    fmt <- paste0("%s%0",col_nchar,"d")
    sprintf(fmt, rep(row_labels(seq_len(nrow)), each = ncol), rep(seq_len(ncol), nrow))
}

row_labels <- function(row) {
    labels <- c(LETTERS, paste0(LETTERS[1], LETTERS), paste0(LETTERS[2], LETTERS))
    labels[row]
}

#' make row labels for a microtitre plate from a set of scales
#'
#' @param xs    <list> bandwidth scale trained on plate height and n cols
#' @param ys    <list> bandwidth scale trained on plate width and n rows
#' @param nudge_x <num>  amount to nudge the labels by, defaults to 0.
#'
#' @return A [tibble()] of length nrows. One row per label with [label]
#'   positioned at point [x], [y].
#'
make_row_labels <- function(xs, ys, nudge_x = 0) {
    rows <- seq_len(ys$n)
    tibble::tibble(
        row = rows,
        x = ((xs$step * xs$pad_out) / 2) + nudge_x,
        y = ys$values + ys$bandwidth / 2,
        # y = c(0, cumsum(rep.int(ys$step, ys$n - 1))) + ys$step * ys$pad_out + ys$step / 2,
        label = row_labels(rows) # lookup of stored labels for rows... in 'A' style
    )
}

#' @rdname make_row_labels
#'
#' @param nudge_y <num>  amount to nudge the labels by, defaults to 0.
#'
make_col_labels <- function(xs, ys, nudge_y = 0) {
    cols <- seq_len(xs$n)
    tibble::tibble(
        col = cols,
        x = xs$values + xs$bandwidth / 2,
        # x = xs$step * xs$pad_out + cols * xs$step - xs$step / 2,
        y = ys$range - ((ys$step * ys$pad_out) / 2) + nudge_y,
        label = as.character(cols)
    )

}

#' Make a bandscale
#'
#' @param range   <num> length of the range of the data
#' @param n       <num> number of steps in the range
#' @param pad_in  <num> fraction of step size to pad in between bands, defaults
#'   to 0.
#' @param pad_out <num> fraction of step size to pad outside of first and last
#'   band, defaults to 0.
#' @param reverse <lgl> if the values of the scale should be reversed. Useful for y-down vs . y-up
#'
#' @return A [list()] with elements: [range], [n], [step], [bandwidth], [pad_in]
#'   , [pad_out], [first], [values].
#'
#' @examples
#'     make_bandscale(127.76, 12, 0.02, 0.17) # column bands for 96 well plate
#'     make_bandscale(87.45, 8, 0.02, 0.245) # row bands for 96 well plate
make_bandscale <- function(range, n, pad_in = 0, pad_out = 0, reverse = F) {
    step <- range / ((2 * pad_out) - pad_in + n)
    first <- step * pad_out
    values <- c(first, seq(1, n - 1) * step + first)
    if (reverse)
        values <- rev(values)
    list(
        range = range,
        n = n,
        step = step,
        bandwidth = step - (step * pad_in),
        pad_in = pad_in,
        pad_out = pad_out,
        first = first,
        values = values
    )
}


# functions for use in bioscreen plate generation -------------------------

make_notched_border_bioscreen = function(w, h) {
    tibble::tribble( ~ x, ~ y,
                     0, 0,
                     w, 0,
                     w, h,
                     7, h,
                     0, h - 7,
                     0, 0)

}

make_wells_bioscreen = function(xs, ys) {
    rows <- rep(c(seq(1, 20, by = 2), seq(2, 20, by = 2)), 5)
    cols <- rep(seq_len(10), each = 10)
    tibble::tibble(
        id = seq_len(100L),
        well = as.character(id),
        row = rep(seq_len(10), 10),
        col = cols,
        xmin = xs$values[cols],
        xmax = xmin + xs$bandwidth,
        ymin = ys$values[rows],
        ymax = ymin + ys$bandwidth
    )
}

make_row_labels_bioscreen = function(xs, ys, nudge_x = 0) {
    tibble::tibble(
        row = seq_len(ys$n / 2),
        x = ((xs$step * xs$pad_out) / 2) + nudge_x,
        y = ((ys$values[seq(1, 20, by  = 2)] + ys$values[seq(2, 20, by  = 2)]) / 2) + (ys$step / 2),
        label = as.character(row)
    )
}

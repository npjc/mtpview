mtp_spec <- function(nrow,
                     ncol,
                     w,
                     h,
                     pad_in = NA,
                     pad_out = NA,
                     pad_in_col = pad_in,
                     pad_in_row = pad_in,
                     pad_out_col = pad_out,
                     pad_out_row = pad_out,
                     label_nudge = NA,
                     row_label_nudge = label_nudge,
                     col_label_nudge = label_nudge
                     # label_size,
                     # row_label_size = label_size,
                     # col_label_size = label_size,
                     # label_color,
                     # row_label_color = label_color,
                     # col_label_color = label_color,
                     # well_border_size = border_size,
                     # well_border_color,
                     # well_fill_color,
                     # well_text_size = label_size,
                     # well_text_color = text_color,
                     ) {

    structure(
        list(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in_col =  pad_in_col,
             pad_in_row =  pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge),
        class = "mtp_spec"
    )
}

#' @export
mtp_spec_6well <- function(nrow = 2L,
                            ncol = 3L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = 0.02,
                            pad_out = NA,
                            pad_in_col = pad_in,
                            pad_in_row = pad_in,
                            pad_out_col = 0.245,
                            pad_out_row = 0.17,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_12well <- function(nrow = 3L,
                            ncol = 4L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = 0.02,
                            pad_out = NA,
                            pad_in_col = pad_in,
                            pad_in_row = pad_in,
                            pad_out_col = 0.245,
                            pad_out_row = 0.17,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_24well <- function(nrow = 4L,
                            ncol = 6L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = 0.02,
                            pad_out = NA,
                            pad_in_col = pad_in,
                            pad_in_row = pad_in,
                            pad_out_col = 0.44,
                            pad_out_row = 0.3,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_48well <- function(nrow = 6L,
                            ncol = 8L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = 0.02,
                            pad_out = NA,
                            pad_in_col = pad_in,
                            pad_in_row = pad_in,
                            pad_out_col = 1.24,
                            pad_out_row = 0.5,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_96well <- function(nrow = 8L,
                            ncol = 12L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = 0.01,
                            pad_out = NA,
                            pad_in_col = pad_in,
                            pad_in_row = pad_in,
                            pad_out_col = 0.95,
                            pad_out_row = 0.652,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_384well <- function(nrow = 16L,
                            ncol = 24L,
                            w = 127.76,
                            h = 85.48,
                            pad_in = NA,
                            pad_out = NA,
                            pad_in_col = 0.01,
                            pad_in_row = 0.02,
                            pad_out_col = 1.78,
                            pad_out_row = 1.305,
                            label_nudge = NA,
                            row_label_nudge = 1,
                            col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

#' @export
mtp_spec_1536well <- function(nrow = 32L,
                             ncol = 48L,
                             w = 127.76,
                             h = 85.48,
                             pad_in = NA,
                             pad_out = NA,
                             pad_in_col = 0.01,
                             pad_in_row = 0.01,
                             pad_out_col = 3.9,
                             pad_out_row = 2.61,
                             label_nudge = NA,
                             row_label_nudge = 1,
                             col_label_nudge = -1) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

mtp_spec_bioscreen <- function(nrow = 20L,
                              ncol = 10L,
                              w = 85.48,
                              h = 127.76,
                              pad_in = NA,
                              pad_out = NA,
                              pad_in_col = 0.03,
                              pad_in_row = -0.2295,
                              pad_out_col = 0.75,
                              pad_out_row = 0.75,
                              label_nudge = NA,
                              row_label_nudge = 1,
                              col_label_nudge = 0) {

    mtp_spec(nrow = nrow,
             ncol = ncol,
             w = w,
             h = h,
             pad_in = pad_in,
             pad_out = pad_out,
             pad_in_col = pad_in_col,
             pad_in_row = pad_in_row,
             pad_out_col = pad_out_col,
             pad_out_row = pad_out_row,
             label_nudge = label_nudge,
             row_label_nudge = row_label_nudge,
             col_label_nudge = col_label_nudge)

}

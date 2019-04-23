GeomFootprint <- ggplot2::ggproto("GeomFootprint", ggplot2::GeomRect,

                           default_aes = ggplot2::aes(
                               colour = "grey40", fill = "grey96", size = 0.2,
                               linetype = 1, alpha = NA, w = NA, h = NA
                           ),
                           setup_data = function(data, params) {
                               footprint <- make_footprint(params$w, params$h)
                               data <- dplyr::group_by(data, plate)
                               data <- dplyr::filter(data, dplyr::row_number() == 1)
                               data <- tidyr::crossing(data, footprint)
                               dplyr::ungroup(data)
                           },
                         required_aes = c("plate")
)

#' @export
geom_footprint <- function(mapping = NULL, data = NULL, stat = "identity",
                           geom = GeomFootprint,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, w = NA, h = NA, ...) {
    ggplot2::layer(
        geom = geom, mapping = mapping,  data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, w = w, h = h, ...), layer_class = LayerMtp
    )
}


GeomNotchedBorder <- ggplot2::ggproto("GeomNotchedBorder", ggplot2::GeomPath,
                             required_aes = c("plate"),

                             default_aes = ggplot2::aes(
                                 colour = "grey40", size = 0.2,
                                 linetype = 1, alpha = NA, w = NA, h = NA, fill = NA
                             ),
                             setup_data = function(self, data, params) {
                                 notched_border <- make_notched_border(params$w, params$h)
                                 data <- dplyr::group_by(data, plate)
                                 data <- dplyr::filter(data, dplyr::row_number() == 1)
                                 data <- tidyr::crossing(data, notched_border)
                                 dplyr::ungroup(data)
                             }

)

#' @export
geom_notched_border <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", geom = GeomNotchedBorder, na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, w = NA, h = NA, ...) {
    ggplot2::layer(
        geom = geom, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, w = w, h = h, ...), layer_class = LayerMtp
    )
}

GeomWellRect <- ggplot2::ggproto("GeomWellRect", ggplot2::GeomRect,
                        extra_params = c("na.rm"),

                        default_aes = ggplot2::aes(fill = "grey40", colour = "grey40", size = 0.1, linetype = 1,
                                          alpha = NA,
                                          nrow = NA,
                                          ncol = NA,
                                          w = NA,
                                          h = NA,
                                          pad_in_col = NA,
                                          pad_in_row = NA,
                                          pad_out_col = NA,
                                          pad_out_row = NA),

                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            wells <- make_wells(xs, ys)
                            data <- dplyr::group_by(data, plate, well)
                            data <- dplyr::filter(data, dplyr::row_number() == 1)
                            data <- dplyr::ungroup(data)
                            dplyr::left_join(data, wells, by = c("well"))
                        },


                        required_aes = c("plate","well")
)

#' @export
geom_well_rect <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           geom = GeomWellRect,
                           nrow = NA, ncol = NA, w = NA, h = NA,
                           pad_in_col = NA, pad_in_row = NA, pad_out_col = NA,
                           pad_out_row = NA,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            nrow = nrow,
            ncol = ncol,
            w = w,
            h = h,
            pad_in_col = pad_in_col,
            pad_in_row = pad_in_row,
            pad_out_col = pad_out_col,
            pad_out_row = pad_out_row,
            ...
        ),
        layer_class = LayerMtp
    )
}

#' @export
geom_row_label <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           geom = GeomRowLabel,
                           nrow = NA,
                           ncol = NA,
                           w = NA,
                           h = NA,
                           pad_in_col = NA,
                           pad_in_row = NA,
                           pad_out_col = NA,
                           pad_out_row = NA,
                           row_label_nudge = NA,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            nrow = nrow,
            ncol = ncol,
            w = w,
            h = h,
            pad_in_col = pad_in_col,
            pad_in_row = pad_in_row,
            pad_out_col = pad_out_col,
            pad_out_row = pad_out_row,
            row_label_nudge = row_label_nudge,
            ...
        ),
        layer_class = LayerMtp
    )
}


GeomRowLabel <- ggplot2::ggproto("GeomRowLabel", ggplot2::GeomText,
                        extra_params = c("na.rm"),

                        required_aes = c("plate"),

                        # These aes columns are created by setup_data(). They need to be listed here so
                        # that GeomTexturedRect$handle_na() properly removes any bars that fall outside the defined
                        # limits, not just those for which x and y are outside the limits
                        non_missing_aes = c("x", "y", "label"),

                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            row_labels <- make_row_labels(xs, ys, params$row_label_nudge)
                            # could do something about grouping by row here
                            # if I have access to that info?
                            data <- dplyr::group_by(data, plate)
                            data <- dplyr::filter(data, dplyr::row_number() == 1)
                            data <- tidyr::crossing(data, row_labels)
                            dplyr::ungroup(data)
                        },
                        default_aes = ggplot2::aes(
                            colour = "grey40", size = 4, angle = 0, hjust = 0.5,
                            vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                            nrow = NA,
                            ncol = NA,
                            w = NA,
                            h = NA,
                            pad_in_col = NA,
                            pad_in_row = NA,
                            pad_out_col = NA,
                            pad_out_row = NA,
                            row_label_nudge = NA
                        )
                        )

#' @export
geom_col_label <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           nrow = NA,
                           ncol = NA,
                           w = NA,
                           h = NA,
                           pad_in_col = NA,
                           pad_in_row = NA,
                           pad_out_col = NA,
                           pad_out_row = NA,
                           col_label_nudge = NA,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomColLabel,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            nrow = nrow,
            ncol = ncol,
            w = w,
            h = h,
            pad_in_col = pad_in_col,
            pad_in_row = pad_in_row,
            pad_out_col = pad_out_col,
            pad_out_row = pad_out_row,
            col_label_nudge = col_label_nudge,
            ...
        ),
        layer_class = LayerMtp
    )
}

GeomColLabel <- ggplot2::ggproto("GeomColLabel", ggplot2::GeomText,
                        extra_params = c("na.rm"),

                        required_aes = c("plate"),

                        default_aes = ggplot2::aes(
                            colour = "grey40", size = 4, angle = 0, hjust = 0.5,
                            vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                            nrow = NA,
                            ncol = NA,
                            w = NA,
                            h = NA,
                            pad_in_col = NA,
                            pad_in_row = NA,
                            pad_out_col = NA,
                            pad_out_row = NA,
                            col_label_nudge = NA
                        ),

                        # These aes columns are created by setup_data(). They need to be listed here so
                        # that GeomTexturedRect$handle_na() properly removes any bars that fall outside the defined
                        # limits, not just those for which x and y are outside the limits
                        non_missing_aes = c("x", "y", "label"),

                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            col_labels <- make_col_labels(xs, ys, params$col_label_nudge)
                            data <- dplyr::group_by(data, plate)
                            data <- dplyr::filter(data, dplyr::row_number() == 1)
                            data <- tidyr::crossing(data, col_labels)
                            dplyr::ungroup(data)
                        }
)


# well text ---------------------------------------------------------------

#' @export
geom_well_text <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           geom = GeomWellText,
                           nrow = NA, ncol = NA, w = NA, h = NA,
                           pad_in_col = NA, pad_in_row = NA, pad_out_col = NA,
                           pad_out_row = NA,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            nrow = nrow,
            ncol = ncol,
            w = w,
            h = h,
            pad_in_col = pad_in_col,
            pad_in_row = pad_in_row,
            pad_out_col = pad_out_col,
            pad_out_row = pad_out_row,
            ...
        ),
        layer_class = LayerMtp
    )
}

GeomWellText <- ggplot2::ggproto("GeomWellText", ggplot2::GeomText,
                                 extra_params = c("na.rm"),

                                 default_aes = ggplot2::aes(colour = "grey40", size = 3, angle = 0, hjust = "left",
                                                            vjust = "top", alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                                                            nrow = NA,
                                                            ncol = NA,
                                                            w = NA,
                                                            h = NA,
                                                            pad_in_col = NA,
                                                            pad_in_row = NA,
                                                            pad_out_col = NA,
                                                            pad_out_row = NA,
                                                            row_label_nudge = NA),

                                 non_missing_aes = c("x", "y"),

                                 setup_data = function(data, params) {
                                     xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                                     ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                                     wells <- make_wells(xs, ys)
                                     data <- dplyr::group_by(data, well)
                                     data <- dplyr::filter(data, dplyr::row_number() == 1)
                                     data <- dplyr::ungroup(data)
                                     data <- dplyr::left_join(data, wells, by = c("well"))
                                     dplyr::rename(data, x = xmin, y = ymax)
                                 },


                                 required_aes = c("well", "label")
)


# rescaled lineplots ------------------------------------------------------

rescale01 <- function(x) (x - min(x)) / diff(range(x))


GeomWellLine <- ggplot2::ggproto("GeomWellLine", ggplot2::GeomPath,
                        required_aes = c("x", "y"),

                        default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,
                                          nrow = NA,
                                          ncol = NA,
                                          w = NA,
                                          h = NA,
                                          pad_in_col = NA,
                                          pad_in_row = NA,
                                          pad_out_col = NA,
                                          pad_out_row = NA,
                                          col_label_nudge = NA),

                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            wells <- make_wells(xs, ys)
                            data <- dplyr::left_join(data, wells, by = c("well"))
                            # the input data is going to have x and y coords
                            data <- dplyr::group_by(data, plate)
                            data <- dplyr::mutate(data,
                                                  x01 = rescale01(x),
                                                  y01 = rescale01(y))
                            data <- dplyr::ungroup(data)
                            data <- dplyr::mutate(data,
                                                  x = xmin + (xmax - xmin) * x01,
                                                  y = ymin - (ymin - ymax) * y01)
                            dplyr::arrange(data, PANEL, group, x)
                            # data[order(data$PANEL, data$group, data$x), ]
                        }
)

#' @export
geom_well_line <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           geom = GeomWellLine,
                           nrow = NA, ncol = NA, w = NA, h = NA,
                           pad_in_col = NA, pad_in_row = NA, pad_out_col = NA,
                           pad_out_row = NA,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            nrow = nrow,
            ncol = ncol,
            w = w,
            h = h,
            pad_in_col = pad_in_col,
            pad_in_row = pad_in_row,
            pad_out_col = pad_out_col,
            pad_out_row = pad_out_row,
            ...
        ),
        layer_class = LayerMtp
    )
}


# bioscreen section -------------------------------------------------------

GeomNotchedBorderBioscreen <- ggplot2::ggproto("GeomNotchedBorderBioscreen", GeomNotchedBorder,
                             setup_data = function(self, data, params) {
                                 notched_border <- make_notched_border_bioscreen(params$w, params$h)
                                 data <- dplyr::group_by(data, plate)
                                 data <- dplyr::filter(data, dplyr::row_number() == 1)
                                 data <- tidyr::crossing(data, notched_border)
                                 dplyr::ungroup(data)
                             }

)

GeomWellRectBioscreen <- ggplot2::ggproto("GeomWellRectBioscreen", GeomWellRect,


                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            wells <- make_wells_bioscreen(xs, ys)
                            dplyr::left_join(data, wells, by = c("well"))
                        }
)

GeomRowLabelBioscreen <- ggplot2::ggproto("GeomRowLabelBioscreen", GeomRowLabel,

                        setup_data = function(data, params) {
                            xs <- make_bandscale(params$w, params$ncol, params$pad_in_col, params$pad_out_col)
                            ys <- make_bandscale(params$h, params$nrow, params$pad_in_row, params$pad_out_row, reverse = T)
                            row_labels <- make_row_labels_bioscreen(xs, ys, params$row_label_nudge)
                            # could do something about grouping by row here
                            # if I have access to that info?
                            data <- dplyr::group_by(data, plate)
                            data <- dplyr::filter(data, dplyr::row_number() == 1)
                            data <- tidyr::crossing(data, row_labels)
                            dplyr::ungroup(data)
                        }
)

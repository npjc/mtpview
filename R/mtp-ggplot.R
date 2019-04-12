#' plot a microtitre plate ggplot style
#'
#' @param tbl table of data to operate on, one row per well.
#' @param well_var name of the variable in tbl that has well identifiers (e.g. 'A01')
#' @param well_fill_var name of variable in tbl to map to well fill
#' @param well_alpha_var name of variable in tbl to map to well transparency (alpha)
#'
#' @examples
#'     tbl <- read_tsv('test-plate.tsv') %>%  nest(runtime, measure)
#'     tbl <- tbl %>% mutate(cond = case_when(
#'                                            is.na(drug_name) ~ 'ref',
#'                                            drug_name == 'Blank' ~ 'blank',
#'                                            TRUE ~ 'trt'))
#'     ggplate(tbl, well, )
#' @export
mtp_ggplot <- function(tbl,
                    well_var = 'well',
                    well_fill_var = NULL,
                    well_alpha_var = NULL,
                    draw_lineplots = FALSE) {
    n_wells <- nrow(tbl)
    stopifnot(n_wells %in% mtp_params_data$n_wells)
    plot_data <- make_mtp_tbl(n_wells = n_wells, wells_as = "rects")
    plot_data$wells <- dplyr::left_join(tibble::as_tibble(plot_data$wells), tbl, by = c('well_label' = well_var))


    p <- ggplot() +
        # plate footprint and well border
        geom_rect(data = plot_data$footprint, aes(xmin = x, ymin = y, xmax = x + width, ymax = y + height), color = "black", fill = "grey95") +
        geom_path(data = plot_data$well_border, aes(x = x, y = y), color = "black") +
        # wells
        geom_rect(data = plot_data$wells, aes_string(xmin = 'x', ymin = 'y', xmax = 'x + width', ymax = 'y + height',
                                                     fill = well_fill_var, alpha = well_alpha_var), color = "black") +
        # labels
        geom_text(data = plot_data$row_labels, aes(x = x, y = y, label = text), vjust = 0.5, size = 4, color = "grey50") +
        geom_text(data = plot_data$col_labels, aes(x = x, y = y, label = text), vjust = 0.25, size = 4, color = "grey50")

    if (draw_lineplots) {
        well_lineplot_data <-  dplyr::select(plot_data$wells, id, well_label, x,y,width,height, data)
        well_lineplot_data <- tidyr::unnest(well_lineplot_data)
        plate_x_range <- range(well_lineplot_data[['runtime']])
        plate_y_range <- range(well_lineplot_data[['measure']])
        well_lineplot_data <- dplyr::group_by(well_lineplot_data, well_label)
        well_lineplot_data <- dplyr::mutate(well_lineplot_data,
                                            scaled_x = scales::rescale(runtime, to = c(x[1], x[1] + width[1] * (max(runtime)/plate_x_range[2]))),
                                            scaled_y = scales::rescale(measure, to = c(y[1] + height[1], y[1] + height[1] * (1 - (max(measure)/plate_y_range[2])))))
        well_lineplot_data <- dplyr::ungroup(well_lineplot_data)

        p <- p + geom_line(data = well_lineplot_data, aes(x = scaled_x, y = scaled_y, group = id))
    }
    p +
        scale_y_reverse() +
        coord_fixed() +
        theme_void()
}


#' The idea here is to have a function that generates the following elements to represent a microplate:
#'   1. footprint: this is a rounded rect that is the outer border of the microwell plate, it's footprint.
#'   2. notched border: this is the border that has notches in it.
#'   3. wells: this is either circles or rounded rect elements (one per well)
#'   4. labels: row and column labels for the microwell plate, text elements.
#'
#'
#' @param n_wells <int> number of wells
#' @param wells_as <chr> "rects" or "circles"
#'
#' @examples
#'   make_microwell_plate_tbl(384, wells_as = "rects")
make_mtp_tbl <- function(n_wells, wells_as) {
    p <- dplyr::filter(mtp_params_data, n_wells == !!n_wells)

    footprint <- mtp_footprint(p$width, p$height)
    well_border <- mtp_notched_border(p$width, p$height)
    wells <- mtp_wells(p$n_wells, p$n_rows, p$n_cols, p$cx_shift, p$cy_shift,
                         p$well_center_to_center, p$well_radius, wells_as)
    row_labels <- mtp_row_labels(p$n_rows, p$cx_shift, p$cy_shift,
                                   p$well_center_to_center)
    col_labels <- mtp_col_labels(p$n_cols, p$cx_shift, p$cy_shift,
                                   p$well_center_to_center)
    list(plate = p,
         footprint = footprint,
         well_border = well_border,
         wells = wells,
         row_labels = row_labels,
         col_labels = col_labels)
}


#' Rounded rectangle of microtitreplate footprint from width and height
#'
#' @param w width in cm
#' @param h height in cm
#'
#' @examples
#'     mtp_footprint(127.76, 85.48)
mtp_footprint <- function(w, h) {
    r <- 3
    tibble::tibble(element = "rect",
                   class = "border",
                   id = "plate-footprint",
                   x = 0,
                   y = 0,
                   width = w,
                   height = h,
                   rx = r,
                   ry = r)
}

#' Path for rectangle with notched border of microtitre plate.
#'
#' @param w width in cm
#' @param h height in cm
#'
#' @examples
#'     mtp_notched_border(127.76, 85.48)
mtp_notched_border <- function(w, h) {
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
        element = "path",
        class = "border",
        id = "plate-notched-border",
        x = unlist(lapply(l, `[`, 1)),
        y = unlist(lapply(l, `[`, 2))
    )
}


#' plot elements for wells as rects or circles based on plate params.
#' One row per well output.
#'
mtp_wells <- function(n_wells, n_rows, n_cols, cx_shift, cy_shift,
                      well_center_to_center, well_radius, wells_as) {
    fun <- switch(wells_as,
                  "circles" = mtp_wells_circles,
                  "rects" = mtp_wells_rects)
    fun(n_wells, n_rows, n_cols, cx_shift, cy_shift, well_center_to_center,
        well_radius)
}


#' @rdname mtp_wells
mtp_wells_circles <- function(n_wells, n_rows, n_cols, cx_shift, cy_shift,
                              well_center_to_center, well_radius) {
    # center x and y for each well
    cxs <- c(0, cumsum(rep.int(well_center_to_center, n_cols - 1))) + cx_shift
    cys <- c(0, cumsum(rep.int(well_center_to_center, n_rows - 1))) + cy_shift
    # fill the grid
    d <- expand.grid(cx = cxs, cy = cys)
    # add radius, class, id
    d <- dplyr::mutate(d, element = "circle", class = "well",
                       r = well_radius,
                       id = paste0("well-", dplyr::row_number(r)))
    d <- dplyr::select(d, element, class, id, cx, cy, r)
    d
}


#' @rdname mtp_wells
mtp_wells_rects <- function(n_wells, n_rows, n_cols, cx_shift, cy_shift,
                            well_center_to_center, well_radius) {
    d <- mtp_wells_circles(n_wells, n_rows, n_cols, cx_shift, cy_shift,
                             well_center_to_center, well_radius)
    # switch to rounded rects if plate has > 96 wells
    # left side:  x = cx - r, width = r*2
    # top side: y = cy - r
    d <- dplyr::transmute(d, element = "rect", class = class, id = id,
                          x = cx - r, y = cy - r, width = r * 2, height = width,
                          rx = r / 10, ry = r / 10)
    d <- dplyr::mutate(d, well_label = mtp_wells_from_dims(n_rows, n_cols))
    d
}

mtp_row_labels <- function(n_rows, cx_shift, cy_shift, well_center_to_center) {
    d <- tibble::tibble(
        element = "text",
        class = "label row-label",
        x = cx_shift / 2,
        y = c(0, cumsum(rep.int(well_center_to_center, n_rows - 1))) + cy_shift,
        text = LETTERS[seq_len(n_rows)]
    )

    d
}

mtp_col_labels <- function(n_cols, cx_shift, cy_shift, well_center_to_center) {
    d <- tibble::tibble(
        element = "text",
        class = "label col-label",
        x = c(0, cumsum(rep.int(well_center_to_center, n_cols - 1))) + cx_shift,
        y = cy_shift / 2,
        text = as.character(seq_len(n_cols))
    )
    d
}

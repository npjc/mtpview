# well coords -------------------------------------------------------------

well_coords <- function(nrow, ncol, ipad, opad, labels = NULL) {
    xs <- bandscale_01(ncol, ipad, opad)
    ys <- bandscale_01(nrow, ipad, opad, reverse = TRUE)
    n <- ncol * nrow
    if (is.null(labels))
        labels <- .well_labels[[as.character(n)]]
    list(
        well = labels,
        x = rep.int(xs[['values']], nrow),
        y = rep(ys[['values']], each = ncol),
        width = rep.int(xs[['band']], n),
        height = rep.int(ys[['band']], n)
    )
}

well_limits <- function(wells) {
    width = wells[['width']]
    height = wells[['height']]
    x0 = wells[['x']] - width / 2
    y0 = wells[['y']] - height / 2

    list(
        well = wells[['well']],
        x0 = x0, y0 = y0,
        x1 = x0 + width,
        y1 = y0 + height
    )
}

well_scale_maker <- function(well, x0, x1, y0, y1) {
    to_x <- c(x0, x1)
    to_y <- c(y0, y1)
    function(data, range_x, range_y) {
        data[['x']] <- scales::rescale(data[['x']], to = to_x, from = range_x)
        data[['y']] <- scales::rescale(data[['y']], to = to_y, from = range_y)
        data
    }
}

well_scales <- function(l) {
    n <- length(l$well)
    out <- vector('list', n)
    for (i in seq_len(n)) {
        fxn <- well_scale_maker(l$well[i], l$x0[i], l$x1[i], l$y0[i], l$y1[i])
        out[[i]] <- fxn
    }
    names(out) <- l$well
    out
}


scale_to_wells <- function(data, limits, range_x = range(data$x, na.rm = TRUE),
                           range_y = range(data$y, na.rm = TRUE)) {
    scales <- well_scales(limits)
    l <- dplyr::group_split(data, well)
    for (i in seq_along(l)) {
        curr_well <- unique(l[[i]][['well']])
        curr_scale <- scales[[curr_well]]
        l[[i]] <- curr_scale(l[[i]], range_x, range_y)
    }
    dplyr::bind_rows(l)
}

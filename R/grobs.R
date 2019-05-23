# grobs -------------------------------------------------------------------

footprint_roundrect_grob <- function(l = NULL, vp = NULL, name = 'footprint') {
    if (is.null(l))
        l <- list(x = 0.5, y = 0.5, width = 1, height = 1, r = 1 / 42)

    grid::roundrectGrob(
        x = l[['x']] %||% 0.5,
        y = l[['y']] %||% 0.5,
        width = l[['width']] %||% 1,
        height = l[['height']] %||% 1,
        r = unit_npc(l[['r']] %||% 1 / 42),
        vp = vp,
        name = name,
        gp = get_gpars(l)
    )
}

notched_border_path_grob <- function(l = NULL, vp = NULL, name = 'notched_border') {
    if (is.null(l))
        l <- list(x = c(0.0547, 0.984, 0.984, 0.0547, 0.0156, 0.0156),
                  y = c(0.0234, 0.0234, 0.977, 0.977, 0.918, 0.0818))

    grid::pathGrob(
        x = l[['x']] %||% c(0.0547, 0.984, 0.984, 0.0547, 0.0156, 0.0156),
        y = l[['y']] %||% c(0.0234, 0.0234, 0.977, 0.977, 0.918, 0.0818),
        vp = vp,
        name = name
    )
}

col_labels_grob <- function(l, vp = NULL, name = 'col_labels') {
    x <- unique(l[['x']])
    n <- length(x)
    y_well_boundary <- min(l[['y']] - l[['height']] / 2) * (2/3)
    y <-  min(l[['y']] - l[['height']] / 2) * (2/3)
    y <- rep.int(1 - y, n)
    label <- col_labels(seq_len(n))
    grid::textGrob(label = label, x = x, y = y,
                   gp = get_gpars(l), vp = vp, name = name)
}

row_labels_grob <- function(l, vp = NULL, name = 'row_labels') {
    y <- unique(l[['y']])
    n <- length(y)
    x <- min(l[['x']] - l[['width']] / 2) * (2/3)
    x <- rep.int(x, n)
    label <- row_labels(seq_len(n))
    grid::textGrob(label = label, x = x, y = y,
                   gp = get_gpars(l), vp = vp, name = name)
}

wells_rect_grob <- function(l, vp = NULL, name = 'wells') {
    grid::rectGrob(x = l[['x']], y = l[['y']], width = l[['width']], height = l[['height']],
                   gp = get_gpars(l), vp = vp, name = name)
}

wells_polyline_grob <- function(l, vp = NULL, name = NULL) {
    grid::polylineGrob(x = l[['x']], y = l[['y']],
                       id = as.numeric(as.factor(l[['well']])),
                       gp = get_gpars(l), vp = vp, name = name)
}


# grob trees --------------------------------------------------------------

fixed_ratio_vp <- function(nrow, ncol) {
    if (ncol >= nrow) {
        w <- ncol / nrow
        h <- 1
    }
    if (ncol < nrow) {
        w <- 1
        h <- nrow / ncol
    }
    grid::viewport(width = unit_snpc(w), height = unit_snpc(h))
}

mtp_generic <- function(nrow, ncol, ipad, opad,
                        data = NULL, vp = NULL, gp = NULL, draw = TRUE) {
    coords <- well_coords(nrow, ncol, ipad, opad)
    limits <- well_limits(coords)

    cvp <- fixed_ratio_vp(nrow, ncol)

    lines <- NULL
    if (!is.null(data)) {
        l <- scale_to_wells(data, limits)
        lines <- wells_polyline_grob(l, vp = cvp)
    }

    gl <- grid::gList(
        footprint = footprint_roundrect_grob(vp = cvp),
        notched_border = notched_border_path_grob(vp = cvp),
        col_labels = col_labels_grob(coords, vp = cvp),
        row_labels = row_labels_grob(coords, vp = cvp),
        wells = wells_rect_grob(coords, vp = cvp),
        lines = lines
    )


    gl <- grid::gTree(children = gl, vp = vp, gp = gp)

    if (draw)
        grid::grid.draw(gl)
    gl
}

mtp_12 <- function(data = NULL, vp = NULL, gp = NULL, draw = TRUE, ipad = 0.1, opad = 0.5) {
    mtp_generic(nrow = 3, ncol = 4, ipad = ipad, opad = opad,
                data = data, vp = vp, gp = gp, draw = draw)
}

mtp_24 <- function(data = NULL, vp = NULL, gp = NULL, draw = TRUE, ipad = 0.1, opad = 0.5) {
    mtp_generic(nrow = 4, ncol = 6, ipad = ipad, opad = opad,
                data = data, vp = vp, gp = gp, draw = draw)
}

#' @export
mtp_48 <- function(data = NULL, vp = NULL, gp = NULL, draw = TRUE, ipad = 0.1, opad = 0.5) {
    mtp_generic(nrow = 6, ncol = 8, ipad = ipad, opad = opad,
                data = data, vp = vp, gp = gp, draw = draw)
}
#' @export
mtp_96 <- function(data = NULL, vp = NULL, gp = NULL, draw = TRUE, ipad = 0.1, opad = 0.7) {
    mtp_generic(nrow = 8, ncol = 12, ipad = ipad, opad = opad,
                data = data, vp = vp, gp = gp, draw = draw)
}
#' @export
mtp_384 <- function(data = NULL, vp = NULL, gp = NULL, draw = TRUE, ipad = 0.1, opad = 1.2) {
    mtp_generic(nrow = 16, ncol = 24, ipad = ipad, opad = opad,
                data = data, vp = vp, gp = gp, draw = draw)
}

# mtp_1536 <- function(data = NULL, vp = NULL, draw = TRUE, ipad = 0.1, opad = 0.5) {
#     mtp_generic(nrow = 8, ncol = 12, ipad = ipad, opad = opad)
# }
#
# mtp_6144 <- function(data = NULL, vp = NULL, draw = TRUE, ipad = 0.1, opad = 0.5) {
#     mtp_generic(nrow = 8, ncol = 12, ipad = ipad, opad = opad)
# }

#' @export
mtp_auto <- function(data, vp = NULL, gp = NULL, draw = TRUE) {
    f <- mtp_fxn_from_wells(data$well)
    f(data = data, vp = vp, gp = gp, draw = draw)
}

mtp_fxn_from_wells <- function(x) {
    x <- unique(x)
    if (all(x %in% .well_labels[['12']]))
        return(mtp_12)
    if (all(x %in% .well_labels[['24']]))
        return(mtp_24)
    if (all(x %in% .well_labels[['48']]))
        return(mtp_48)
    if (all(x %in% .well_labels[['96']]))
        return(mtp_96)
    if (all(x %in% .well_labels[['384']]))
        return(mtp_384)
    # if (all(x %in% .well_labels[['1536']]))
    #     return(mtp_1536)
    # if (all(x %in% .well_labels[['6144']]))
    #     return(spec_6144well())
    stop('cannot match all wells to one type.', call. = F)
}


# titles ------------------------------------------------------------------

add_title <- function(g, title, vp = NULL, name = NULL, draw = TRUE) {
    title_grob <- titleGrob(title)
    ge <- grid::editGrob(g, vp = grid::viewport(x = 0.5, y = 0.45, width = 0.95, height = 0.9))
    gt <- grid::gTree(
        children = grid::gList(title_grob, ge),
        vp = vp,
        name = name
    )
    if (draw)
        grid::grid.draw(gt)
    gt
}

titleGrob <- function(title) {
    grid::textGrob(label = title, vp = grid::viewport(x = 0.5, y = 0.95, height = 0.9, width = 1))
}



# layout ------------------------------------------------------------------



# multi-plate -------------------------------------------------------------

mtp_layout <- function(gList, nrow = NULL, ncol = NULL) {
    n <- length(gList)
    dims <- pick_dims(n, nrow, ncol)

    row <- rep(seq_len(dims[['nrow']]), each = dims[['ncol']])[seq_len(n)]
    col <- rep(seq_len(dims[['ncol']]), dims[['nrow']])[seq_len(n)]

    for (i in seq_len(n)) {
        vp <- grid::viewport(layout.pos.row = row[i], layout.pos.col = col[i])
        grob <- gList[[i]]
        if (!is.null(grob[['vp']])) {
            message('viewport not null!')
            vp <- grid::vpStack(grid::vpList(vp), grob[['vp']])
        }
        gList[[i]] <- grid::editGrob(grob, vp = vp)
    }

    layout <- grid::grid.layout(nrow = dims[['nrow']], ncol = dims[['ncol']])

    gt <- grid::gTree(
        children = gList,
        vp = grid::viewport(layout = layout,  name = 'mtpLayout')
    )
    gt
}

pick_dims <- function(n, nrow = NULL, ncol = NULL) {
    if (is.null(ncol) && is.null(nrow)) {
        if (n < 4) {
            nrow <- n
            ncol <- 1
        }
        else if (n < 7) {
            nrow <- (n + 1) %/% 2
            ncol <- 2
        }
        else if (n < 13) {
            nrow <- (n + 2) %/% 3
            ncol <- 3
        }
        else {
            nrow <- ceiling(sqrt(n))
            ncol <- ceiling(n / nrow)
        }
    } else if (is.null(ncol)) {
        ncol <- ceiling(n / nrow)
    } else if (is.null(nrow)) {
        nrow <- ceiling(n / ncol)
    }
    stopifnot(nrow * ncol >= n)
    list(nrow = nrow, ncol = ncol)
}


# grid units --------------------------------------------------------------

unit_npc <- function(x) {
    structure(x, valid.unit = 0L, unit = 'npc', class = 'unit')
}

unit_cm <- function(x) {
    structure(x, valid.unit = 1L, unit = 'cm', class = 'unit')
}

unit_inches <- function(x) {
    structure(x, valid.unit = 2L, unit = 'inches', class = 'unit')
}

unit_lines <- function(x) {
    structure(x, valid.unit = 3L, unit = 'lines', class = 'unit')
}

unit_native <- function(x) {
    structure(x, valid.unit = 4L, unit = 'native', class = 'unit')
}

unit_snpc <- function(x) {
    structure(x, valid.unit = 6L, unit = 'snpc', class = 'unit')
}

unit_mm <- function(x) {
    structure(x, valid.unit = 7L, unit = 'mm', class = 'unit')
}

unit_points <- function(x) {
    structure(x, valid.unit = 8L, unit = 'points', class = 'unit')
}


# grid gpar ---------------------------------------------------------------

gpar <- function(...) {
    structure(
        list(
            ...
        ),
        class = 'gpar'
    )
}

# get gpars from list -----------------------------------------------------

get_gpars <- function(l) {
    alpha <- l[['alpha']] %||% 1

    grid::gpar(
        fill = scales::alpha(l[['fill']] %||% "white", alpha),
        col = scales::alpha(l[['col']] %||% "black", alpha),
        lty = l[['lty']] %||% "solid",
        # lwd = l[['lwd']] %||% 1,
        cex = l[['cex']] %||% 1,
        fontsize = l[['fontsize']] %||% 7,
        lineheight = l[['lineheight']] %||% 1.2,
        font = l[['font']] %||% 1L,
        fontfamily = l[['fontfamily']] %||% "",
        # alpha = l[['alpha']] %||% 1,
        lineend = l[['lineend']] %||% "round",
        linejoin = l[['linejoin']] %||% "round",
        linemitre = l[['linemitre']] %||% 10,
        lex = l[['lex']] %||% 1
    )
}

# grid viewport -----------------------------------------------------------

viewport_native <- function(x, y, width, height, name, gp = NULL,
                            xscale = c(0, 1), yscale = c(0, 1), angle = 0) {
    if (is.null(gp))
        gp <- gpar()
    structure(
        list(
            x = unit_native(x),
            y = unit_native(y),
            width = unit_native(width),
            height = unit_native(height),
            justification = "centre",
            gp = gp,
            clip = FALSE,
            xscale = xscale,
            yscale = yscale,
            angle = angle,
            layout = NULL,
            layout.pos.row = NULL,
            layout.pos.col = NULL,
            valid.just = c(0.5, 0.5),
            valid.pos.row = NULL,
            valid.pos.col = NULL,
            name = name
        ),
        class = "viewport"
    )

}

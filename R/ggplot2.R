mtp_generic2 <- function(nrow, ncol, ipad, opad,
                        data = NULL, vp = NULL, gp = NULL, draw = TRUE) {
    coords <- well_coords(nrow, ncol, ipad, opad)
    limits <- well_limits(coords)
    well_data <- merge(coords, data, by = 'well')


    gl <- grid::gList(
        footprint = footprint_roundrect_grob(),
        notched_border = notched_border_path_grob(),
        col_labels = col_labels_grob(coords),
        row_labels = row_labels_grob(coords),
        wells = wells_rect_grob(well_data)
    )


    gl <- grid::gTree(children = gl, vp = vp, gp = gp)

    if (draw)
        grid::grid.draw(gl)
    gl
}

# plate and wells ---------------------------------------------------------

GeomPlate <- ggplot2::ggproto("GeomPlate", ggplot2::Geom,

                              required_aes = c("plate"),

                              default_aes = ggplot2::aes(
                                  colour = "grey40", fill = "grey96", alpha = 1,
                                  linetype = 1, size = 0.2
                              ),
                              setup_data = function(data, params) {
                                  dplyr::distinct(data, plate, .keep_all = TRUE)
                              },
                              mtp = function(ptype) {
                                  do.call(ptype, list(draw = FALSE))
                              },
                              draw_panel = function(self, data, panel_params, coord) {
                                  coords <- coord$transform(data, panel_params)

                                  test <- self$mtp()
                                  str(test, 1)
                              }
)

#' @export
geom_plate <- function(mapping = NULL, data = NULL, stat = "identity",
                       geom = GeomPlate,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = geom, mapping = mapping,  data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,  ...))
}

GeomWellRect <- ggplot2::ggproto("GeomWellRect", ggplot2::GeomRect,
                                 extra_params = c("na.rm"),

                                 required_aes = c("plate", "well"),

                                 default_aes = ggplot2::aes(
                                     colour = "grey40", fill = "grey96", alpha = 1,
                                     linetype = 1, size = 0.2, mtp_type = "mtp_auto"
                                 ),
                                 setup_data = function(data, params) {
                                     dplyr::distinct(data, plate, well, .keep_all = TRUE)
                                 },
                                 draw_panel = function(self, data, panel_params, coord) {
                                     coords <- coord$transform(data, panel_params)

                                     well_coords <- well_coords(nrow, ncol, ipad, opad)
                                     limits <- well_limits(well_coords)
                                     wells_rect_grob(well_coords)

                                     g <- wellGrob_from_l(spec$wells)
                                     grid::editGrob(g, gp = gp)

                                 }
)

#' @export
geom_well_rect <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           geom = GeomWellRect,
                           n_wells = 96,
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
            n_wells = n_wells,
            ...
        ))
}

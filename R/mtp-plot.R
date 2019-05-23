#' @export
mtp_plot <- function(data, plate_group = vars(plate)) {
    l <- dplyr::group_split(data, !!!plate_group)
    gl <- lapply(l, mtp_auto, draw = FALSE)
    titles <- auto_title(data, plate_group)
    gl <- purrr::map2(gl, titles, ~add_title(.x, .y, draw = FALSE))
    gl <- do.call(grid::gList, gl)
    grid::grid.draw(mtp_layout(gl))
}


auto_title <- function(data, group_vars) {
    d <- dplyr::distinct(data, !!!group_vars)
    d <- tidyr::unite(d, title, !!!group_vars, sep = ';')
    d[['title']]
}

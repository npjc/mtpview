# 0 to 1 bandscale --------------------------------------------------------

bandscale_01 <- function(n, i, o, reverse = FALSE) {
    step <- 1 / ((2 * o) - i + n)
    band_i = step * i
    band_o = step * o
    band = step - band_i

    range = (2 * band_o) + (n * band) +  ((n - 1) * band_i)
    values = seq.int(0, n - 1) * step + band_o + (band / 2)

    if (reverse)
        values = rev(values)

    list(values = values / range, n = n, band = band,
         band_i = band_i, band_o = band_o)
}

#' show_bandscale(bandscale_01(3, 0.05, 1))
#' show_bandscale(bandscale_01(12, 0.1, 0.5))
#' show_bandscale(bandscale_01(24, 0.1, 0.5))
#' show_bandscale(bandscale_01(96, 0.1, 0.5))
show_bandscale <- function(l) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport())
    grid::grid.rect(x = l$band_o / 2, width = l$band_o,
              gp = grid::gpar(fill = 'red', alpha = 0.5))
    grid::grid.rect(x = l$values, width = l$band,
              gp = grid::gpar(fill = 'green', alpha = 0.5))
    grid::grid.rect(x = 1 - (l$band_o / 2), width = l$band_o,
              gp = grid::gpar(fill = 'red', alpha = 0.5))
}



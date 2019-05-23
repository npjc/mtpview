# validate grid gpar ------------------------------------------------------

validate_vars <- function(x) {
    valid <- c('col', 'fill', 'alpha', 'lty', 'lwd', 'lex',
               'lineend', 'linejoin', 'linemitre')
    not_valid <- setdiff(names(x), valid)
    if (length(not_valid) != 0)
        stop(paste0('invalid vars: ', paste0(not_valid, collapse = ',')), call. = F)
}


# rescale data to gpars ---------------------------------------------------

as_lwd <- function(x, max = 5) {
    if (is.factor(x) | is.character(x))
        x <- as.numeric(as.factor(x))
    scales::rescale(x, to = c(1, max))
}

as_lty <- function(x) {
    xl <- as.numeric(as.factor(x))
    ltyu <- rep_len(seq_len(6), length(unique(xl)))
    names(ltyu) <- unique(xl)
    res <- ltyu[xl]
    res[is.na(res)] <- 0
    unname(res)
}

as_alpha <- function(x) {
    if (is.factor(x) | is.character(x))
        x <- as.numeric(as.factor(x))
    scales::rescale(x, to = c(0, 1))
}

as_color <- function(x, n = 99) {
    if (is.factor(x) | is.character(x)) {
        f <- scales::col_factor(
            palette = "Set1",
            domain = NULL,
            levels = NULL,
            ordered = FALSE,
            na.color = "#ffffff"
        )
    }
    if (is.numeric(x)) {
        palette <- c("#ffffff", viridisLite::viridis(n))
        f <- scales::col_numeric(palette, domain = NULL, na.color = "#808080")
    }

    f(x)
}

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

#' @export
vars <- function(...) {
    rlang::quos(...)
}

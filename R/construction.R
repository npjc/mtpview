#' @export
mtp_ggplot <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
    if (!missing(mapping) && !inherits(mapping, "uneval")) {
        stop("Mapping should be created with `aes() or `aes_()`.", call. = FALSE)
    }

    p <- structure(list(
        data = data,
        layers = list(),
        scales = ggplot2:::scales_list(),
        mapping = mapping,
        theme = ggplot2::theme_void(),
        coordinates = coord_fixed(expand = FALSE),
        facet = facet_null(),
        plot_env = environment
    ), class = c("gg", "ggplot"))

    p$labels <- ggplot2:::make_labels(mapping)

    ggplot2:::set_last_plot(p)
    p
}

LayerMtp <- ggplot2::ggproto("LayerMtp", ggplot2:::Layer)

validate_mtp_spec <- function(mtp_spec, object) {
    geom_name <- ggplot2:::snakeize(class(object$geom)[1])
    if (is.null(mtp_spec))
        stop('must specficy mtp spec for ', geom_name, '()',call. = F)
}

#' @export
ggplot_add.LayerMtp <- function(object, plot, object_name) {
    validate_mtp_spec(plot$mtp_spec, object)
    # update NA defaults from params in spec!
    new_defaults <- update_if_na(object$aes_params, plot$mtp_spec)
    # object$geom$default_aes <- new_defaults
    object$aes_params <- new_defaults

    plot$layers <- append(plot$layers, object)

    # Add any new labels
    mapping <- ggplot2:::make_labels(object$mapping)
    default <- ggplot2:::make_labels(object$stat$default_aes)
    new_labels <- ggplot2:::defaults(mapping, default)
    plot$labels <- ggplot2:::defaults(plot$labels, new_labels)
    plot
}

# geom_default_params <- list(fill = "grey40", w = NA, h = NA)
# spec_params <- list(fill = "grey40", w = 127.76, h = 85.48)
# update_if_na(geom_default_params, spec_params)

# # - [x] param x = NA is default but spec sets it to x = 1
# update_if_na(list(x = NA), list(x = 1))
# # - [x] param x = NA is set to x = 1 but y = 1 value NOT overwritten
# update_if_na(list(x = NA, y = 1), list(x = 1, y = 2))
# # - [x] spec has additional values not present as part of defaults,
# #       these don't get included
# update_if_na(list(), list(x = 1))
# update_if_na(list(), list())
# # - [ ] ignores NAs that aren't in the spec
# update_if_na(list(x = NA), list())
# update_if_na(list(x = NA), list(y = NA))
update_if_na <- function(default, updated) {
    is_na <- is.na(default)
    in_updated <- names(default) %in% names(updated)
    to_update <- names(default[is_na & in_updated])
    to_keep <- setdiff(names(default), to_update)

    c(default[to_keep], updated[to_update])
}

# I need to import the generic version in order to be able to export S3 methods
# against it.

#' @importFrom ggplot2 ggplot_add

#' @export
#' @keywords internal
ggplot_add.mtp_spec <- function(object, plot, object_name) {
    if (!is.null(plot$mtp_spec)) {
        message(
            "Microtitre Plate Specificiation already present,",
            "Adding another replaces the existing one."
        )
    }

    plot$mtp_spec <- object
    plot
}


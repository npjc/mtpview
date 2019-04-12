
# rescaling factor out ----------------------------------------------------

#' rescale x,y to the coordinate system of the destination well
#' for plotting. to_x, to_y, width, height, are the attributes of
#' the destination well to scale against
#' by default this scales from the ranges of x and y to the
#' range of the rectangular well. Need to come up with a way to
#' offer well-rescaling, plate-rescaling, experiment-rescaling.
rescale_to_wells <- function(x, y, to_x, to_y, width, height) {
    x <- scales::rescale(x, to = c(to_x, to_x + width))
    y <- scales::rescale(y, to = c(to_y + height, to_y))
    tibble::tibble(x = x, y = y)
}

# i.e. i want to end up with...
# mtp_rescale(range_by = c("well", "plate", "run", "experiment"))

# one plate combined with other graph -------------------------------------


# p1 <- tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure))) %>%
#     ggplate('well', well_fill_var = 'cond') +
#     labs(title = 'Plate 1 / 144 in FDA2 screen')
#
# p2 <- tbl %>%
#     unnest(data) %>%
#     ggplot(aes(x = runtime / 3600, y = measure, color = cond, group = well)) +
#     geom_line(aes(alpha = fct_rev(cond))) +
#     theme_bw() +
#     theme(panel.grid.minor = element_blank(),
#           panel.grid.major = element_line(size = 0.1))
#
# gridExtra::grid.arrange(p1, p2, ncol = 1)


# multiple plates ---------------------------------------------------------


ggplates <- function(tbl, plate_var, well_var, well_fill_var = NULL, well_alpha_var = NULL, draw_lineplots = F) {
    plates <- split(tbl, tbl[[plate_var]])
    plot_fun <- function(...) {
        ggplate(...) + guides(fill = F, alpha = F)
    }
    plots <- map(plates, plot_fun, well_var = well_var, well_fill_var = well_fill_var, well_alpha_var = well_alpha_var, draw_lineplots = draw_lineplots)
    do.call(gridExtra::grid.arrange, plots)
}

# tbl <- read_tsv('test-plates.tsv') %>%  nest(runtime, measure)
# tbl <- tbl %>% mutate(cond = case_when(is.na(drug_name) ~ 'ref',
#                                        drug_name == 'Carmustine' ~ 'in focus',
#                                        drug_name == 'Blank' ~ 'blank',
#                                        TRUE ~ 'trt'))
# tbl <- tbl %>%
#     mutate(cond = factor(cond, levels = c("in focus", "ref", "blank", "trt")))
# tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure))) %>%
#     ggplates(plate_var = 'plate', well_var = 'well', well_fill_var = 'cond', well_alpha_var = '-max_abs')
#
# tbl %>%
#     filter(plate == 'S1L1') %>%
#     ggplate(well_var = 'well', well_fill_var = 'cond', draw_lineplots = T) +
#     guides(fill = F)
#
#
#
# tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure))) %>%
#     ggplates(plate_var = 'plate', well_var = 'well', well_fill_var = 'cond', draw_lineplots = T)
#
#
# summary_data <- tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure))) %>%
#     mutate(alpha = if_else(cond %in% c('ref', 'in focus'), 1, 0.6)) %>%
#     group_by(plate, cond) %>%
#     summarise(max_mean = mean(max_abs),
#               max_sd = sd(max_abs))
#
#
# tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure)),
#            alpha = if_else(cond %in% c('ref', 'in focus'), 1, 0.4)) %>%
#     filter(alpha == 1) %>%
#     ggplot() +
#     ggbeeswarm::geom_beeswarm(aes(x = plate,
#                                   y = max_abs,
#                                   color = cond,
#                                   alpha = alpha),
#                               size = 2.5, cex = 1.1)  +
#     geom_errorbar(aes(x = plate,
#                       ymin = max_mean + max_sd,
#                       ymax = max_mean - max_sd,
#                       color = cond),
#                   data = summary_data %>% filter(cond == 'ref')) +
#     scale_alpha_continuous(range = c(0.4, 1)) +
#     scale_color_manual(values = c('#e77c72', '#86ab34')) +
#     guides(alpha = F) +
#     cowplot::theme_cowplot()


# tbl %>%
#     mutate(max_abs = map_dbl(data, ~max(.x$measure)),
#            alpha = if_else(cond %in% c('ref', 'in focus'), 1, 0.4)) %>%
#     # filter(alpha == 1) %>%
#     unnest(data) %>%
#     ggplot() +
#     geom_line(aes(x = runtime / 3600,
#                   y = measure,
#                   group = well,
#                   color = cond,
#                   alpha = alpha)) +
#     facet_wrap(~plate) +
#     cowplot::theme_cowplot()

#' i should separate the rescale and put it upstream into a completely different
#' function that always outputs the data rescaled to a unit cell (0,1).
# tbl %>%
#     mp_rescale(range_by = c("well", "plate", "run", "experiment")) %>%
#     mp_plot(well_var = 'well')

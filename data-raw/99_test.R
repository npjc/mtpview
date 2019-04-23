library(tidyverse)
library(mtpview)
source("R/graphical-elements.R")
source("R/mtp-spec.R")
source("R/construction.R")
source("R/geoms.R")

# errors without spec as expected.
mtp_example1 %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate)) +
    geom_notched_border() +
    geom_footprint()

# - [ ] validate that 1 rect drawn per well
# - [x] validate that 1 footprint and 1 notched border drawn per plate
# - [ ] validate that nrow and ncol labels are drawn per plate

# i have to be able to handle different defaults...
# okay so I could have the defaults be NA, then when I add
# a layer to the plot I replace all NA values with the
# default from plot$mtp_spec$param_a type of thing. If it
# is not NA then I don't replace. This achieves the desired
# behaviour of 1. not having to expliclty specify defaults,
# 2. default values, that may be different for different plate
# sizes, are inherited from the spec. 3. you can overide these
# by providing things explicity in the geom call.



crossing(plate = 1:4, mtp_example1) %>%
    mtp_ggplot(aes(well = well)) +
    mtp_spec_96well() +
    geom_footprint(aes(color = as.factor(plate)), alpha = 0.3)

mtp_example1 %>%
    mutate(plate = 1) %>%
    mtp_ggplot(aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect(aes(fill = drug, alpha = conc))

mtp_example1 %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect(aes(fill = drug, alpha = conc)) +
    coord_fixed(expand = F) +
    guides(fill = F, alpha = F ) +
    theme_void()

mtp_example2 %>%
    unnest() %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect(aes(fill = cond)) +
    geom_well_line(aes(x = runtime, y = measure)) +
    coord_fixed(expand = F) +
    guides(fill = F, alpha = F ) +
    theme_void()

mtp_example3 %>%
    unnest() %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect(aes(fill = drug, alpha = dose_um)) +
    geom_well_line(aes(x = runtime, y = measure)) +
    annotate("text", x = 110, y = 5, label = "0", size = 2, color = "grey70") +
    annotate("text", x = 118, y = 5, label = "24h", size = 2, color = "grey70") +
    annotate("text", x = 120, y = 6.5, label = "0", size = 2, hjust = "left", color = "grey70") +
    annotate("text", x = 120, y = 15, label = "1 OD", size = 2, hjust = "left", color = "grey70") +
    coord_fixed(expand = F) +
    guides(fill = F, alpha = F ) +
    theme_void()



data <- mtp_example1 %>%
    crossing(tibble(plate = 1:4, strain = rep(c("wt", "del"), 2)))


ggplot(data, aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label(size = 2) +
    geom_col_label(size = 2, col_label_nudge = -1) +
    geom_well_rect(aes(fill = drug, alpha = conc)) +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

ggplot(data, aes(plate = plate, well = well)) +
    mtp_spec_384well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label(size = 2) +
    geom_col_label(size = 2) +
    geom_well_rect(aes(fill = drug, alpha = conc)) +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

mtp_example1 %>%
    mutate(plate = 1) %>%
    sample_n(30) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_96well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label(size = 2) +
    geom_col_label(size = 2) +
    geom_well_rect(aes(fill = drug, alpha = conc)) +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

plot

wells_tbl(6, 8) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_48well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

wells_tbl(4, 6) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_24well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

wells_tbl(3, 4) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_12well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

wells_tbl(2, 3) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_6well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label() +
    geom_col_label() +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

wells_tbl(16, 24) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_384well() +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label(size = 2) +
    geom_col_label(size = 2) +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

wells_tbl(32, 48) %>%
    mutate(plate = 1) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_1536well(pad_in_col = 0.2, pad_in_row = 0.2) +
    geom_footprint() +
    geom_notched_border() +
    geom_row_label(size = 2) +
    geom_col_label(size = 2) +
    geom_well_rect() +
    coord_fixed(expand = T) +
    theme_void() +
    facet_wrap(~plate)

layer_data(plot, 4)


# bioscreen ---------------------------------------------------------------

crossing(plate = 1, well = as.character(1:100)) %>%
    ggplot(aes(plate = plate, well = well)) +
    mtp_spec_bioscreen() +
    geom_notched_border(geom = GeomNotchedBorderBioscreen) +
    geom_col_label() +
    geom_row_label(geom = GeomRowLabelBioscreen) +
    geom_well_rect(geom = GeomWellRectBioscreen) +
    coord_fixed(expand = T) +
    theme_void()


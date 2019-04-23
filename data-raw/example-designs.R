library(tidyverse)


# example 1 ---------------------------------------------------------------

# 8 point dose-response of 11 compounds and a blank (solvent only or dmso...)

drug <- c(NA_character_, 'doxorubicin', 'daunorubicin', 'epirubicin', 'aclarubicin',
          'zorubicin', 'idarubicin', 'mitoxantrone', 'pirarubicin',
          'valrubicin', 'amrubicin', 'pixantrone')
conc <- c(0,2,4,8,16,32,64,128)
wells <- crossing(row = LETTERS[1:8], col = 1:12L) %>%
    mutate(well = sprintf("%s%02d", row, col)) %>%
    pull(well)

mtp_example1 <- crossing(drug = drug, conc = conc) %>%
    arrange(desc(conc), drug) %>%
    # if there is no drug the concentration is 0, not crossing.
    mutate(conc = if_else(is.na(drug), 0, conc)) %>%
    mutate(well = wells) %>%
    select(well, drug, conc)

mtp_example1 %>%
    filter(is.na(drug))

usethis::use_data(mtp_example1, overwrite = TRUE)


# example 2 ---------------------------------------------------------------

mtp_example2 <- file.path('data-raw', 'example-data2.tsv') %>%
    read_tsv(col_types = 'ccddd') %>%
    nest(runtime, measure) %>%
    mutate(cond = case_when(
        is.na(drug_name) ~ 'ref',
        drug_name == 'Carmustine' ~ 'in focus',
        drug_name == 'Blank' ~ 'blank',
        TRUE ~ 'trt')) %>%
    mutate(cond = factor(cond, levels = c("in focus", "ref", "blank", "trt")))

usethis::use_data(mtp_example2, overwrite = TRUE)


# example 3 ---------------------------------------------------------------

mtp_example3 <- file.path('data-raw', 'example-data3.tsv') %>%
    read_tsv(col_types = 'cccddd') %>%
    nest(runtime, measure)

usethis::use_data(mtp_example3, overwrite = TRUE)



# example 4 ---------------------------------------------------------------

mtp_example4 <- read_rds(file.path('data-raw', 'example-data4.rds'))
usethis::use_data(mtp_example4, overwrite = TRUE)

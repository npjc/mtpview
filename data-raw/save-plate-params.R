library(readr)
mtp_params_data <- read_tsv('data-raw/plate-params.tsv', col_types = 'iiidddddd')
usethis::use_data(mtp_params_data, internal = TRUE, overwrite = TRUE)

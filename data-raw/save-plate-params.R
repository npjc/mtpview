library(readr)
mtp_params_data <- read_tsv('data-raw/plate-params.tsv', col_types = 'iiidddddd')
# no longer use these data for computing graphical elements
# usethis::use_data(mtp_params_data, internal = TRUE, overwrite = TRUE)

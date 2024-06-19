library(targets)
library(tarchetypes)
source("config.R")
tar_source()

tar_option_set(
  packages = c("data.table", "ncdf4", "raster", "terra", "sf", "exactextractr", "lubridate"),
  format = "rds"
)


list(
  tar_target(
    dat_spei,
    load_spei(
      indir
    )
  )
  ,
  tar_target(
    dat_cru,
    load_cru(
      indir
    )
  )
  ,
  tar_target(
    cru_rescale,
    do_rescale(
      dat_cru
    )
  )
  # ,
  # tar_target(
  #   mrg,
  #   do_mrg_cru_spei(
  #     dat_spei,
  #     dat_cru
  #   )
  # )
)
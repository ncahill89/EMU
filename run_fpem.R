## I need a work around on my macbook to run jags.parallel
## WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

library(fpemlocal)
library(tidyverse)

# data ----------------------------------------------------------------
## service statistics data for permission countries
emu_data <- read_csv("data/emu_data.csv")
## division numeric codes for permission countries
country_codes <- emu_data$division_numeric_code %>% unique
## survey database for permission countries (contained within fpemlocal)
cu_dat <- contraceptive_use_track20 %>%
  filter(division_numeric_code %in% country_codes) %>%
  filter(is_in_union == "Y")

# run model ---------------------------------------------------------------
fit_c <- fit_fp_c(is_in_union = "Y",
                  surveydata_filepath = "data/cu_dat.csv",
                  service_stats_filepath = "data/emu_data.csv",
                  division_numeric_code = 508,
                  first_year = 1989,
                  last_year = 2030)

res_c <- calc_fp_c(fit = fit_c,
                   population_data = population_counts)

plot_fp_c(fit_c,
          res_c,
          indicators = c("contraceptive_use_modern"))




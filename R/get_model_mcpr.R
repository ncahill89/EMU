get_model_mcpr <- function(data_type = c("ss","survey"))
{
mcpr <- mcpr_save <- mcpr_2020 <- tibble(year = numeric(),
              mean = numeric(),
             `2.5%` = numeric(),
             `5%` = numeric(),
             `10%` = numeric(),
             `50%` = numeric(),
             `90%` = numeric(),
             `95%` = numeric(),
             `97.5%` = numeric(),
              division_numeric_code = integer(),
              country = character())

meta_dat <- readRDS("data/meta_data.rds")
country_codes <- meta_dat$division_numeric_code
country_names <- meta_dat$name

for(i in 1:length(country_codes)) {
  c_index <- i
  code <- country_codes[c_index]

  if(data_type == "ss")
  {
  read_res <- readRDS(paste0("output_ss","/res_",code,".rds"))
  res <- read_res$Y
  }


  if(data_type == "survey")
  {
    read_res <- readRDS(paste0("output_survey","/res_",code,".rds"))
    res <- read_res$fit # output from an previous fpemlocal version that stored res as fit instead of Y
  }



  mcpr <- res$contraceptive_use_modern %>%
    pivot_wider(names_from = percentile,
                values_from = value) %>%
    filter(year <= 2020) %>%
    mutate(division_numeric_code = code,
           country = paste0(country_names[c_index], "(", code, ")"))

  mcpr_save <- rbind(mcpr_save,mcpr)
}

return(mcpr = mcpr_save)
}

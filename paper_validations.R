# validation results ------------------------------------------------------
library(tidyverse)
valid_res_survey <- readr::read_csv("data/valid_res_survey.csv")

valid_res_survey <- valid_res_survey %>%
  mutate(interval_score = (`97.5` - `2.5%`) + (2/0.05)*(`2.5%` - test_dat)*(test_dat<`2.5%`) + (2/0.05)*(test_dat - `97.5`)*(test_dat>`97.5`))

valid_res_ss <- readr::read_csv("data/valid_res_ss.csv")

valid_res_ss <- valid_res_ss %>%
  mutate(interval_score = (`97.5` - `2.5%`) + (2/0.05)*(`2.5%` - test_dat)*(test_dat<`2.5%`) + (2/0.05)*(test_dat - `97.5`)*(test_dat>`97.5`))



valid_res_survey %>% drop_na() %>% dplyr::summarise(n_within = sum(within_CI),
                                                    coverage = sum(within_CI)/n(),
                                                    ME = mean(err),
                                                    MAE = mean(abs(err)),
                                                    MedE = median(err),
                                                    MedAE = median(abs(err)),
                                                    n = n(),
                                                    RMSE = sqrt(mean(err^2)),
                                                    mean_score = mean(interval_score))


valid_res_ss %>% drop_na() %>% dplyr::summarise(n_within = sum(within_CI),
                                                    coverage = sum(within_CI)/n(),
                                                    ME = mean(err),
                                                    MAE = mean(abs(err)),
                                                    MedE = median(err),
                                                    MedAE = median(abs(err)),
                                                    n = n(),
                                                    RMSE = sqrt(mean(err^2)),
                                                    mean_score = mean(interval_score))


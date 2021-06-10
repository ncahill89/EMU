
Estimating mCPR for married women with custom EMU data
================

## Introduction

In this vignette we obtain estimates of the modern contraceptive
prevalence rate (mCPR) for married women with package survey data and a
custom dataset of Estimated Modern Use (EMU) derived from service
statistics.

1.  [Format of EMU dataset](#emu)
2.  [Fit a one country model](#fit) `fit_fp_c`
3.  [Calculate point estimates for mCPR](#results) `calc_fp_c`
4.  [Plot the point estimates against the survey data](#plot)
    `plot_fp_c`
5.  **ss_type:** the service statistics source type which can be:

    
(i) number of contraceptive commodities distributed to clients and/or
    facilities (clients/facilities);
    
(ii) number of times clients interacted with a provider for
     contraceptive services (visits);
     
(iii) number of current contraceptive users of any method including
      those who are still using longer acting methods that were received
      in previous years (users).

```{r}
emu_data <- readr::read_csv("emu_data.csv")
emu_data
```

## <a name="fit"></a>

## 2. Fit a one-country model using surveys and EMUs

First, find the UNPD country code under the variable
`division_numeric_code` in the dataset `divisions`. See `?divisions` for
the metadata.

```{r}
div_code <- divisions %>%
     dplyr::filter(name_country == "Mozambique")

div_code
```

If using the Track20 survey database included in the package data, you
need to filter the data using `div_code` and save it.

```{r}
t20_survey <- contraceptive_use_track20 %>%
      dplyr::filter(division_numeric_code %in% div_code$division_numeric_code) 

readr::write_csv(t20_survey, "t20_survey.csv")
```

Fit the one-country family planning estimation model with the function
`fit_fp_c`. First, supply the UNPD country code to the argument
`division_numeric_code`. Specify the `is_in_union = "Y"`. By default,
the function `fit_fp_c` utilizes the UNPD contraceptive use survey
dataset, `contraceptive_use` and filters the dataset based on the
aforementioned function arguments. When using the Track20 database
instead of the default, supply the file path of "t20_survey.csv" file to
the argument `surveydata_filepath`. Supply the file path of
"emu_data.csv" containing your country EMU data to the argument
`service_stats_filepath`. Lastly, specify the years of estimates to be
returned. Note: year arguments will not filter the supplied survey data.
All years of available survey data will be used.

```{r}
fit <- fit_fp_c(division_numeric_code = div_code$division_numeric_code,
                is_in_union = "Y",
                surveydata_filepath = "t20_survey.csv",
                service_stats_filepath = "emu_data.csv",
                first_year = 1989,
                last_year = 2030)

```

## <a name="results"></a>

## 3. Calculate point estimates for indicators

Calculate point estimates for family planning indicators with the
function `calc_fp_c`. Supply the fit object from `fit_fp_c` to the
argument `fit`. Read in your population count dataset. Then supply the
dataset to the argument `population_data`.

```{r}
results <- calc_fp_c(fit = fit)
```

A set of results here consist of the following family planning
indicators

```{r}
results$Y %>% names
```

The point estimates for each indicator are long-format tibbles. Let's
take a look at the tibble for the indicator `contraceptive_use_modern`

```{r}
results$Y$contraceptive_use_modern
```

## <a name="plot"></a>

## 3. Plot estimates and data

`fpemlocal` includes a function named `plot_fp_c` to plot the calculated
point estimates against the survey data. The arguments to this function
are, the fit object from step 2, the results from step 3, and a vector
of indicator names. The vector of indicator names corresponds to the
names which appear in the results from step 3.

```{r, fig.width=5.5, fig.height=3.5}
plot_fp_c(
  fit,
  results,
  indicators = c(
    "contraceptive_use_modern")
  )
```


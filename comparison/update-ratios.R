# Create ratios
# Rts: "Combined", "cases", "deaths"


# National ----------------------------------------------------------------
# Split out Rts
rt_national_cases <- all_r %>%
  dplyr::filter(Geography %in% nation_names
         & type == "Cases") %>%
  dplyr::select(date, region = Geography, 
         median_cases = Value, 
         lower50_cases = `Quantile 0.25`, upper50_cases = `Quantile 0.75`,
         lower95_cases = `Quantile 0.05`, upper95_cases = `Quantile 0.95`)

rt_national_deaths <- all_r %>%
  dplyr::filter(Geography %in% nation_names
         & type == "Deaths") %>%
  dplyr::select(date, region = Geography, 
         median_deaths = Value, 
         lower50_deaths = `Quantile 0.25`, upper50_deaths = `Quantile 0.75`,
         lower95_deaths = `Quantile 0.05`, upper95_deaths = `Quantile 0.95`)

# Set ratio
ratio_national <- 
  dplyr::full_join(rt_national_cases, rt_national_deaths, by = c("date")) %>%
  dplyr::mutate(
    # cases / deaths
    median_cases_deaths = median_cases / median_deaths,
    lower50_case_deaths = lower50_cases / lower50_deaths,
    upper50_case_deaths = upper50_cases / upper50_deaths,
    lower95_case_deaths = lower95_cases / lower95_deaths,
    upper95_case_deaths = upper95_cases / upper95_deaths
  )


# Regional ----------------------------------------------------------------
# region_names <- c("regions")

# Split out Rts
rt_regional_deaths <- all_r %>%
  dplyr::filter(Geography %in% region_names
         & type == "Cases") %>%
  dplyr::select(date, region = Geography, 
         median_hospdeaths = Value, 
         lower50_deaths = `Quantile 0.25`, upper50_deaths = `Quantile 0.75`,
         lower95_deaths = `Quantile 0.05`, upper95_deaths = `Quantile 0.95`)

rt_regional_cases <- all_r %>%
  dplyr::filter(Geography %in% region_names
         & type == "Cases") %>%
  dplyr::select(date, region = Geography, 
         median_cases = Value, 
         lower50_cases = `Quantile 0.25`, upper50_cases = `Quantile 0.75`,
         lower95_cases = `Quantile 0.05`, upper95_cases = `Quantile 0.95`)

# Set ratio
ratio_regional <- 
  dplyr::full_join(rt_regional_cases, rt_regional_deaths, by = c("region", "date")) %>%
  dplyr::mutate(
    # cases / deaths
    median_case_deaths = median_cases / median_deaths,
    lower50_case_deaths = lower50_cases / lower50_deaths,
    upper50_case_deaths = upper50_cases / upper50_deaths,
    lower95_case_deaths = lower95_cases / lower95_deaths,
    upper95_case_deaths = upper95_cases / upper95_deaths
    ) 
  

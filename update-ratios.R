# Create ratios
# Rts: "Combined", "Admissions", "Public tests", "All deaths", "Hospital deaths"


# National ----------------------------------------------------------------
# Split out Rts
rt_national_admissions <- all_r %>%
  filter(Geography %in% nation_names
         & type == "Admissions") %>%
  select(date, region = Geography, 
         median_admissions = Value, 
         lower50_admissions = `Quantile 0.25`, upper50_admissions = `Quantile 0.75`,
         lower95_admissions = `Quantile 0.05`, upper95_admissions = `Quantile 0.95`)

rt_national_tests <- all_r %>%
  filter(Geography %in% nation_names
         & type == "Public tests") %>%
  select(date, region = Geography, 
         median_tests = Value, 
         lower50_tests = `Quantile 0.25`, upper50_tests = `Quantile 0.75`,
         lower95_tests = `Quantile 0.05`, upper95_tests = `Quantile 0.95`)

rt_national_deaths <- all_r %>%
  filter(Geography %in% nation_names
         & type == "All deaths") %>%
  select(date, region = Geography, 
         median_deaths = Value, 
         lower50_deaths = `Quantile 0.25`, upper50_deaths = `Quantile 0.75`,
         lower95_deaths = `Quantile 0.05`, upper95_deaths = `Quantile 0.95`)

rt_national_hospdeaths <- all_r %>%
  filter(Geography %in% nation_names
         & type == "Hospital deaths") %>%
  select(date, region = Geography, 
         median_hospdeaths = Value, 
         lower50_hospdeaths = `Quantile 0.25`, upper50_hospdeaths = `Quantile 0.75`,
         lower95_hospdeaths = `Quantile 0.05`, upper95_hospdeaths = `Quantile 0.95`)

# Set ratio
ratio_national <- 
  full_join(rt_national_tests, rt_national_admissions, by = c("region", "date")) %>%
  full_join(rt_national_deaths, by = c("region", "date")) %>%
  full_join(rt_national_hospdeaths, by = c("region", "date")) %>%
  mutate(
    # Public tests / admissions
    median_test_adm = median_tests / median_admissions,
    lower50_test_adm = lower50_tests / lower50_admissions,
    upper50_test_adm = upper50_tests / upper50_admissions,
    lower95_test_adm = lower95_tests / lower95_admissions,
    upper95_test_adm = upper95_tests / upper95_admissions,
    # Public tests / deaths
    median_test_deaths = median_tests / median_deaths,
    lower50_test_deaths = lower50_tests / lower50_deaths,
    upper50_test_deaths = upper50_tests / upper50_deaths,
    lower95_test_deaths = lower95_tests / lower95_deaths,
    upper95_test_deaths = upper95_tests / upper95_deaths,
    # Public tests / hospital deaths
    median_test_hospdeaths = median_tests / median_hospdeaths,
    lower50_test_hospdeaths = lower50_tests / lower50_hospdeaths,
    upper50_test_hospdeaths = upper50_tests / upper50_hospdeaths,
    lower95_test_hospdeaths = lower95_tests / lower95_hospdeaths,
    upper95_test_hospdeaths = upper95_tests / upper95_hospdeaths,
    # Admissions / deaths
    median_adm_deaths = median_admissions / median_deaths,
    lower50_adm_deaths = lower50_admissions / lower50_deaths,
    upper50_adm_deaths = upper50_admissions / upper50_deaths,
    lower95_adm_deaths = lower95_admissions / lower95_deaths,
    upper95_adm_deaths = upper95_admissions / upper95_deaths,
    # Admissions / hospital deaths
    median_adm_hospdeaths = median_admissions / median_hospdeaths,
    lower50_adm_hospdeaths = lower50_admissions / lower50_hospdeaths,
    upper50_adm_hospdeaths = upper50_admissions / upper50_hospdeaths,
    lower95_adm_hospdeaths = lower95_admissions / lower95_hospdeaths,
    upper95_adm_hospdeaths = upper95_admissions / upper95_hospdeaths
  )



# Regional ----------------------------------------------------------------

# Split out Rts
rt_regional_admissions <- all_r %>%
  filter(Geography %in% region_names
         & type == "Admissions") %>%
  select(date, region = Geography, 
         median_admissions = Value, 
         lower50_admissions = `Quantile 0.25`, upper50_admissions = `Quantile 0.75`,
         lower95_admissions = `Quantile 0.05`, upper95_admissions = `Quantile 0.95`)

rt_regional_tests <- all_r %>%
  filter(Geography %in% region_names
         & type == "Public tests") %>%
  select(date, region = Geography, 
         median_tests = Value, 
         lower50_tests = `Quantile 0.25`, upper50_tests = `Quantile 0.75`,
         lower95_tests = `Quantile 0.05`, upper95_tests = `Quantile 0.95`)

rt_regional_deaths <- all_r %>%
  filter(Geography %in% region_names
         & type == "All deaths") %>%
  select(date, region = Geography, 
         median_deaths = Value, 
         lower50_deaths = `Quantile 0.25`, upper50_deaths = `Quantile 0.75`,
         lower95_deaths = `Quantile 0.05`, upper95_deaths = `Quantile 0.95`)

rt_regional_hospdeaths <- all_r %>%
  filter(Geography %in% region_names
         & type == "Hospital deaths") %>%
  select(date, region = Geography, 
         median_hospdeaths = Value, 
         lower50_hospdeaths = `Quantile 0.25`, upper50_hospdeaths = `Quantile 0.75`,
         lower95_hospdeaths = `Quantile 0.05`, upper95_hospdeaths = `Quantile 0.95`)

# Set ratio
ratio_regional <- 
  full_join(rt_regional_tests, rt_regional_admissions, by = c("region", "date")) %>%
  full_join(rt_regional_deaths, by = c("region", "date")) %>%
  full_join(rt_regional_hospdeaths, by = c("region", "date")) %>%
  mutate(
    # Public tests / admissions
    median_test_adm = median_tests / median_admissions,
    lower50_test_adm = lower50_tests / lower50_admissions,
    upper50_test_adm = upper50_tests / upper50_admissions,
    lower95_test_adm = lower95_tests / lower95_admissions,
    upper95_test_adm = upper95_tests / upper95_admissions,
    # Public tests / deaths
    median_test_deaths = median_tests / median_deaths,
    lower50_test_deaths = lower50_tests / lower50_deaths,
    upper50_test_deaths = upper50_tests / upper50_deaths,
    lower95_test_deaths = lower95_tests / lower95_deaths,
    upper95_test_deaths = upper95_tests / upper95_deaths,
    # Public tests / hospital deaths
    median_test_hospdeaths = median_tests / median_hospdeaths,
    lower50_test_hospdeaths = lower50_tests / lower50_hospdeaths,
    upper50_test_hospdeaths = upper50_tests / upper50_hospdeaths,
    lower95_test_hospdeaths = lower95_tests / lower95_hospdeaths,
    upper95_test_hospdeaths = upper95_tests / upper95_hospdeaths,
    # Admissions / deaths
    median_adm_deaths = median_admissions / median_deaths,
    lower50_adm_deaths = lower50_admissions / lower50_deaths,
    upper50_adm_deaths = upper50_admissions / upper50_deaths,
    lower95_adm_deaths = lower95_admissions / lower95_deaths,
    upper95_adm_deaths = upper95_admissions / upper95_deaths,
    # Admissions / hospital deaths
    median_adm_hospdeaths = median_admissions / median_hospdeaths,
    lower50_adm_hospdeaths = lower50_admissions / lower50_hospdeaths,
    upper50_adm_hospdeaths = upper50_admissions / upper50_hospdeaths,
    lower95_adm_hospdeaths = lower95_admissions / lower95_hospdeaths,
    upper95_adm_hospdeaths = upper95_admissions / upper95_hospdeaths
    ) 
  

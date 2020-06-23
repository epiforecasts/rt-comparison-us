# Rt from public hospital deaths data
# Produces estimates for 7 English regions + England
# Rts based on NHS reported deaths in hospitals
# 
# Packages -----------------------------------------------------------------
require(EpiNow)
require(data.table)
require(forecastHybrid)
require(future)
require(dplyr)
require(tidyr)
require(magrittr)

# Define a target date ----------------------------------------------------

target_date <- Sys.Date()

# Read in delay --------------------------------------------------------

delay_dists <- readRDS(here::here("nowcast", "data", "onset_to_death_delay.rds"))

# Read in incubation period -----------------------------------------------

incubation_defs <- readRDS(here::here("nowcast", "data", "incubation.rds"))

# Get deaths  ---------------------------------------------------------------
source(here::here("utils/get-hospital-deaths.R"))

deaths <- readRDS(here::here("data/hospital_deaths.rds"))

deaths_national <- deaths %>%
  dplyr::filter(region %in% c("Northern Ireland", "England", "Scotland", "Wales")) %>% 
  dplyr::rename(local = deaths) %>% 
  dplyr::mutate(imported = 0) %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 

deaths_regional <- deaths %>%
  dplyr::filter(!region %in% c("Northern Ireland", "England", "Scotland", "Wales")) %>% 
  dplyr::rename(local = deaths) %>% 
  dplyr::mutate(imported = 0) %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 



if (all(!deaths_national$region %in% "England")) {
  deaths_england <- deaths_regional %>% 
    dplyr::group_by(date, import_status) %>% 
    dplyr::summarise(confirm = sum(confirm)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(region = "England")
  
  deaths_national <- deaths_england %>% 
    dplyr::bind_rows(deaths_national)
}


max_date <- min(data.table::as.data.table(deaths_national)[, .SD[date == max(date)], by = region]$date)


# Define nowcast lag at the 40% quantile ----------------------------------

nowcast_lag <- 9 + 5 # Delay from death -> onset + onset -> infection

# # Set up cores -----------------------------------------------------
setup_future <- function(jobs) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }
  
  
  plan(list(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
            tweak(multiprocess, workers = max(1, round(future::availableCores() / jobs)))),
       gc = TRUE, earlySignal = TRUE)
}


setup_future(length(unique(deaths_national$region)))

## National
EpiNow::regional_rt_pipeline(
  cases = deaths_national,
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/hospital-deaths/national",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  min_forecast_cases = 25,
  approx_delay = TRUE,
  horizon = 0)

setup_future(length(unique(deaths_regional$region)))

## Regional 
EpiNow::regional_rt_pipeline(
  cases = deaths_regional, 
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/hospital-deaths/regional", 
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  min_forecast_cases = 25,
  approx_delay = TRUE,
  horizon = 0)

# Summarise results -------------------------------------------------------


EpiNow::regional_summary(results_dir = "nowcast/hospital-deaths/national", 
                         summary_dir = "nowcast/hospital-deaths/national-summary",
                         target_date = "latest",
                         region_scale = "Country")


EpiNow::regional_summary(results_dir = "nowcast/hospital-deaths/regional", 
                         summary_dir = "nowcast/hospital-deaths/regional-summary",
                         target_date = "latest",
                         region_scale = "Region")

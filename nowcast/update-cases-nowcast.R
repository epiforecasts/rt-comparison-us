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

delay_dists <- readRDS(here::here("nowcast", "data", "onset_to_admission_delay.rds"))


# Read in incubation period -----------------------------------------------

incubation_defs <- readRDS(here::here("nowcast", "data", "incubation.rds"))

# Get cases  ---------------------------------------------------------------

cases <- readRDS(here::here("nowcast", "data", "cases.rds")) %>% 
  dplyr::select(date, region = geography, cases = value)

cases_national <- cases %>%
  dplyr::filter(region %in% c("Northern Ireland", "England", "Scotland", "Wales")) %>% 
  dplyr::rename(local = cases) %>% 
  dplyr::mutate(imported = 0) %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 

# max_date <- min(data.table::as.data.table(cases_national)[, .SD[date == max(date)], 
#                                                           by = region]$date)
# 
# uk_cases <- data.table::as.data.table(cases_national)[date <= max_date][, 
#            .(confirm = sum(confirm, na.rm = TRUE), region = "United Kingdom"),
#            by = .(date, import_status)]
# 
# data.table::setorder(uk_cases, import_status, date)
# 
# cases_national <- data.table::rbindlist(list(uk_cases, cases_national), use.names = TRUE)

cases_regional <- cases %>%
  dplyr::filter(!region %in% c("Northern Ireland", "England", "Scotland", "Wales")) %>% 
  dplyr::rename(local = cases) %>% 
  dplyr::mutate(imported = 0) %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 


# Define nowcast lag at the 40% quantile ----------------------------------

nowcast_lag <- 8 # Delay from report -> onset + onset -> infection

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


setup_future(length(unique(cases_national$region)))

## National
EpiNow::regional_rt_pipeline(
  cases = cases_national,
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/linelist/national",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  min_forecast_cases = 25,
  approx_delay = TRUE,
  horizon = 0)

setup_future(length(unique(cases_regional$region)))

## Regional 
EpiNow::regional_rt_pipeline(
  cases = cases_regional, 
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/linelist/regional", 
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  min_forecast_cases = 25,
  approx_delay = TRUE,
  horizon = 0)


# Summarise results -------------------------------------------------------

# 
# EpiNow::regional_summary(results_dir = "nowcast/linelist/national", 
#                          summary_dir = "nowcast/linelist/national-summary",
#                          target_date = "latest",
#                          region_scale = "Country")


EpiNow::regional_summary(results_dir = "nowcast/linelist/regional", 
                         summary_dir = "nowcast/linelist/regional-summary",
                         target_date = "latest",
                         region_scale = "Region")

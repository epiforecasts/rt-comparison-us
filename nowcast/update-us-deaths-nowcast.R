# Nowcast Rt and estimated infections and deaths
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

# Read in delay from onset to death --------------------------------------------------------

death_delay_dists <- readRDS(here::here("nowcast", "data", "onset_to_death_delay.rds"))

# Read in incubation period -----------------------------------------------

incubation_defs <- readRDS(here::here("nowcast", "data", "incubation.rds"))

# Get and reshape deaths data ---------------------------------------------------------------

deaths <- readRDS(here::here("data", "deaths_data.rds"))

deaths_national <- deaths %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(deaths = sum(deaths)) %>%
  dplyr::rename(local = deaths) %>%
  dplyr::mutate(imported = 0, region = "US") %>%
  tidyr::gather(key = "import_status", value = "confirm", local, imported)

deaths_regional <- deaths %>%
  dplyr::rename(local = deaths, region = state) %>%
  dplyr::mutate(imported = 0) %>%
  tidyr::gather(key = "import_status", value = "confirm", local, imported)

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

# Estimate Rt and death counts ----------------------------------------------------------------

## National
setup_future(length(unique(deaths_national$region)))

EpiNow::regional_rt_pipeline(
  cases = deaths_national,
  delay_defs = death_delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/deaths/national",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 0,
  approx_delay = TRUE,
  horizon = 0)

## Regional

setup_future(length(unique(deaths_regional$region)))

EpiNow::regional_rt_pipeline(
  cases = deaths_regional,
  delay_defs = death_delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/deaths/state",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 0,
  approx_delay = TRUE,
  horizon = 0)

# Summarise results -------------------------------------------------------

EpiNow::regional_summary(results_dir = "nowcast/deaths/national",
                         summary_dir = "nowcast/deaths/national-summary",
                         target_date = "latest",
                         region_scale = "Country")

EpiNow::regional_summary(results_dir = "nowcast/deaths/state",
                         summary_dir = "nowcast/deaths/state-summary",
                         target_date = "latest",
                         region_scale = "State")

# Nowcast Rt and estimated infections and cases
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

case_delay_dists <- readRDS(here::here("nowcast", "data", "onset_to_admission_delay.rds"))

# Read in incubation period -----------------------------------------------

incubation_defs <- readRDS(here::here("nowcast", "data", "incubation.rds"))

# Get and reshape cases data ---------------------------------------------------------------

cases <- readRDS(here::here("data", "case_data.rds"))

cases_national <- cases %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::rename(local = cases) %>%
  dplyr::mutate(imported = 0, region = "US") %>%
  tidyr::gather(key = "import_status", value = "confirm", local, imported)

cases_state <- cases %>%
  dplyr::rename(local = cases, region = state) %>%
  dplyr::select(-epiweek) %>%
  dplyr::mutate(imported = 0) %>%
  tidyr::gather(key = "import_status", value = "confirm", local, imported)

# Define nowcast lag at the 40% quantile ----------------------------------

nowcast_lag <- 8 # Infection -> symptom (5 days) + symptom -> report (3 days) 

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

# Estimate Rt and case counts ----------------------------------------------------------------

## National
setup_future(length(unique(cases_national$region)))

EpiNow::regional_rt_pipeline(
  cases = cases_national,
  delay_defs = case_delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/cases/national",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 0,
  approx_delay = TRUE,
  horizon = 0)

## Regional

setup_future(length(unique(cases_state$region)))

EpiNow::regional_rt_pipeline(
  cases = cases_state,
  delay_defs = case_delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "nowcast/cases/state",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 25,
  approx_delay = TRUE,
  horizon = 0)

# Summarise results -------------------------------------------------------

EpiNow::regional_summary(results_dir = "nowcast/cases/national",
                        summary_dir = "nowcast/cases/national-summary",
                        target_date = "latest",
                        region_scale = "Country")

EpiNow::regional_summary(results_dir = "nowcast/cases/state",
                         summary_dir = "nowcast/cases/state-summary",
                         target_date = "latest",
                         region_scale = "State")

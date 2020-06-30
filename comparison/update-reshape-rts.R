# Packages ----------------------------------------------------------------

require(data.table, quietly = TRUE)
require(EpiNow, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(cowplot, quietly = TRUE)

library(dplyr)
library(magrittr)

# Target date -------------------------------------------------------------

target <- Sys.Date()

# Get function ------------------------------------------------------------

source(here::here("utils/report-rt.R"))

# Get national Rts -------------------------------------------------------------
national <- list(
  deaths = report_rt("nowcast/deaths/national", creation_date = target)[, type := "Deaths"],
  cases = report_rt("nowcast/cases/national", creation_date = target)[, type := "Cases"]
  )

national <- data.table::rbindlist(national)

nation_names <- unique(national$Geography)

# Get regions Rts -------------------------------------------------------------

# england_regions <- list(
#   deaths_all = report_rt("nowcast/deaths/regional", creation_date = target)[, type := "All deaths"],
#   cases_admissions = report_rt("nowcast/regional", creation_date = target)[, type := "Admissions"],
#   cases_public = report_rt("nowcast/linelist/regional", creation_date = target)[, type := "Public tests"],
#   deaths_hospital = report_rt("nowcast/hospital-deaths/regional", creation_date = target)[, type := "Hospital deaths"]
# )
# 
# england_regions <- data.table::rbindlist(england_regions)
# 
# region_names <- unique(england_regions$Geography)

# Join national and regional Rts ------------------------------------------
overall <- national %>%
  dplyr::select(-Version)

# overall <- data.table::rbindlist(list(
#   national, 
#   england_regions
# ))
# 

# # Remove dates that don't have data for all estimates ---------------------
# 
combined <- data.table::copy(overall)[, estimates := .N,
                                                        by = .(`Day of Value`, `Month of Value`,
                                                        `Year of Value`, Geography)][,
                                                                                 .SD[estimates == max(estimates)], by = "Geography"][,estimates := NULL]


# 
# # Quantile average --------------------------------------------------------
# 
combined <- combined[, type := NULL][,
                                     lapply(.SD, mean),
                                     by = .(Group, `Creation Day`, `Creation Month`,
                                            `Creation Year`, `Day of Value`, `Month of Value`,
                                            `Year of Value`, Geography, ValueType,
                                            Model),
                                     .SDcols = c("Value", colnames(overall)[grep("Quantile", colnames(overall))])]


# Quantile average of all estimate types --------------------------------------
combined_all <- data.table::copy(overall)[, estimates := .N, 
                                                            by = .(`Day of Value`, `Month of Value`, `Year of Value`, Geography)][,
                                                                                          .SD[estimates == max(estimates)], by = "Geography"][,
                                                                                                                                              estimates := NULL]

combined_all <- combined_all[, type := NULL][, 
                                     lapply(.SD, mean),
                                     by = .(Group, `Creation Day`, `Creation Month`, 
                                            `Creation Year`, `Day of Value`, `Month of Value`, 
                                            `Year of Value`, Geography, ValueType,
                                            Model),
                                     .SDcols = c("Value", colnames(overall)[grep("Quantile", colnames(overall))])]


# Join & factor for plotting -------------------------------------------------------------

all_r <- data.table::rbindlist(list(
  overall, 
  combined_all[, type := "Combined"]))

all_r <- all_r[, date := lubridate::dmy(paste0(`Day of Value`, "-", `Month of Value`, "-", `Year of Value`))][,
                 type := factor(type, levels = c("Combined", "Cases", "Deaths"))]

# Check what data are present where
# View(table(all_r$Geography, all_r$type))

# all_r$Geography <- factor(all_r$Geography, 
#                                     levels = c("England", "Scotland", "Wales", "Northern Ireland", 
#                                                "East of England", "London", "Midlands", "North East and Yorkshire", "North West", "South East", "South West"))



# Last data point ---------------------------------------------------------

last_estimate <- data.table::copy(all_r)[, estimates := .N, by = .(`Day of Value`,
                                                                   `Month of Value`,
                                                                   `Year of Value`, 
                                                                   Geography)]

last_estimate <- last_estimate[, .SD[estimates == max(estimates)], by = "Geography"][,
                                                                                     estimates := NULL][,.SD[date == max(date)], by = "Geography"]


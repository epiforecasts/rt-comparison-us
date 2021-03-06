# Update raw case and death data

source(here::here("utils", "get-us-data.R"))
case_data <- get_us_cases(data = "daily")
deaths_data <- get_us_deaths(data = "daily")

saveRDS(case_data, here::here("data", "case_data.rds"))
saveRDS(deaths_data, here::here("data", "deaths_data.rds"))

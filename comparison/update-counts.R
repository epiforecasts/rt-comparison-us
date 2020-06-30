# Count data

# Deaths
count_deaths <- readRDS(here::here("data/deaths_data.rds")) 

national_count_deaths <- count_deaths %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths))

# Cases
count_cases <- readRDS(here::here("data/case_data.rds")) 

national_count_cases <- count_cases %>%
  group_by(date) %>%
  summarise(cases = sum(cases))

# Set national counts -----------------------------------------------------

count_all_national <- left_join(national_count_cases, national_count_deaths, by = c("date")) %>%
  filter(date >= min(ratio_national$date) & date <= max(ratio_national$date)) %>%
  rename(Cases = cases, Deaths = deaths) %>%
  pivot_longer(cols = c(Cases, Deaths)) %>%
  group_by(name) %>%
  mutate(z_score = scale(value, center = TRUE, scale = TRUE)) %>%
  ungroup() %>%
  rename("Data source" = name, Date = date)

count_all_national$`Data source` <- factor(count_all_national$`Data source`, 
                                           levels = c("Combined", "Cases", "Deaths"))


# Set regional counts -----------------------------------------------------
# 
# count_all_regional <- left_join(count_tests, count_admissions, by = c("region", "date")) %>%
#   left_join(count_deaths, by = c("region", "date")) %>%
#   left_join(count_hospdeaths, by = c("region", "date")) %>%
#   filter(region %in% region_names) %>%
#   filter(date >= min(ratio_national$date) & date <= max(ratio_national$date)) %>%
#   pivot_longer(cols = c(cases_tests, cases_admissions, all_deaths, hosp_deaths)) %>%
#   group_by(region, name) %>%
#   mutate(z_score = scale(value, center = TRUE, scale = TRUE)) %>%
#   ungroup() %>%
#   rename("Data source" = name, Date = date)
# 
# count_all_regional$`Data source` <- recode(count_all_regional$`Data source`, 
#                                   cases_tests = "Public tests",
#                                   cases_admissions = "Admissions", 
#                                   all_deaths = "All deaths",
#                                   hosp_deaths = "Hospital deaths")
# 
# count_all_regional$`Data source` <- factor(count_all_regional$`Data source`, 
#                                   levels = c("Combined", "Public tests", "Admissions", "All deaths", "Hospital deaths"))
# 

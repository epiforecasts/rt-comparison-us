# Count data

# Public tests

count_tests_national <- NCoVUtils::get_uk_regional_cases(geography = "all countries") %>%
  group_by(country, date) %>%
  summarise(cases_tests = sum(cases)) %>%
  rename(region = country)

count_tests_regional <- readRDS(here::here("nowcast/data/cases.rds")) %>%
  rename(cases_tests = value, region = geography) %>%
  filter(region %in% region_names) %>%
  select(date, region, cases_tests)

count_tests <- bind_rows(count_tests_national, count_tests_regional)

# Admissions
count_admissions <- as.data.table(readRDS(here::here("data/hospital_admissions.rds"))) %>%
  mutate(date = as.Date(date)) %>%
  rename(cases_admissions = cases)

# All deaths
count_deaths <- readRDS(here::here("data/linelist_deaths.rds")) %>%
  rename(all_deaths = deaths)

# Hospital deaths
count_hospdeaths <- readRDS(here::here("data/hospital_deaths.rds")) %>%
  rename(hosp_deaths = deaths)

# Set national counts -----------------------------------------------------

count_all_national <- left_join(count_tests, count_admissions, by = c("region", "date")) %>%
  left_join(count_deaths, by = c("region", "date")) %>%
  left_join(count_hospdeaths, by = c("region", "date")) %>%
  filter(region %in% nation_names) %>%
  filter(date >= min(ratio_national$date) & date <= max(ratio_national$date)) %>%
  pivot_longer(cols = c(cases_tests, cases_admissions, all_deaths, hosp_deaths)) %>%
  group_by(region, name) %>%
  mutate(z_score = scale(value, center = TRUE, scale = TRUE)) %>%
  ungroup() %>%
  rename("Data source" = name, Date = date)

count_all_national$`Data source` <- recode(count_all_national$`Data source`, 
                                  cases_tests = "Public tests",
                                  cases_admissions = "Admissions", 
                                  all_deaths = "All deaths",
                                  hosp_deaths = "Hospital deaths")

count_all_national$`Data source` <- factor(count_all_national$`Data source`, 
                                           levels = c("Combined", "Public tests", "Admissions", "All deaths", "Hospital deaths"))

count_all_national$region <- factor(count_all_national$region, 
                                  levels = c("England", "Scotland", "Wales", "Northern Ireland"))


# Set regional counts -----------------------------------------------------

count_all_regional <- left_join(count_tests, count_admissions, by = c("region", "date")) %>%
  left_join(count_deaths, by = c("region", "date")) %>%
  left_join(count_hospdeaths, by = c("region", "date")) %>%
  filter(region %in% region_names) %>%
  filter(date >= min(ratio_national$date) & date <= max(ratio_national$date)) %>%
  pivot_longer(cols = c(cases_tests, cases_admissions, all_deaths, hosp_deaths)) %>%
  group_by(region, name) %>%
  mutate(z_score = scale(value, center = TRUE, scale = TRUE)) %>%
  ungroup() %>%
  rename("Data source" = name, Date = date)

count_all_regional$`Data source` <- recode(count_all_regional$`Data source`, 
                                  cases_tests = "Public tests",
                                  cases_admissions = "Admissions", 
                                  all_deaths = "All deaths",
                                  hosp_deaths = "Hospital deaths")

count_all_regional$`Data source` <- factor(count_all_regional$`Data source`, 
                                  levels = c("Combined", "Public tests", "Admissions", "All deaths", "Hospital deaths"))


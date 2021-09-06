## -------------------------
##
## Script name: isalugu_covid.R
## Purpose of script: Check results of https://isalugu.blogspot.com/2021/09/pohjalik-arvutus-viie-kuuga-suri-ligi.html
## Author: Mart Roben
## Date Created: 6 Sept 2021
##
## Copyright: BSD-3-Clause
## https://github.com/martroben/isalugu_covid
##
## Contact: fb.com/mart.roben
##
## ---------------------------
##
## Notes:
##
## Results:
### 543 excess deaths in vaccinated group Jan - May 2021 (~25 % more than expected)
##
## Limitatons:
### 2021 death statistics are preliminary (https://www.stat.ee/et/avasta-statistikat/viiruse-moju-eestile/surmade-kiirstatistika)
### No age group data available for deaths.
### The number of vaccination doesn't take into account people who completed their vaccination with 1 done
### (ie. covid survivers and some vaccine brands that only need 1 dose)
### Could be confounded by vaccines having been given preferentially to individuals with high health risks.
### Death rate in vaccinated group and general population is probably not the same.
##
## Perspectives:
### Get data on all 1st vaccine doses
### Check data for each month separately
### Use deaths data by age groups
### Check data from other countries
##
## ---------------------------



#################
# Load packages #
#################


if(!require(pacman)) install.packages("pacman")

pacman::p_load("magrittr",
               "tibble",
               "tidyr",
               "dplyr",
               "readr",
               "stringr",
               "lubridate",
               "purrr",
               "httr",
               "jsonlite")



############################
# Input data and functions #
############################


# Total deaths by month 2021
# From https://isalugu.blogspot.com/2021/09/pohjalik-arvutus-viie-kuuga-suri-ligi.html
deaths_by_month <- tibble::tribble(
  
  ~month, ~deaths_all_age,
  1,      1680,
  2,      1456,
  3,      1797,
  4,      1614,
  5,      1438
)


# 2021 Jan - May total deaths of vaccinated (at least once) and unvaccinated people
# From https://isalugu.blogspot.com/2021/09/pohjalik-arvutus-viie-kuuga-suri-ligi.html
vacc_total_deaths <- 2702
unvacc_total_deaths <- 5283


# Est Covid vaccination data url
vacc_data_url <- "https://opendata.digilugu.ee/opendata_covid19_vaccination_county_agegroup_gender.csv"

# Est stat API base url
stat_api_url <- "https://andmed.stat.ee/api/v1/en/stat/"


# Function to create Est stat API request body in json format
make_stat_api_json <- function(sugu, aasta, vanuseryhm) {
  
  as_char_list <- function(x) {
    
    x %>%
      unlist() %>%
      purrr::map(~as.character(.))
  }
  
  json_body <- list(
    
    query = list(
      list(code = "Sugu",
           selection = list(filter = "item",
                            values = as_char_list(sugu))),
      list(code = "Aasta",
           selection = list(filter = "item",
                            values = as_char_list(aasta))),
      list(code = "Vanuserühm",
           selection = list(filter = "item",
                            values = as_char_list(vanuseryhm)))
    ),
    
    response = 
      list(format = "csv")
  )
  
  jsonlite::toJSON(json_body, pretty = TRUE, auto_unbox = TRUE)
}


# Function to divide Est stat data into same age groups as vaccination data
# Assuming each subgroup (year) in 15-19 age group has equal number of people
match_age_groups <- function(x) {

  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`0-4` = rowSums(across(any_of(c("0", "1-4", "0-4"))), na.rm = TRUE),
                  `0_17` = sum(`0-4`, `5-9`, `10-14`, 3/5 * `15-19`, na.rm = TRUE),
                  `18_29` = sum(2/5 * `15-19`, `20-24`, `25-29`, na.rm = TRUE),
                  `30_39` = sum(`30-34`, `35-39`, na.rm = TRUE),
                  `40_49` = sum(`40-44`, `45-49`, na.rm = TRUE),
                  `50_59` = sum(`50-54`, `55-59`, na.rm = TRUE),
                  `60_69` = sum(`60-64`, `65-69`, na.rm = TRUE),
                  `70_79` = sum(`70-74`, `75-79`, na.rm = TRUE),
                  `80_plus` = rowSums(across(any_of(c("80-84", "85-89", "90-94", "95-99", "100 and older", "100 and over"))), na.rm = TRUE)) %>%
    dplyr::select(Year, `0_17`, `18_29`, `30_39`, `40_49`, `50_59`, `60_69`, `70_79`, `80_plus`) %>%
    dplyr::ungroup()
}


# Make Estonian statistics API request full url-s:
# RV021: POPULATION BY SEX AND AGE GROUP,
# RV45: DEATHS BY SEX AND AGE GROUP
stat_pop_data_url <- stringr::str_c(stat_api_url, "RV021")
stat_death_data_url <- stringr::str_c(stat_api_url, "RV45")


# Pull vaccination data
vacc_data_raw <- readr::read_csv(vacc_data_url, show_col_types = FALSE)


# Pull population data for all ages (2021, Jan 1)
stat_pop_data_json <- make_stat_api_json(
  sugu = 1,
  aasta = 2021,
  vanuseryhm = c(1:21, 31:34)
)

pop_data_raw <- httr::POST(stat_pop_data_url, body = stat_pop_data_json)


# Pull 2016-2020 death data for all ages
stat_death_data_json <- make_stat_api_json(
  sugu = 1,
  aasta = 2016:2020,
  vanuseryhm = c(1:20, str_c("MSG", 19:22))
)

death_data_raw <- httr::POST(stat_death_data_url, body = stat_death_data_json)



################
# Calculations #
################


# Population data by matching age groups
pop_by_age_group <- pop_data_raw %>%
  httr::content(show_col_types = FALSE) %>%
  match_age_groups()


# Deaths data by matching age groups
deaths_by_age_group <- death_data_raw %>%
  httr::content(show_col_types = FALSE) %>%
  match_age_groups()


# Proportion of deaths in different age groups (2016-2020 average)
prop_deaths_by_age_group <- deaths_by_age_group %>%
  dplyr::mutate(total = rowSums(across(!Year))) %>%
  dplyr::mutate(across(!Year & !total, ~ .x / total)) %>%
  dplyr::summarise(across(!Year & !total, mean))


# Cumulative number of vaccinated people by age group, by date
# Using the number of "in progress" vaccinations (1st of 2 doses) performed as a proxy for number of vaccinated people (not quite correct)
vacc_by_age_group <- vacc_data_raw %>%
  dplyr::filter(stringr::str_detect(VaccinationStatus, "(?i)InProgress")) %>%
  tidyr::pivot_wider(names_from = AgeGroup, values_from = TotalCount, values_fill = 0) %>%
  dplyr::group_by(StatisticsDate) %>%
  dplyr::summarise(across(is.numeric & !c("TargetDiseaseCode", "CountryEHAK", "CountyEHAK", "DailyCount"), sum))


# Monthly average (mean of all days) proportion of population vaccinated, by age group
proportion_vaccinated <- vacc_by_age_group %>%
  dplyr::mutate(
    
    `0_17` = `0-17` / pop_by_age_group$`0_17`,
    `18_29` = `18-29` / pop_by_age_group$`18_29`,
    `30_39` = `30-39` / pop_by_age_group$`30_39`,
    `40_49` = `40-49` / pop_by_age_group$`40_49`,
    `50_59` = `50-59` / pop_by_age_group$`50_59`,
    `60_69` = `60-69` / pop_by_age_group$`60_69`,
    `70_79` = `70-79` / pop_by_age_group$`70_79`,
    `80_plus` = `80+` / pop_by_age_group$`80_plus`
    ) %>%
  dplyr::select(StatisticsDate, matches("_")) %>%
  dplyr::group_by(month = month(StatisticsDate), year = year(StatisticsDate)) %>%
  dplyr::summarise(across(is.numeric, mean), .groups = "drop")


# Expected number of deaths by age group, Jan - May 2021
# Using historical proportion of deaths in different age groups (not quite correct during corona)
exp_deaths_by_age_group <- deaths_by_month %>%
  dplyr::full_join(prop_deaths_by_age_group, by = character()) %>%
  dplyr::mutate(across(where(is.numeric) & !deaths_all_age & !month, ~.x * deaths_all_age)) %>%
  dplyr::select(-deaths_all_age)


# Expected number of deaths in vaccinated people, by age group, Jan - May 2021
exp_vacc_deaths <- proportion_vaccinated %>%
  dplyr::filter(month %in% 1:5) %>%
  dplyr::select(-month, -year) %>%
  as.matrix() %>%
  magrittr::multiply_by(
    exp_deaths_by_age_group %>%
      dplyr::select(-month) %>%
      as.matrix()
    ) %>%
  dplyr::bind_cols(month = 1:5)


# Estimated excess deaths in vaccinated group (all ages)
est_excess_vacc_deaths <- exp_vacc_deaths %>%
  dplyr::summarise(across(matches("_"), sum)) %>%
  sum() %>%
  magrittr::subtract(vacc_total_deaths, .) %>%
  round(0)


# Display output
stringr::str_c("\n\nEstimated excess deaths in vaccinated group in Estonia: ", est_excess_vacc_deaths,
               "\n(Jan-May 2021)\n\n") %>% cat


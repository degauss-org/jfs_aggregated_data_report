# #test file
# d <- read_csv('test/simulated_jfs_data_geocoded_all_years_bigger_2.csv',
#               col_types = cols(INTAKE_ID = col_character(),
#                                SCREENING_DECISION = col_character(),
#                                #DECISION_DATE = col_character(),
#                                DECISION_DATE = col_date(format = "%m/%d/%Y"),
#                                BIRTH_DATE = col_date(format = "%m/%d/%Y"),
#                                PERSON_ID = col_character(),
#                                RACE = col_character(),
#                                ADDRESS_START = col_date(),
#                                MANDATED_REPORTER = col_character(),
#                                #  REPORTER_PERSON_ID = col_character(),
#                                address_type = col_character(),
#                                address = col_character(),
#                                bad_address = col_logical(),
#                                PO = col_logical(),
#                                lat = col_double(),
#                                lon = col_double(),
#                                score = col_double(),
#                                precision = col_character(),
#                                precise_geocode = col_logical(),
#                                fips_tract_id = col_character(),
#                                fraction_assisted_income = col_double(),
#                                fraction_high_school_edu = col_double(),
#                                median_income = col_double(),
#                                fraction_no_health_ins = col_double(),
#                                fraction_poverty = col_double(),
#                                fraction_vacant_housing = col_double(),
#                                dep_index = col_double()
#               ))
# 
# options(dplyr.summarise.inform = FALSE)

##special test dataset to check for multiple entries per week
# d <- read_csv('test/simulated_jfs_data_concentrated.csv')
# d$DECISION_DATE <- as.Date(d$DECISION_DATE, format = "%m/%d/%Y")
# d$BIRTH_DATE <- as.Date(d$BIRTH_DATE, format = "%m/%d/%Y")
##

# consider 'SCREENED IN AR' same as 'SCREENED IN'
d <- d %>%
  mutate(screened_in = SCREENING_DECISION %in% c("SCREENED IN", "SCREENED IN AR"))

two_weeks <- seq(as.Date("2017-01-01"), by = "14 days", length.out = 130) %>%
  as_tibble() %>%
  mutate(two_week_num = seq(1, 130, by = 1)) %>%
  rename( "two_week_date" = value)

d <- d %>%
  filter(!duplicated(INTAKE_ID)) %>%
  select( -ADDRESS_START, -address_type) %>%
  mutate(week = lubridate::week(DECISION_DATE),
         two_week = findInterval(DECISION_DATE, two_weeks$two_week_date),
         year = lubridate::year(DECISION_DATE)) %>%
  left_join(two_weeks, by = c("two_week" = "two_week_num"))

#get age, sort into groups
d <- d %>%
  mutate(BIRTH_DATE = replace_na(BIRTH_DATE, as.Date('1900-01-01'))) %>%
  mutate(age = as.numeric(difftime(DECISION_DATE, BIRTH_DATE, units = "weeks")/52.25)) %>%
  mutate(age_grp = case_when(age <= 5.0 ~ "zero_five",
                             age < 19 ~ "five_plus",
                             age >= 19 ~ "no_birth_date"))
          
##Annual counts
d_yearly <- d %>%
  group_by(year) %>%
  summarize(num_year = n())

print(knitr::kable(d_yearly))

##

### Weekly Counts
d_neigh <- d  %>%
  #mutate(fips_tract_id = as.character(fips_tract_id)) %>% #comment these out when testing with concentrated data
  #left_join(tract_to_neighborhood, by='fips_tract_id') %>%
  filter(!is.na(DECISION_DATE))

d_neigh <- d_neigh %>%
  mutate(neighborhood = ifelse(is.na(lat), 'Missing', neighborhood))

screen_neighborhood <- d_neigh %>%
  group_by(neighborhood, year, two_week) %>%
  summarise(n_screened_in_all = sum(screened_in == TRUE, na.rm = TRUE),
            n_calls_all = n(),
            n_screened_in_less_5 = sum(screened_in == TRUE & age_grp == 'zero_five', na.rm = TRUE),
            n_calls_less_5 = length(INTAKE_ID[age_grp == 'zero_five']),
            n_screened_in_greater_5 = sum(screened_in == TRUE & age_grp == 'five_plus', na.rm = TRUE),
            n_calls_greater_5 = length(INTAKE_ID[age_grp == 'five_plus']),
            n_screened_in_no_bd = sum(screened_in == TRUE & age_grp == 'no_birth_date', na.rm = TRUE),
            n_calls_no_bd = length(INTAKE_ID[age_grp == 'no_birth_date']),
            n_screened_in_MR = sum(screened_in == TRUE & MANDATED_REPORTER == 'Yes', na.rm = TRUE),
            n_calls_MR = length(INTAKE_ID[MANDATED_REPORTER == 'Yes']),
            n_screened_in_NON_MR = sum(screened_in == TRUE & MANDATED_REPORTER == 'No', na.rm = TRUE),
            n_calls_NON_MR = length(INTAKE_ID[MANDATED_REPORTER == 'No'])
            ) %>%
  ungroup() %>% 
  group_by(year, two_week) %>%
  bind_rows(summarize(., across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
  

screen_neighborhood_rate <- screen_neighborhood %>%
  group_by(neighborhood, year, two_week) %>%
  mutate(screen_in_rate_all = round(n_screened_in_all/n_calls_all,2),
         screen_in_rate_less_5 = round(n_screened_in_less_5/n_calls_less_5,2),
         screen_in_rate_greater_5 = round(n_screened_in_greater_5/n_calls_greater_5,2),
         screen_in_rate_no_bd = round(n_screened_in_no_bd/n_calls_no_bd,2),
         screen_in_rate_MR = round(n_screened_in_MR/n_calls_MR,2),
         screen_in_rate_NON_MR = round(n_screened_in_NON_MR/n_calls_NON_MR,2)
         ) %>%
  mutate(across(starts_with('n_'),
            ~ifelse(.x < 5, NA, .x)))

##
d_csv <- screen_neighborhood_rate
# d_csv <- screen_neighborhood_rate %>%
#   select(neighborhood, year, two_week,
#          number_of_calls = n_calls,
#          number_of_calls_screened_in = n_screened_in,
#          screen_in_rate = screen_in_rate)

path <- "/tmp/"
write.csv(d_csv, paste(path, "weekly_report_v4.1.0.csv", sep = ''))


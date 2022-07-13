# # #test file
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

options(dplyr.summarise.inform = FALSE)

##special test dataset to check for multiple entries per week
# d <- read_csv('test/simulated_jfs_data_concentrated.csv')
# d$DECISION_DATE <- as.Date(d$DECISION_DATE, format = "%m/%d/%Y")
# d$BIRTH_DATE <- as.Date(d$BIRTH_DATE, format = "%m/%d/%Y")
##

# consider 'SCREENED IN AR' same as 'SCREENED IN'
d <- d %>%
  mutate(screened_in = SCREENING_DECISION %in% c("SCREENED IN", "SCREENED IN AR"))


d <- d %>%
  filter(!duplicated(INTAKE_ID)) %>%
  select( -ADDRESS_START, -address_type) %>%
  mutate(month = lubridate::month(DECISION_DATE),
         year = lubridate::year(DECISION_DATE)) 

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
  summarize(num_year = n(), .groups = "keep")

print(knitr::kable(d_yearly))

##

### Monthly Counts
d_neigh <- d  %>%
  mutate(fips_tract_id = as.character(fips_tract_id)) %>% #comment these out when testing with concentrated data
  left_join(tract_to_neighborhood, by='fips_tract_id') %>%
  filter(!is.na(DECISION_DATE))

d_neigh <- d_neigh %>%
  mutate(neighborhood = ifelse(is.na(lat), 'Missing', neighborhood))

screen_neighborhood_age <- d_neigh %>%
  group_by(neighborhood, year, month, age_grp) %>%
  summarise(n_screened_in = sum(screened_in == TRUE, na.rm = TRUE),
            n_calls = n(),
            .groups = "keep"
            ) %>%
  ungroup() 

screen_neighborhood_age_2 <- screen_neighborhood_age %>% 
  group_by(year, month, age_grp) %>%
  summarise(neighborhood = "Total",
            n_calls = sum(n_calls),
            n_screened_in = sum(n_screened_in)) %>%
  bind_rows(screen_neighborhood_age, .)


screen_neighborhood_age_2 <- screen_neighborhood_age_2 %>%
  rename(group = "age_grp")


screen_neighborhood_MR <- d_neigh %>%
  group_by(neighborhood, year, month, MANDATED_REPORTER) %>%
  summarise(n_screened_in = sum(screened_in == TRUE, na.rm = TRUE),
            n_calls = n(),
            .groups = "keep"
  ) %>%
  ungroup() 

screen_neighborhood_MR_2 <- screen_neighborhood_MR %>% 
  group_by(year, month, MANDATED_REPORTER) %>%
  summarise(neighborhood = "Total",
            n_calls = sum(n_calls),
            n_screened_in = sum(n_screened_in)) %>%
  bind_rows(screen_neighborhood_MR, .)

screen_neighborhood_MR_2 <- screen_neighborhood_MR_2 %>%
  mutate(MANDATED_REPORTER = recode_factor(MANDATED_REPORTER, Yes = "MR", No = "Non_MR")) %>%
  rename(group = "MANDATED_REPORTER")

screen_neighborhood <- rbind(screen_neighborhood_age_2, screen_neighborhood_MR_2) %>%
  arrange(neighborhood, year, month)

screen_neighborhood_2 <- screen_neighborhood %>%
  group_by(neighborhood, year, month) %>%
  summarise(group = "Total",
            n_calls = sum(n_calls),
            n_screened_in = sum(n_screened_in)) %>%
  bind_rows(screen_neighborhood, .) %>%
  arrange(neighborhood, year, month)


###---- Produces wide data format
#   summarise(n_screened_in_all = sum(screened_in == TRUE, na.rm = TRUE),
#             n_calls_all = n(),
#             n_screened_in_less_5 = sum(screened_in == TRUE & age_grp == 'zero_five', na.rm = TRUE),
#             n_calls_less_5 = length(INTAKE_ID[age_grp == 'zero_five']),
#             n_screened_in_greater_5 = sum(screened_in == TRUE & age_grp == 'five_plus', na.rm = TRUE),
#             n_calls_greater_5 = length(INTAKE_ID[age_grp == 'five_plus']),
#             n_screened_in_no_bd = sum(screened_in == TRUE & age_grp == 'no_birth_date', na.rm = TRUE),
#             n_calls_no_bd = length(INTAKE_ID[age_grp == 'no_birth_date']),
#             n_screened_in_MR = sum(screened_in == TRUE & MANDATED_REPORTER == 'Yes', na.rm = TRUE),
#             n_calls_MR = length(INTAKE_ID[MANDATED_REPORTER == 'Yes']),
#             n_screened_in_NON_MR = sum(screened_in == TRUE & MANDATED_REPORTER == 'No', na.rm = TRUE),
#             n_calls_NON_MR = length(INTAKE_ID[MANDATED_REPORTER == 'No']),
#             .groups = "keep"
#             ) %>%
#   ungroup() %>% 
#   group_by(year, two_week) %>%
#   bind_rows(summarize(., across(where(is.numeric), sum),
#                       across(where(is.character), ~"Total"), .groups = "keep"))
#   
# 
# screen_neighborhood_rate <- screen_neighborhood %>%
#   group_by(neighborhood, year, two_week) %>%
#   mutate(screen_in_rate_all = round(n_screened_in_all/n_calls_all,2),
#          screen_in_rate_less_5 = round(n_screened_in_less_5/n_calls_less_5,2),
#          screen_in_rate_greater_5 = round(n_screened_in_greater_5/n_calls_greater_5,2),
#          screen_in_rate_no_bd = round(n_screened_in_no_bd/n_calls_no_bd,2),
#          screen_in_rate_MR = round(n_screened_in_MR/n_calls_MR,2),
#          screen_in_rate_NON_MR = round(n_screened_in_NON_MR/n_calls_NON_MR,2)
#          ) %>%
#   mutate(across(starts_with('n_'),
#             ~ifelse(.x < 5, NA, .x)))
##---

screen_neighborhood_rate <- screen_neighborhood_2 %>%
  rowwise() %>%
  mutate(screen_in_rate = round(n_screened_in / n_calls, 2)) %>%
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
write.csv(d_csv, paste(path, "monthly_report_v4.1.1.csv", sep = ''))


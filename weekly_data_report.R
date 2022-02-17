
# consider 'SCREENED IN AR' same as 'SCREENED IN'
d <- d %>%
  mutate(screened_in = SCREENING_DECISION %in% c("SCREENED IN", "SCREENED IN AR"))

d <- d %>%
  filter(!duplicated(INTAKE_ID)) %>%
  select( -ADDRESS_START, -address_type) %>%
  mutate(week = lubridate::week(DECISION_DATE),
         year = lubridate::year(DECISION_DATE))

### Weekly Counts
d_neigh <- d  %>%
  mutate(fips_tract_id = as.character(fips_tract_id)) %>%
  left_join(tract_to_neighborhood, by='fips_tract_id') %>%
  filter(!is.na(DECISION_DATE))

d_neigh <- d_neigh %>%
  mutate(neighborhood = ifelse(is.na(lat), 'Missing', neighborhood))

screen_neighborhood <- d_neigh %>%
  group_by(neighborhood, year, week) %>%
  summarize(n_screened_in = sum(screened_in == TRUE, na.rm = TRUE),
            n_calls = n(), .groups = "drop")  %>%
  mutate(screen_in_rate = round(n_screened_in/n_calls,2)) %>%
  mutate_at(vars(n_screened_in, n_calls),
            ~ifelse(.x < 5, NA, .x))

##

d_csv <- screen_neighborhood %>%
  select(neighborhood, year, week,
         number_of_calls = n_calls,
         number_of_calls_screened_in = n_screened_in,
         screen_in_rate = screen_in_rate)

path <- "/tmp/"
write.csv(d_csv, paste(path, "weekly_report_v4.0.1.csv", sep = ''))


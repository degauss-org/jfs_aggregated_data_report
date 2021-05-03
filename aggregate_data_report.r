#!/usr/local/bin/Rscript

setwd('/tmp')

suppressPackageStartupMessages(library(argparser))
p <- arg_parser('create JFS Neighborhood Report')
p <- add_argument(p,'file_name',help='name of geocoded csv file')
args <- parse_args(p)

# args <- list()
# args$file_name <- "simulated_jfs_data_geocoded.csv"

# INTAKE_ID
# SCREENING_DECISION = SCREENED OUT or SCREENED IN
# ALLEGATION_ADDRESS, formatted in a string without punctuation, it includes city, state, and zip code

message("\nNeighborhood results with < 5 instances have been censored for privacy purposes\n")

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(knitr))

d <- read_csv(args$file_name,
              col_types = cols(INTAKE_ID = col_character(),
                               SCREENING_DECISION = col_character(),
                               DECISION_DATE = col_character(),
                               PERSON_ID = col_character(),
                               RACE = col_character(),
                               ADDRESS_START = col_date(),
                               MANDATED_REPORTER = col_character(),
                              #  REPORTER_PERSON_ID = col_character(),
                               address_type = col_character(),
                               address = col_character(),
                               bad_address = col_logical(),
                               PO = col_logical(),
                               lat = col_double(),
                               lon = col_double(),
                               score = col_double(),
                               precision = col_character(),
                               precise_geocode = col_logical(),
                               fips_tract_id = col_character(),
                               fraction_assisted_income = col_double(),
                               fraction_high_school_edu = col_double(),
                               median_income = col_double(),
                               fraction_no_health_ins = col_double(),
                               fraction_poverty = col_double(),
                               fraction_vacant_housing = col_double(),
                               dep_index = col_double()
                               ))
d <- dplyr::mutate(d, DECISION_DATE = dht::check_dates(DECISION_DATE))

# rmarkdown::render(input = '/app/aggregate_data_report.rmd',
#                   params = list(d = d),
#                   envir = new.env())


tract_to_neighborhood <- readRDS('/app/tract_to_neighborhood.rds')
neighborhood_shp <- readRDS('/app/ham_neighborhoods_dep_index_shp.rds')

# Overall Summary
date_min <- min(d$DECISION_DATE)
date_max <- max(d$DECISION_DATE)

# consider 'SCREENED IN AR' same as 'SCREENED IN'
for (i in 1:nrow(d)) {
  if (d$SCREENING_DECISION[i] == 'SCREENED IN AR') {
    d$SCREENING_DECISION[i] <- 'SCREENED IN'
  }
}

d_missing_alleg_add <- d %>%
  filter(address_type == 'ALLEGATION_ADDRESS',
         is.na(address)) %>%
  select(PERSON_ID:address_type)

d_fill_in_address <- d %>%
  filter(PERSON_ID %in% d_missing_alleg_add$PERSON_ID,
         address_type == 'CHILD_ADDRESS') %>%
  group_by(PERSON_ID) %>%
  arrange(desc(ADDRESS_START)) %>%
  slice(1) %>%
  select(INTAKE_ID, address:dep_index)

d_missing_alleg_add <- left_join(d_missing_alleg_add, d_fill_in_address, by = 'PERSON_ID')

d <- d %>%
  filter(address_type == 'ALLEGATION_ADDRESS' & !is.na(address) |
           address_type == 'CHILD_ADDRESS') %>%
  bind_rows(d_missing_alleg_add) %>%
  filter(address_type == 'ALLEGATION_ADDRESS')

### Geocoding Summary
d_acv<- filter(d, !duplicated(PERSON_ID)) %>%
  select( -ADDRESS_START, -address_type)

d_intake <- filter(d, !duplicated(INTAKE_ID)) %>%
  select(-PERSON_ID, -RACE, -ADDRESS_START,
         -address_type)

# intake_geocode_summary <- d_intake %>%
#   group_by(is.na(lat)) %>%
#   tally() %>%
#   mutate(pct = round(n/sum(n)*100,1))
#
# acv_geocode_summary <- d_acv %>%
#   group_by(is.na(lat)) %>%
#   tally() %>%
#   mutate(pct = round(n/sum(n)*100,1))

### Race Summary

'%notin%'<-Negate('%in%')

d_acv_other <- d_acv %>%
  filter(RACE %notin% c("White", "Black/African American"))

d_acv <- d_acv %>%
  filter(RACE %in% c("White", "Black/African American")) #Only looking at black vs white

### Mandated Reporter Summary
d_mandated <- d %>%
  filter(MANDATED_REPORTER == 'Yes')

d_mandated_intakes <- unique(d_mandated$INTAKE_ID)

d_intake <- d_intake %>%
  mutate(at_least_one_mandated_reporter = ifelse(INTAKE_ID %in% d_mandated_intakes, 'yes', 'no'))

## Summaries
race_summary <- d_acv %>%
  filter(!is.na(lat)) %>%
  group_by(RACE, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  select(RACE, Decision = SCREENING_DECISION, n, pct)

mr_summary <- d_intake %>%
  filter(!is.na(lat)) %>%
  group_by(at_least_one_mandated_reporter, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  select(`At Least One Mandated Reporter` = at_least_one_mandated_reporter,
         Decision = SCREENING_DECISION, n, pct)


d_neigh_acv <- d_acv  %>%
  mutate(fips_tract_id = as.character(fips_tract_id)) %>%
  filter(!is.na(lat)) %>%
  left_join(tract_to_neighborhood, by='fips_tract_id')

acv_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood) %>%
  summarize(n_acvs = n())

acv_screen_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_acvs_screened_in = n) %>%
  select(-n, -SCREENING_DECISION) %>%
  left_join(acv_neighborhood, by = 'neighborhood')

##
white_acv_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood, RACE) %>%
  summarize(n_white_acvs = n()) %>%
  ungroup() %>%
  filter(RACE == 'White') %>%
  select(-RACE)

white_acv_screen_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood, RACE, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(RACE == 'White',
         SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_white_acvs_screened_in = n) %>%
  select(neighborhood, n_white_acvs_screened_in) %>%
  left_join(white_acv_neighborhood, by = 'neighborhood')

##

black_acv_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood, RACE) %>%
  summarize(n_black_acvs = n()) %>%
  ungroup() %>%
  filter(RACE == 'Black/African American') %>%
  select(-RACE)

black_acv_screen_neighborhood <- d_neigh_acv %>%
  group_by(neighborhood, RACE, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(RACE == 'Black/African American',
         SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_black_acvs_screened_in = n) %>%
  select(neighborhood, n_black_acvs_screened_in) %>%
  left_join(black_acv_neighborhood, by = 'neighborhood')

d_neighborhood_acv <- acv_screen_neighborhood %>%
  left_join(white_acv_screen_neighborhood, by = 'neighborhood') %>%
  left_join(black_acv_screen_neighborhood, by = 'neighborhood')

d_neighborhood_acv <- neighborhood_shp %>%
  left_join(d_neighborhood_acv, by = 'neighborhood') %>%
  mutate_at(vars(n_acvs, n_acvs_screened_in,
                 n_white_acvs,
                 n_white_acvs_screened_in,
                 n_black_acvs,
                 n_black_acvs_screened_in),
            ~ifelse(.x < 5, NA, .x)) %>%  # suppress if n < 5 for privacy reasons
  mutate(#n_acvs_per_1000 = n_acvs/pop_under_18*1000,
    acvs_fractional_screen_in = n_acvs_screened_in/n_acvs,
    # n_acvs_white_per_1000 = n_white_acvs/pop_under_18*1000,
    acvs_white_fractional_screen_in = n_white_acvs_screened_in/n_white_acvs,
    # n_acvs_black_per_1000 = n_black_acvs/pop_under_18*1000,
    acvs_black_fractional_screen_in = n_black_acvs_screened_in/n_black_acvs) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2))

##

d_neigh_intake <- d_intake  %>%
  mutate(fips_tract_id = as.character(fips_tract_id)) %>%
  filter(!is.na(lat)) %>%
  left_join(tract_to_neighborhood, by='fips_tract_id')

intakes_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood) %>%
  summarize(n_intakes = n())

intake_screen_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_intakes_screened_in = n) %>%
  select(-n, -SCREENING_DECISION) %>%
  left_join(intakes_neighborhood, by = 'neighborhood')

##

mr_intakes_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood, at_least_one_mandated_reporter) %>%
  summarize(n_mandated_reporter_intakes = n()) %>%
  ungroup() %>%
  filter(at_least_one_mandated_reporter == 'yes') %>%
  select(-at_least_one_mandated_reporter)

mr_intake_screen_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood, at_least_one_mandated_reporter, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(at_least_one_mandated_reporter == 'yes',
         SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_mandated_reporter_intakes_screened_in = n) %>%
  select(neighborhood, n_mandated_reporter_intakes_screened_in) %>%
  left_join(mr_intakes_neighborhood, by = 'neighborhood')

##

nmr_intakes_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood, at_least_one_mandated_reporter) %>%
  summarize(n_nonmandated_reporter_intakes = n()) %>%
  ungroup() %>%
  filter(at_least_one_mandated_reporter == 'no') %>%
  select(-at_least_one_mandated_reporter)

nmr_intake_screen_neighborhood <- d_neigh_intake %>%
  group_by(neighborhood, at_least_one_mandated_reporter, SCREENING_DECISION) %>%
  tally() %>%
  ungroup() %>%
  filter(at_least_one_mandated_reporter == 'no',
         SCREENING_DECISION == 'SCREENED IN') %>%
  mutate(n_nonmandated_reporter_intakes_screened_in = n) %>%
  select(neighborhood, n_nonmandated_reporter_intakes_screened_in) %>%
  left_join(nmr_intakes_neighborhood, by = 'neighborhood')

d_neighborhood_int <- intake_screen_neighborhood %>%
  left_join(mr_intake_screen_neighborhood, by = 'neighborhood') %>%
  left_join(nmr_intake_screen_neighborhood, by = 'neighborhood')

d_neighborhood_int <- neighborhood_shp %>%
  left_join(d_neighborhood_int, by = 'neighborhood') %>%
  mutate_at(vars(n_intakes, n_intakes_screened_in,
                 n_mandated_reporter_intakes,
                 n_mandated_reporter_intakes_screened_in,
                 n_nonmandated_reporter_intakes,
                 n_nonmandated_reporter_intakes_screened_in),
            ~ifelse(.x < 5, NA, .x)) %>%  # suppress if n < 5 for privacy reasons
  mutate(#n_intakes_per_1000hh = n_intakes/num_hh*1000,
    intake_fractional_screen_in = n_intakes_screened_in/n_intakes,
    # n_intakes_by_mandated_reporter_per_1000hh = n_mandated_reporter_intakes/num_hh*1000,
    intake_by_mandated_reporter_fractional_screen_in = n_mandated_reporter_intakes_screened_in/n_mandated_reporter_intakes,
    #  n_intakes_by_nonmandated_reporter_per_1000hh = n_nonmandated_reporter_intakes/num_hh*1000,
    intake_by_nonmandated_reporter_fractional_screen_in = n_nonmandated_reporter_intakes_screened_in/n_nonmandated_reporter_intakes) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2))

##

d_csv_acv <- d_neighborhood_acv %>%
  st_drop_geometry() %>%
  select(Neighborhood = neighborhood,
         `Number of White ACVs` = n_white_acvs,
         `White ACVs Screened In` = n_white_acvs_screened_in,
         `Fraction of White ACVs Screened In` = acvs_white_fractional_screen_in,
         `Number of Black/African American ACVs` = n_black_acvs,
         `Black/African American ACVs Screened In` = n_black_acvs_screened_in,
         `Fraction of Black/African American ACVs Screened In` = acvs_black_fractional_screen_in)

path <- "/tmp/"
write.csv(d_csv_acv, paste(path,"acv_level_report.csv", sep = ''))

d_csv_intake <- d_neighborhood_int %>%
  st_drop_geometry() %>%
  select(Neighborhood = neighborhood,
         `Number of Intakes` = n_intakes,
         `Fraction of Intakes Screened In` = intake_fractional_screen_in,
         `Number of Intakes by Mandated Reporter` = n_mandated_reporter_intakes,
         `Intakes by Mandated Reporter Screened In` = n_mandated_reporter_intakes_screened_in,
         `Fraction of Mandated Reporter Intakes Screened In` = intake_by_mandated_reporter_fractional_screen_in,
         `Number of Intakes by Non-Mandated Reporter` = n_nonmandated_reporter_intakes,
         `Intakes by Non-Mandated Reporter Screened In` = n_nonmandated_reporter_intakes_screened_in,
         `Fraction of Non-Mandated Reporter Intakes Screened In` = intake_by_nonmandated_reporter_fractional_screen_in)


write.csv(d_csv_intake, paste(path,"intake_level_report.csv", sep = ''))


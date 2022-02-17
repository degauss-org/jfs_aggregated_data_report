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

###
#d_test <- read.csv('test/simulated_jfs_data_geocoded_all_years.csv')
#d <- d_test
###
d <- dplyr::mutate(d, DECISION_DATE = dht::check_dates(DECISION_DATE))

tract_to_neighborhood <- readRDS('/app/tract_to_neighborhood.rds')
neighborhood_shp <- readRDS('/app/ham_neighborhoods_dep_index_shp.rds')

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
  select(`neighborhood` = neighborhood,
         `year` = year,
         `week` = week,
         `number_of_calls` = n_calls,
         `number_of_calls_screened_in` = n_screened_in,
         `screen_in_rate` = screen_in_rate)

path <- "/tmp/"
write.csv(d_csv, paste(path,"weekly_report.csv", sep = ''))


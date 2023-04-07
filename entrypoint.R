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

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyverse))
d <- read_csv(args$file_name,
              col_types = cols(INTAKE_ID = col_character(),
                               SCREENING_DECISION = col_character(),
                               DECISION_DATE = col_date(format = "%m/%d/%Y"),
                               BIRTH_DATE = col_date(format = "%m/%d/%Y"),
                               PERSON_ID = col_character(),
                               RACE = col_character(),
                               ADDRESS_START = col_date(),
                               REPORTER_PERSON_ID = col_character(),
                               MANDATED_REPORTER = col_character(),
                               address_type = col_character(),
                               address = col_character(),
                               bad_address = col_logical(),
                               PO = col_logical(),
                               matched_street = col_character(),
                               matched_zip = col_character(),
                               matched_city = col_character(),
                               matched_state = col_character(),
                               lat = col_double(),
                               lon = col_double(),
                               score = col_double(),
                               precision = col_character(),
                               geocode_result = col_character(),
                               census_block_group_id_2010 = col_character(),
                               census_tract_id_2010 = col_character()
              ))
                            


# d <- d %>%
#   mutate(DECISION_DATE = lubridate::parse_date_time(DECISION_DATE, c("%Y-%m-%d", "%m/%d/%Y")))

#d <- dplyr::mutate(d, DECISION_DATE = dht::check_dates(DECISION_DATE))

tract_to_neighborhood <- readRDS('/app/tract_to_neighborhood.rds')

message("\nNeighborhood repsonses with < 5 instances have been censored for privacy purposes\n")

source("/app/monthly_data_report.R")
# rmarkdown::render(input = '/app/weekly_data_report.R',
#                   params = list(d = d),
#                   envir = new.env())

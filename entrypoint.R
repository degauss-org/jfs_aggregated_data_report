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

rmarkdown::render(input = '/app/acv_level_report.rmd',
                  params = list(d = d),
                  envir = new.env(),
                  output_file = fs::path("/tmp", paste0(gsub('.csv', '', args$file_name, fixed=TRUE), '_acv_level_report.html')))

rmarkdown::render(input = '/app/intake_level_report.rmd',
                  params = list(d = d),
                  envir = new.env(),
                  output_file = fs::path("/tmp", paste0(gsub('.csv', '', args$file_name, fixed=TRUE), '_intake_level_report.html')))


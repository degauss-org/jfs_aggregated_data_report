# jfs_aggregated_data_report <a href='https://degauss-org.github.io/DeGAUSS/'><img src='DeGAUSS_hex.png' align="right" height="138.5" /></a>

> DeGAUSS container that generates one CSV file of aggregated, monthly data for JFS from a geocoded CSV file. Designed to be used in the pipeline described on [jfs_data](https://github.com/degauss-org/jfs_data)

[![Docker Build Status](https://img.shields.io/docker/automated/degauss/jfs_aggregated_data_report)](https://hub.docker.com/repository/docker/degauss/jfs_aggregated_data_report/tags)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/degauss-org/jfs_aggregated_data_report)](https://github.com/degauss-org/jfs_aggregated_data_report/releases)

## Dataset Notes

- Screening status `SCREENED IN AR` included with `SCREENED IN`
- Intakes that either have no listed address or were unsuccessfully geocoded will be included as a "neighborhood" called `Missing`

## geomarker data

- census tract-level [deprivation index](https://geomarker.io/dep_index/) from 2015 ACS measures
- census tract-level population under age 18 from 2018 ACS
- `00_create_tract_to_neighborhood.R` (based on code from Stu) makes `tract_to_neighborhood.rds` which is used to convert tracts to neighborhoods
- `00_create_tract_to_neighborhood.R` also aggregates tract-level deprivation index (mean) and tract-level population under 18 (sum) to neighborhood and creates the neighborhood shapefile called `ham_neighborhoods_dep_index_shp.rds`
- Neighborhood level fields with values less than 5 have been censored for privacy considerations

## DeGAUSS details

For detailed documentation on DeGAUSS, including general usage and installation, please see the [DeGAUSS README](https://degauss.org/).

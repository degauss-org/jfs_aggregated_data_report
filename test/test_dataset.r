d <- read_csv('simulated_jfs_data_geocoded_all_years.csv')

## Increase sample test size to better see censoring 
d <- d %>%
  bind_rows(replicate(5, d, simplify = F)) %>%
  mutate(INTAKE_ID = sample.int(1200, size = nrow(.), replace = F))

set.seed(1066)
d <- d %>%
 mutate(DECISION_DATE = sample(seq(as.Date('2017-01-01'), as.Date('2021-12-31'), by = "day"), 
                               nrow(.), replace = T)) %>%
  mutate(DECISION_DATE = format(DECISION_DATE, "%m/%d/%Y"))

write_csv(d, 'simulated_jfs_data_geocoded_all_years_bigger.csv')

#Add in birth dates
d2 <- read_csv('simulated_jfs_data_geocoded_all_years_bigger.csv')

d2 <- d2 %>%
  mutate(BIRTH_DATE = sample(seq(as.Date('2003-01-01'), as.Date('2021-12-31'), by = "day"), 
                             nrow(.), replace = T)) %>%
  mutate(BIRTH_DATE = format(BIRTH_DATE, "%m/%d/%Y"))

#add in a few NA birth dates
addNAs <- sample(1:1200, 100, replace = F)
d2$BIRTH_DATE[addNAs] <- NA

d3 <- d2 %>%
  mutate(census_tract_id_2010 = fips_tract_id,
         census_block_group_id_2010 = ifelse(is.na(fips_tract_id), NA, paste0(fips_tract_id, '0001'))) %>%
  select(-c(fips_tract_id, fraction_assisted_income, fraction_high_school_edu, median_income, 
            fraction_poverty, fraction_vacant_housing, dep_index))

write_csv(d3, 'simulated_jfs_data_geocoded_all_years_bigger_3.csv')

#special set for testing age splits
d4 <- read_csv('simulated_jfs_data_geocoded_all_years_bigger_2.csv')

tract_to_neighborhood <- read_rds('tract_to_neighborhood.rds')
tract_to_neighborhood$fips_tract_id <- as.double(tract_to_neighborhood$fips_tract_id)

d3 <- d3 %>%
  left_join(tract_to_neighborhood, by = 'fips_tract_id')

d3 <- d3 %>%
  filter(neighborhood == "Avondale") %>%
  mutate(year = lubridate::year(as.Date(DECISION_DATE, format = "%m/%d/%Y"))) %>%
  filter(year == 2017)

d3_all <- d3 %>%
  bind_rows(replicate(50, d3, simplify = F)) %>%
  mutate(BIRTH_DATE = sample(seq(as.Date('2003-01-01'), as.Date('2016-12-31'), by = "day"), 
                             nrow(.), replace = T),
         DECISION_DATE = sample(seq(as.Date('2017-01-01'), as.Date('2017-1-31'), by = "day"), nrow(.), replace = T),
         INTAKE_ID = sample.int(255, size = nrow(.), replace = F)) %>%
  mutate(BIRTH_DATE = format(BIRTH_DATE, "%m/%d/%Y"))

addNAs <- sample(1:255, 25, replace = F)
d3_all$BIRTH_DATE[addNAs] <- NA

d3_all %>%
  group_by(DECISION_DATE) %>%
  tally()

write_csv(d3_all, 'simulated_jfs_data_concentrated.csv')  
  
  
  

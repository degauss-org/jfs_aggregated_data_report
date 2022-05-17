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

write_csv(d2, 'simulated_jfs_data_geocoded_all_years_bigger_2.csv')

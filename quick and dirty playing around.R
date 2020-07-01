
# load libraries
library(tidyr)
library(RPostgreSQL)
library(dbplyr)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(tidycensus)
library(covdata)



#### PREP DATA ####
# We'll load multiple data sources 
# 1) clinical trial data from Cytel's clinical trial tracker, which is a nicely cleaned up list of all COVID19 trials derived from clinicaltrial.gov (and other sources for international trials)
# 2) zip codes of trials from AACT, which is the gold standard for cleaned up data from clinicaltrials.gov; doesn't have some of the more nuanced fields that Cytel's tracker does, but it includes zip codes of all trial location
# 3) Census-based demographic information
# 4) county-level case counts 
# 5) hospital data (individual aggregated to county-level)


# load Cytel tracker data (downloaded from their website: )
trials <- read.csv("table_trials - 2020-06-17 22_22_12.csv") %>%
  filter(Country == "United States") %>%
  rename(nct_id = ID)

### AACT: oh how I've missed SQL databases
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, 
                 dbname="aact",
                 host="aact-db.ctti-clinicaltrials.org", 
                 port=5432, 
                 user="cb11", 
                 password="idmuse")

# details of studies
study_tbl <- tbl(src=con,'studies')

# get locations
location_tbl <- tbl(src=con,'facilities')
locations <- location_tbl %>% 
  filter(country == "United States") %>%
  select(nct_id, zip) %>%
  collect()

# locations for covid trials only
trial_locations <- trials %>% 
  left_join(locations, by = "nct_id") %>%
  filter(!is.na(zip)) 

# load census data 
source("census_data.R")

# get urban designation (also from census, but outside tidycensus)
urban <- read.csv("PctUrbanRural_County.csv", header = TRUE) %>%
  mutate(geoid = as.double(paste0(ï..STATE, COUNTY))) %>%
  select(geoid, POPPCT_URBAN)

# load hosptial data
hospitals <- read.csv("Hospitals.csv") %>%
  mutate(BEDS = ifelse(BEDS == -999, NA, BEDS)) %>%
  group_by(COUNTYFIPS) %>%
  summarise(hospitals = n(),
            hospital_beds = sum(BEDS)) %>%
  mutate(geoid = as.double(COUNTYFIPS))

# count trials per zip
# need to group by county; zips aren't as relevant here (plus we're merging with case data)
one_to_one_area = zcta::zcta_county_rel_10 %>% 
  select(zcta5, state, state, county, geoid, poppt, zpoppct, zareapct) %>% 
  group_by(zcta5) %>% 
  slice(which.max(zareapct))


trial_counts <- trial_locations %>%
  left_join(one_to_one_area %>% select(zcta5, geoid), by = c("zip" = "zcta5")) %>%
  distinct(nct_id, geoid, .keep_all = TRUE)
  
cases <- nytcovcounty %>%
  mutate(fips = ifelse(county == "New York City", "36061", fips)) 

### MARCH ###
trial_counts_march <- trial_counts %>%
  filter(as.Date(paste0(Date, "-01"), "%Y-%B-%d") < '2020-03-31' & as.Date(paste0(Completion, "-01"), "%Y-%B-%d") > '2020-03-31') %>%
  group_by(geoid) %>%
  summarize(total_march = n(),
            total_pts_march = sum(Size)) 

cases_march <- cases %>%
  group_by(fips) %>%  
  filter(date == "2020-03-31") %>%
  mutate(cases_march = max(cases) ) %>%
  mutate(geoid = fips) %>%
  ungroup() %>%
  select(geoid, cases_march)
  
### APRIL ###
trial_counts_april <- trial_counts %>%
  filter(as.Date(paste0(Date, "-01"), "%Y-%B-%d") < '2020-04-30' & as.Date(paste0(Completion, "-01"), "%Y-%B-%d") > '2020-04-30') %>%
  group_by(geoid) %>%
  summarize(total_april = n(),
            total_pts_april = sum(Size)) 

cases_april <- cases %>%
  group_by(fips) %>%  
  filter(date == "2020-04-30" | date == "2020-03-31") %>%
  mutate(cases_april = max(cases) - min(cases)) %>%
  mutate(geoid = fips) %>%
  ungroup() %>%
  select(geoid, cases_april)

### MAY ###
trial_counts_may <- trial_counts %>%
  filter(as.Date(paste0(Date, "-01"), "%Y-%B-%d") < '2020-05-31' & as.Date(paste0(Completion, "-01"), "%Y-%B-%d") > '2020-05-31') %>%
  group_by(geoid) %>%
  summarize(total_may = n(),
            total_pts_may = sum(Size)) 

cases_may <- cases %>%
  group_by(fips) %>%  
  filter(date == "2020-05-31" | date == "2020-04-30") %>%
  mutate(cases_may = max(cases) - min(cases)) %>%
  mutate(geoid = fips) %>%
  ungroup() %>%
  select(geoid, cases_may)

### JUNE ###
trial_counts_june <- trial_counts %>%
  filter(as.Date(paste0(Date, "-01"), "%Y-%B-%d") < '2020-06-30' & as.Date(paste0(Completion, "-01"), "%Y-%B-%d") > '2020-06-30') %>%
  group_by(geoid) %>%
  summarize(total_june = n(),
            total_pts_june = sum(Size)) 

cases_june <- cases %>%
  group_by(fips) %>%  
  filter(date == "2020-06-28" | date == "2020-05-31") %>%
  mutate(cases_june = max(cases) - min(cases)) %>%
  mutate(geoid = fips) %>%
  ungroup() %>%
  select(geoid, cases_june)

trials <- cases_march %>%
  left_join(cases_april, by = "geoid") %>%
  left_join(cases_may, by = "geoid") %>%
  left_join(cases_june, by = "geoid") %>%
  mutate(geoid = as.double(geoid)) %>%
  left_join(trial_counts_march, by = "geoid") %>%
  left_join(trial_counts_april, by = "geoid") %>%
  left_join(trial_counts_may, by = "geoid") %>%
  left_join(trial_counts_june, by = "geoid")

trials_census <- census_clean %>%
  mutate(geoid = as.double(GEOID)) %>%
  left_join(urban, by = "geoid") %>%
  left_join(hospitals, by = "geoid") %>%
  left_join(trials, by = "geoid") %>%
  mutate(total_march = ifelse(is.na(total_march), 0, total_march),
         total_april = ifelse(is.na(total_april), 0, total_april),
         total_may = ifelse(is.na(total_may), 0, total_may),
         total_june = ifelse(is.na(total_june), 0, total_june),
         hospitals = ifelse(is.na(hospitals), 0, hospitals),
         hospital_beds = ifelse(is.na(hospital_beds), 0, hospital_beds),
         cases_march = ifelse(is.na(cases_march), 0, cases_march),
         cases_april = ifelse(is.na(cases_april), 0, cases_april),
         cases_may = ifelse(is.na(cases_may), 0, cases_may),
         cases_june = ifelse(is.na(cases_june), 0, cases_june),
         cases = cases_march + cases_april + cases_may + cases_june,
         total = total_march+ total_april + total_may + total_june) %>%
  distinct()

#### LOOK AT DATA ####








#### 
p <- ggplot() +
  geom_smooth(trials_census %>% filter(cases_march < 70000), mapping = aes(cases_march, total_march, weight = population)) +
  geom_point(trials_census %>% filter(cases_march < 70000), mapping = aes(cases_march, total_march, size = population), color = "red", alpha = 0.3) +
  geom_smooth(trials_census %>% filter(cases_april < 70000), mapping = aes(cases_april, total_april, weight = population)) +
  geom_point(trials_census %>% filter(cases_april < 70000), mapping = aes(cases_april, total_april, size = population), color = "blue", alpha = 0.3) +
  geom_smooth(trials_census %>% filter(cases_may < 70000), mapping = aes(cases_may, total_may, weight = population)) +
  geom_point(trials_census %>% filter(cases_may < 70000), mapping = aes(cases_may, total_may, size = population), color = "black", alpha = 0.3) +
  theme_minimal()

p


# some exploratory plots; let's see what we're working with here
p <- ggplot(trials_census %>% filter(cases <50000),  aes(cases, total, size = population)) +
  geom_smooth() +
  geom_jitter(aes(color = black_aa), alpha = 0.5) +
  ggtitle("Total number of COVID trials open in a county vs number of COVID cases") +
  ylab("") +
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal()
p

# for predominanting black communities?
p <- ggplot() +
  geom_smooth(trials_census %>% filter(hispanic >= 10 & cases <50000),  
              mapping = aes(cases, total), color = "blue") +
  geom_smooth(trials_census %>% filter(hispanic < 10 & cases <50000), 
              mapping =  aes(cases, total), color = "red") 
p

# how many counties have/don't have even a single trial?

table <- trials_census %>%
  summarize(cases0_trials0 = sum(cases == 0 & total == 0),
            cases1_trials0 = sum(cases >= 1 & total == 0),
            cases10_trials0 = sum(cases >= 10 & total == 0),
            cases100_trials0 = sum(cases >= 100 & total == 0),
            cases1000_trials0 = sum(cases >= 1000 & total == 0),
            cases10000_trials0 = sum(cases >= 10000 & total == 0),
            cases0 = sum(cases == 0),
            cases1 = sum(cases >= 1),
            cases10 = sum(cases >= 10 ),
            cases100 = sum(cases >= 100),
            cases1000 = sum(cases >= 1000),
            cases10000 = sum(cases >= 10000))
            
# population living in counties with at least one case and no trials:
no_trial <- trials_census %>%
  filter(cases >= 1 & total == 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())

# population living in counties with at least one case and at least one trial:
yes_trial <- trials_census %>%
  filter(cases >= 1 & total > 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())

# population living in counties with at least 100 cases and no trials:
no_trial <- trials_census %>%
  filter(cases >= 100 & total == 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())

# population living in counties with at least 100 cases and at least one trial:
yes_trial <- trials_census %>%
  filter(cases >= 100 & total > 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())


# population living in counties with at least 1000 cases and no trials:
no_trial <- trials_census %>%
  filter(cases >= 1000 & total == 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())

# population living in counties with at least 1000 cases and at least one trial:
yes_trial <- trials_census %>%
  filter(cases >= 1000 & total > 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())


# population living in counties with at least 10k cases and no trials:
no_trial <- trials_census %>%
  filter(cases >= 10000 & total == 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())

# population living in counties with at least 10k cases and at least one trial:
yes_trial <- trials_census %>%
  filter(cases >= 10000 & total > 0) %>%
  summarise(mean_pop = weighted.mean(population)*n())


summary(glm(total_pts ~ 1 + 
                  black_aa + 
                  hispanic + 
                  asian +
                  POPPCT_URBAN +
                  population , 
             family = "poisson", 
            data = trials_census))


april <- summary(glm(total ~ 1 + 
              POPPCT_URBAN + 
              age75_plus +
              log(hospital_beds + 0.001) +
              log(cases + 0.001) + 
              black_aa + 
              hispanic +
              asian +
              poverty +
              offset(log(population)), 
            family = "poisson", 
            data = trials_census))

summary(glm(total ~ 1 + 
              POPPCT_URBAN +
              cases +
              black_aa + 
              hispanic +
              white_race +
              asian +
              poverty + 
              insurance +
              offset(log(population)), 
            family = "poisson", 
            data = trials_census, weights = population))

summary(glm(total > 0 ~ 1 + 
              black_aa + 
              cases +
              hispanic +
              asian, 
            family = "binomial", 
            data = trials_census,
            weights = population))






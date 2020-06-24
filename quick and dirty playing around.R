
# load libraries
library(tidyr)
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(cowplot)
library(tidycensus)
library(covdata)

# We'll load 3 data sources 
# 1) clinical trial data from Cytel's clinical trial tracker, which is a nicely cleaned up list of all COVID19 trials derived from clinicaltrial.gov (and other sources for international trials)
# 2) zip codes of trials from AACT, which is the gold standard for cleaned up data from clinicaltrials.gov; doesn't have some of the more nuanced fields that Cytel's tracker does, but it includes zip codes of all trial location
# 3) Census-based data on zip-code tabulation level demographics
# 4) Other zip code level case counts (or only county?)


# load Cytel tracker data (downloaded from their website: )
trials <- read.csv("table_trials - 2020-06-17 22_22_12.csv") %>%
  filter(Country == "United States" & Trial.status == "Recruiting") %>%
  rename(nct_id = ID)

### AACT: oh how I've missed SQL databases
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, 
                 dbname="aact",
                 host="aact-db.ctti-clinicaltrials.org", 
                 port=5432, 
                 user="cb11", 
                 password="idmuse")

# details of studies; won't load locally though
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

# get urban designation

urban <- read.csv("PctUrbanRural_County.csv", header = TRUE) %>%
  mutate(geoid = as.double(paste0(ï..STATE, COUNTY))) %>%
  select(geoid, POPPCT_URBAN)

# count trials per zip
# need to group by county; zips aren't as relevant here (plus we're merging with case data)
one_to_one_area = zcta::zcta_county_rel_10 %>% 
  select(zcta5, state, state, county, geoid, poppt, zpoppct, zareapct) %>% 
  group_by(zcta5) %>% 
  slice(which.max(zareapct))

trial_counts <- trial_locations %>%
  left_join(one_to_one_area %>% select(zcta5, geoid), by = c("zip" = "zcta5")) %>%
  distinct(nct_id, geoid, .keep_all = TRUE) %>%
  group_by(geoid) %>%
  summarize(total = n(),
            total_pts = sum(Size)) 

cases <- nytcovcounty %>%
  #filter(date == "2020-06-15" | date == "2020-06-14") %>%
  group_by(fips) %>%
  mutate(new_cases = max(cases) ) %>%
  filter(date == "2020-06-15")
  
trials <- trial_counts %>%
  left_join(cases %>% 
              mutate(geoid = as.double(fips)), by = "geoid")

trials_census <- census_clean %>%
  mutate(geoid = as.double(GEOID)) %>%
  left_join(urban, by = "geoid") %>%
  left_join(trials, by = "geoid") %>%
  mutate(total = ifelse(is.na(total), 0, total),
         total_pts = ifelse(is.na(total_pts), 0, total_pts),
         cases = ifelse(is.na(cases), 0, cases),
         total_bin = ifelse(total >=1, 1, 0)) 

# some exploratory plots; let's see what we're working with here
p <- ggplot(trials_census,  aes(cases, total, size = population)) +
  geom_smooth() +
  geom_jitter(aes(color = black_aa), alpha = 0.5) +
  ggtitle("Total number of COVID trials open in a county vs number of COVID cases") +
  ylab("") +
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal()
p

# for predominanting black communities?
p <- ggplot() +
  geom_smooth(trials_census %>% filter(POPPCT_URBAN >= 50),  
              mapping = aes(cases, total), color = "blue") +
  geom_smooth(trials_census %>% filter(POPPCT_URBAN < 50), 
              mapping =  aes(cases, total), color = "red") 
p

# 

summary(glm(total ~ 1 + 
                  black_aa + 
                  hispanic + 
                  asian +
                  population , 
             family = "poisson", 
            data = trials_census))


summary(glm(total ~ 1 + 
              POPPCT_URBAN +
              medianincome +
              insurance +
              black_aa + 
              hispanic +
              asian +
              offset(log(population)), 
            family = "poisson", 
            data = trials_census))

summary(glm(total > 0 ~ 1 + 
              black_aa + 
              cases +
              hispanic +
              asian, 
            family = "binomial", 
            data = trials_census,
            weights = population))






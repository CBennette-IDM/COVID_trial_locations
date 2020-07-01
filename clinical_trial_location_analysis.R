
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
library(DescTools)
library(zipzcta)
library(zcta)
library(sf)
library(maptools)
library(sp)
library(spdep)
library(INLA)
library(kableExtra)


#### PREP DATA ####

# Load multiple data sources 
# 1) clinical trial data from Cytel's clinical trial tracker, which is a nicely cleaned up list of all COVID19 trials derived from clinicaltrial.gov (and other sources for international trials)
# 2) zip codes of trials from AACT, which is the gold standard for cleaned up data from clinicaltrials.gov; doesn't have some of the more nuanced fields that Cytel's tracker does, but it includes zip codes of all trial location
# 3) Census-based demographic information
# 4) county-level case counts from NYTimes (via covdata package)
# 5) hospital data (individual aggregated to county-level) from Medicare/CMS
# 6) universities (https://hifld-geoplatform.opendata.arcgis.com/datasets/colleges-and-universities)


# load Cytel tracker data (downloaded from their website: )
trials <- read.csv("table_trials - 2020-06-30.csv") %>%
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
  filter(!is.na(zip)) %>%
  mutate(zip = ifelse(nchar(zip) == 10, substr(zip, 1, 5), zip))

# load census data 
source("census_data.R")

# get urban designation (also from census, but outside tidycensus)
urban <- read.csv("PctUrbanRural_County.csv", header = TRUE) %>%
  mutate(geoid = as.double(paste0(ï..STATE, COUNTY))) %>%
  select(geoid, POPPCT_URBAN)

# load hospital data
hospitals <- read.csv("Hospitals.csv") %>%
  mutate(BEDS = ifelse(BEDS == -999, NA, BEDS)) %>%
  group_by(COUNTYFIPS) %>%
  summarise(hospitals = n(),
            hospital_beds = sum(BEDS)) %>%
  mutate(geoid = as.double(COUNTYFIPS))

# load university data
universities <- read.csv("Colleges_and_Universities.csv") %>%
  mutate(TOT_ENROLL = ifelse(TOT_ENROLL == -999, NA, TOT_ENROLL)) %>%
  filter(NAICS_DESC == "COLLEGES, UNIVERSITIES, AND PROFESSIONAL SCHOOLS") %>%
  filter(CLOSE_DATE == "-2") %>%
  group_by(COUNTYFIPS) %>%
  summarise(universities = n(),
            enrollment = sum(TOT_ENROLL)) %>%
  mutate(geoid = as.double(COUNTYFIPS))

# count trials per zip
# need to group by county; zips aren't as relevant here (plus we're merging with case data)
one_to_one_area = zcta::zcta_county_rel_10 %>% 
  select(zcta5, state, state, county, geoid, poppt, zpoppct, zareapct) %>% 
  group_by(zcta5) %>% 
  slice(which.max(zareapct))

#### note there are a handful of zip codes that don't match via this program; have manually reviewed these:
missing_zip <- read.csv("missing_zip.csv", as.is = TRUE) %>%
  mutate(geoid = as.double(geoid))

one_to_one_area <- one_to_one_area %>% 
  select(zcta5, geoid) %>%
  rbind(missing_zip)

trial_counts <- trial_locations %>%
  left_join(one_to_one_area %>% select(zcta5, geoid), by = c("zip" = "zcta5")) %>%
  distinct(nct_id, geoid, .keep_all = TRUE)

cases <- nytcovcounty %>%
  mutate(fips = ifelse(county == "New York City", "36061", fips)) 

### monthly estimates ###
trial_counts_month <- trial_counts %>%
  mutate(start = as.Date(paste0(Date, "-01"), "%Y-%B-%d"),
           end = as.Date(paste0(Completion, "-01"), "%Y-%B-%d"),
           march = start < '2020-03-31' & end >= '2020-03-01',
           april = start < '2020-04-30' & end >= '2020-04-01',
           may = start < '2020-05-31' & end >= '2020-05-01',
           june = start < '2020-06-30' & end >= '2020-06-01') %>%
  group_by(geoid) %>%
  mutate(total_march = sum(march, na.rm = T),
         total_april = sum(april, na.rm = T),
         total_may = sum(may, na.rm = T),
         total_june = sum(june, na.rm = T)) %>%
  select(geoid, total_march, total_april, total_may, total_june)

trial_new_month <- trial_counts %>%
  mutate(month = month(as.Date(paste0(Date, "-01"), "%Y-%B-%d"))) %>%
  group_by(geoid, month) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = month, values_from = total) %>%
  mutate(launched_march = ifelse(is.na(`3`), 0, `3`),
         launched_april = ifelse(is.na(`4`), 0, `4`),
         launched_may = ifelse(is.na(`5`), 0, `5`),
         launched_june = ifelse(is.na(`6`), 0, `6`)) %>%
  select(geoid, launched_march, launched_april, launched_may, launched_june)

cases_month <- cases %>%
  mutate(month = month(date)) %>%
  group_by(fips, month) %>%  
  summarise(cases_month = max(cases)) %>%
  pivot_wider(names_from = month, values_from = cases_month) %>%
  mutate(cases_march = ifelse(is.na(`3`), 0, `3`),
         cases_april = ifelse(is.na(`4`), 0, `4`) - cases_march, # want incremental case counts
         cases_may = abs(ifelse(is.na(`5`), 0, `5`) - cases_april),
         cases_june = abs(ifelse(is.na(`6`), 0, `6`) - cases_may)) %>%
  select(fips, cases_march, cases_april, cases_may, cases_june) %>%
  mutate(geoid = fips)

trials <- cases_month %>%
  mutate(geoid = as.double(geoid)) %>%
  left_join(trial_counts_month, by = "geoid") %>%
  left_join(trial_new_month, by = "geoid")

trials_census <- census_clean %>%
  mutate(geoid = as.double(GEOID)) %>%
  left_join(urban, by = "geoid") %>%
  left_join(hospitals, by = "geoid") %>%
  left_join(universities, by = "geoid") %>%
  left_join(trials, by = "geoid") %>%
  mutate(total_march = ifelse(is.na(total_march), 0, total_march),
         total_april = ifelse(is.na(total_april), 0, total_april),
         total_may = ifelse(is.na(total_may), 0, total_may),
         total_june = ifelse(is.na(total_june), 0, total_june),
         launched_march = ifelse(is.na(launched_march), 0, launched_march),
         launched_april = ifelse(is.na(launched_april), 0, launched_april),
         launched_may = ifelse(is.na(launched_may), 0, launched_may),
         launched_june = ifelse(is.na(launched_june), 0, launched_june),
         hospitals = ifelse(is.na(hospitals), 0, hospitals),
         hospital_beds = ifelse(is.na(hospital_beds), 0, hospital_beds),
         universities = ifelse(is.na(universities), 0, universities),
         cases_march = ifelse(is.na(cases_march), 0, cases_march),
         cases_april = ifelse(is.na(cases_april), 0, cases_april),
         cases_may = ifelse(is.na(cases_may), 0, cases_may),
         cases_june = ifelse(is.na(cases_june), 0, cases_june),
         cases = cases_march + cases_april + cases_may + cases_june,
         total = total_march+ total_april + total_may + total_june) %>%
  distinct() %>%
  mutate(state = substring(GEOID, 1, 2)) %>%
  filter(state != 72) # remove puerto rico

#### LOOK AT DATA ####

## number of counties, number of individuals, and number of cases:
CrossTab <- function(data, case, trial){
  case_no_trial <- data %>%
    filter({{case}} > 0 & {{trial}} == 0) %>%
    summarise(name = "Cases + no trials",
              county = n(),
              cases = mean({{case}})*n(),
              pop = mean(population)*n())
  case_trial <- data %>%
    filter({{case}} > 0 & {{trial}} > 0) %>%
    summarise(name = "Cases + trials",
              county = n(),
              cases = mean({{case}})*n(),
              pop = mean(population)*n())
  
  
  no_case_trial <- data %>%
    filter({{case}} == 0 & {{trial}} > 0) %>%
    summarise(name = "No cases + trials",
              county = n(),
              cases = mean({{case}})*n(), #NA
              pop = mean(population)*n()) #NA
  
  no_case_no_trial <- data %>%
    filter({{case}} == 0 & {{trial}} == 0) %>%
    summarise(name = "No cases + no trials",
              county = n(),
              cases = mean({{case}})*n(), #NA
              pop = mean(population)*n()) 

  cross_tab <- rbind(case_no_trial, 
                   case_trial, 
                   no_case_trial, 
                   no_case_no_trial)
  
  return(cross_tab)
}

CrossTab(trials_census, cases, total)

### also want to see the cross tab table by month:
CrossTab(trials_census, cases_march, total_march)
CrossTab(trials_census, cases_april, total_april)
CrossTab(trials_census, cases_may, total_may)
CrossTab(trials_census, cases_june, total_june)

# Gini coefficients
gini <- rbind(c("march", Gini(trials_census$total_march, trials_census$cases_march)),
              c("april", Gini(trials_census$total_april, trials_census$cases_april)),
              c("may", Gini(trials_census$total_may, trials_census$cases_may)),
              c("june", Gini(trials_census$total_june, trials_census$cases_june)))
gini

# look at persistence over time; just trials launched by month (taking out trials that remained open for months)
gini <- rbind(c("march", Gini(trials_census$launched_march, trials_census$cases_march)),
              c("april", Gini(trials_census$launched_april, trials_census$cases_april)),
              c("may", Gini(trials_census$launched_may, trials_census$cases_may)),
              c("june", Gini(trials_census$launched_june, trials_census$cases_june)))
gini


ggplot(trials_census, aes(total_march, total_june)) +
  geom_jitter(alpha = 0.1, color = "blue") + 
  theme_minimal()

ggplot(trials_census, aes(launched_march, launched_june)) +
  geom_jitter(alpha = 0.1, color = "blue") + 
  theme_minimal()

# numbers for early abstract


#### VERY SIMPLE MODELS ####
first_date <- cases %>%
  group_by(fips) %>%  
  summarise(date = min(date),
            days = today() - date)

trials_model <- trials_census %>%
  left_join(first_date, by = c("GEOID" = "fips"))

model <- summary(glm(total ~ 1 + 
                       POPPCT_URBAN + 
                       log(hospital_beds + 1) +
                       log(universities + 1) + 
                       log(cases+1) + 
                       black_aa +
                       hs_education +
                       college_education + 
                       hispanic +
                       medianincome +
                       poverty +
                       days +
                       offset(log(population)), 
                     family = "poisson", 
                     data = trials_model))
model

model <- summary(glmer(total ~ 1 + 
                       POPPCT_URBAN + 
                       log(hospital_beds + 1) +
                       log(universities + 1) + 
                       log(cases+1) + 
                       black_aa +
                       hs_education +
                       college_education + 
                       hispanic +
                       medianincome +
                       poverty +
                       days +
                       (1|state) +
                       offset(log(population)), 
                     family = "poisson", 
                     data = trials_model))
model

#### INLA MODELS ####
census_sf <- raw_census %>%
  group_by(GEOID) %>%
  slice(1) %>%
  ungroup() %>% # much faster than distinct
  rename(fips = GEOID) %>%
  select(fips, geometry) %>%
  inner_join(trials_sf, by = "fips")

# create contiguity based matrix
coords <- st_coordinates(st_centroid(st_geometry(census_sf)))  
plot(st_geometry(census_sf), border="grey")
census_nb <- poly2nb(census_sf, queen=F)
census_w<-nb2listw(census_nb, style="W", zero.policy = TRUE)

moran.plot(census_sf$total, census_w,
           xlab = "# of trials",
           ylab = "Neighbors # trials")

moran.test(census_sf$total, census_w, zero.policy = TRUE)    
# small-ish, but significant Moran's I: 0.159 

plot.nb(sf_nb, coords, add=T)
nb2INLA("~/usa.graph", sf_nb)
usa.adj <- paste("~/usa.graph")

census_sf$ID.area<-seq(1,nrow(census_sf))

# start with poisson model
model <- total_march ~ f(ID.area, model="bym", 
                      graph = usa.adj, 
                      adjust.for.con.comp = FALSE) + 
  POPPCT_URBAN + 
  log(hospital_beds + 1) +
  scale(universities) + 
  log(cases_march + 1) + 
  black_aa +
  college_education + 
  hispanic +
  medianincome 
  
result <- inla(model,
               family = "poisson",
               data = census_sf, E = population,
               control.compute=list(dic = TRUE, cpo = TRUE))

summary(result)



model <- total_april ~ f(ID.area, model="bym", 
                         graph = usa.adj, 
                         adjust.for.con.comp = FALSE) + 
  POPPCT_URBAN + 
  log(hospital_beds + 1) +
  log(universities + 1) + 
  log(cases_march + 1) + 
  black_aa +
  college_education + 
  hispanic +
  poverty

result <- inla(model,
               family = "poisson",
               data = census_sf, E = population,
               control.compute=list(dic = TRUE, cpo = TRUE))

summary(result)


model <- total ~ f(ID.area, model="bym", 
                         graph = usa.adj, 
                         adjust.for.con.comp = FALSE) + 
  POPPCT_URBAN + 
  log(hospital_beds + 1) +
  log(universities + 1) + 
  log(cases_march + 1) + 
  log(cases_april + 1) +
  log(cases_may + 1) + 
  log(cases_june + 1) + 
  black_aa +
  college_education + 
  hispanic +
  poverty

may <- inla(model,
               family = "poisson",
               data = census_sf, E = population,
               control.compute=list(dic = TRUE, cpo = TRUE))

summary(may)

result <- inla(model,
               family = "nbinomial",
               data = census_sf, E = population,
               control.compute=list(dic = TRUE, cpo = TRUE))

summary(result)



# make adjacency matrix and add to data
nb.r <- poly2nb(census_sf, queen = FALSE)
mat <- nb2mat(nb.r, style = "B", zero.policy = TRUE) 

# add ID
census_sf$ID.area<-seq(1,nrow(census_sf))

# source functions
source("model_functions.R")

# regression formula
model_formula <- paste0(
  "POPPCT_URBAN + 
  log(hospital_beds + 1) +
  log(universities + 1) + 
  log(cases+1) + 
  black_aa +
  hs_education +
  college_education + 
  hispanic +
  medianincome +
  poverty +
  days"
)

# case model
case_model <- fit_model(dat = trials_model, 
                        states = state,
                        model_outcome = "total",
                        model_formula = model_formula) 

#### SCRATCH ####



model <- summary(glm(total ~ 1 + 
                       POPPCT_URBAN + 
                       poverty +
                       offset(log(population)), 
                     family = "poisson", 
                     data = trials_census))
model

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






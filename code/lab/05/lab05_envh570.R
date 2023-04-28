#Lab 05 ENV H/EPI 570
#Updated 24 April 2023 by Joan Casey

#Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  weathermetrics,
  drat,
  dplyr,
  tidyr,
  hurricaneexposure,
  sf,
  ggplot2,
  MetBrewer,
  LowRankQP
)
##############################################################################################################
#Installing and loading the the tidysynth package
if (!requireNamespace("remotes")) install.packages("remotes")

# See: https://stackoverflow.com/questions/24194409/
if (!requireNamespace("LowRankQP", quietly = TRUE)) {
  remotes::install_version("LowRankQP", 
                            version = "1.0.5", 
                            repos = "http://cran.us.r-project.org",
                            upgrade = "never")
}

if (!requireNamespace("Synth", quietly = TRUE)) {
  remotes::install_version("Synth", 
                            version = "1.1-6", 
                            repos = "http://cran.us.r-project.org",
                            upgrade = "never")
}

if (!requireNamespace("tidysynth", quietly = TRUE)) {
  remotes::install_github("edunford/tidysynth", upgrade = "never")
}

if (!requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
  remotes::install_version("hurricaneexposuredata", 
                           repos = "geanders",
                           upgrade = "never")
}

library(tidysynth) 
library(hurricaneexposuredata)
data("hurr_tracks")

##############################################################################################################
#LAB STARTS HERE#

#PART 1#
# Visualize hurricane data 
# Storm track
hurricaneexposure::map_tracks(storm = "Harvey-2017")
# Distance from the storm track to the county
hurricaneexposure::map_counties(storm = "Harvey-2017", metric = "distance")
# Estimated wind speed in the county
hurricaneexposure::map_counties(storm = "Harvey-2017", metric = "wind")
# Estimated wind gusts in the county
hurricaneexposure::map_counties(storm = "Harvey-2017", metric = "wind", wind_var = "vmax_gust")

# Distance from Hurricane
dist <- hurricaneexposure::filter_storm_data(storm = "Harvey-2017", output_vars = c("fips", "closest_date", "storm_dist"))
dist <- dist %>% dplyr::mutate(closest_date = as.Date(closest_date))
                        
#Let's consider Galveston as our exposed county. FIPS = 48167
#Identifying counties located farther than 200 km (unexposed group)
dist <- dist %>% dplyr::filter(fips=="48167" | storm_dist>200)
                        
#Create binary exposed variable 
dist <- dist %>% dplyr::mutate(exposed = ifelse(fips=="48167", 1,0))
                        
#Check data
table(dist$exposed) #1 exposed county (Harris)

##############################################################################################################
## PART 2: ALL-CAUSE MORTALITY DATA
mortality <- readRDS("data/lab/05/county_mortality.RDS")

# Restrict to counties in dist dataframe
mortality <- mortality %>% dplyr::filter(fips %in% dist$fips)

#Filter to before 2018 and after 1999
mortality <- mortality %>% dplyr::filter(year>1999 & year<2018) 

#Filter to just September (first full month after exposure in 2017)
mortality <- mortality %>% dplyr::filter(month==9) 

#Create state fips
mortality  <-  mortality  %>% dplyr::mutate(st_fips = substr(fips, 1, 2))

# Create a variable that is the death rate per 1,000
# mortality <- mortality %>% mutate(death_rate = ADD ME HERE)
mortality <- mortality %>% dplyr::mutate(death_rate = n_deaths/pop * 1000)


##############################################################################################################
## PART 3: Other data to help generate the synthetic control
#Annual median income and unemployment
covar <- readRDS("data/lab/05/income_unemp.rds")
mortality <- dplyr::left_join(mortality, covar)
mortality <- dplyr::left_join(mortality, dist)

##############################################################################################################
## PART 4: the synthetic control

#Get rid of missing data because we need complete dataset for this code to work
summary(mortality)
mortality <- mortality %>% tidyr::drop_na(death_rate, unemp, med_inc)

#Also need counties to have complete years
mortality <- mortality %>% group_by(fips) %>% mutate(n=n())
table(mortality$n)
mortality <- mortality %>% dplyr::filter(n==17) #just keeping counties with data in all 17 years

#Look at the number of counties by state
table(mortality$state_name)

#Also going to restrict to counties in Texas, Missouri, North Carolina
#because model takes too long to run
mortality <- mortality %>% dplyr::filter(
    state_name == "Texas" |
    state_name == "Arkansas" |
    state_name == "Mississippi")

#Just keep needed variables
mortality <- mortality %>% dplyr::select(fips, year, death_rate, unemp, med_inc)

mortality_out <-
  mortality %>%
  # initial the synthetic control object
  synthetic_control(
    outcome = death_rate,
    # outcome
    unit = fips,
    # unit index in the panel data
    time = year,
    # time index in the panel data
    i_unit = "48167",
    # unit where the intervention occurred
    i_time = 2017,
    # time period when the intervention occurred
    generate_placebos = T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(
    time_window = 2000:201,
    mmed_inc = mean(med_inc, na.rm = T),
    muemply = mean(unemp, na.rm = T)
  ) %>%
  
  # Lagged mortality
  generate_predictor(time_window = 2000,
                     death_2000 = death_rate) %>%
  generate_predictor(time_window = 2005,
                     death_2005 = death_rate) %>%
  generate_predictor(time_window = 2010,
                     death_2010 = death_rate) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2000:2017,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>% # time to use in the optimization task
  
  # Generate the synthetic control
  generate_control()

##############################################################################################################
#Part 5 plotting etc.
#Plot Galveston County over time versus other counties included
mortality_out %>% plot_trends()

#Plot differences in counties over time
mortality_out %>% plot_differences()

#How balanced were we after implementing the synthetic control?
mortality_out %>% grab_balance_table()

#Which of our (limited) variables contributed most?
mortality_out %>% plot_weights()

#Which counties contribute most to the synthetic control?
weights <- mortality_out %>% grab_unit_weights()
View(weights)





                        
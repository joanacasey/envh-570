# Lab 03 ENV H/EPI 570
# Updated 2024-04-08 by Joan Casey

# Clear workspace of all objects and unload all extra (non-base) packages.
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  weathermetrics,
  dplyr,
  tidyr,
  # hurricaneexposure,   # See below.
  sf,
  ggplot2,
  MetBrewer,
  # LowRankQP,           # See below.
  BiocManager
)

############################################################################
# Install and load the tidysynth & hurricaneexposure packages and dependencies
pacman::p_install_version("LowRankQP", version = "1.0.5")
pacman::p_install_version("Synth", version = "1.1-6")
pacman::p_load(LowRankQP, Synth)
pacman::p_load_gh("edunford/tidysynth")
pacman::p_load_gh("geanders/hurricaneexposure")
pacman::p_load_gh("geanders/hurricaneexposuredata")
data("hurr_tracks")

#############################################################################
# LAB STARTS HERE #

# PART 1#
# Visualize hurricane data 
# Storm track
hurricaneexposure::map_tracks(storm = "Florence-2018")
# Distance from the storm track to the county
hurricaneexposure::map_counties(storm = "Florence-2018", metric = "distance")
# Estimated wind speed in the county
hurricaneexposure::map_counties(storm = "Florence-2018", metric = "wind")
# Estimated wind gusts in the county
hurricaneexposure::map_counties(storm = "Florence-2018", metric = "wind", 
                                wind_var = "vmax_gust")


# Distance from Hurricane
dist <- hurricaneexposure::filter_storm_data(
  storm = "Florence-2018", output_vars = c("fips", "closest_date", "storm_dist"))
dist <- dist %>% dplyr::mutate(closest_date = as.Date(closest_date))

# Let's consider Onslow, NC as our exposed county. FIPS = 37133
# Identifying counties located farther than 200 km (unexposed group)
dist <- dist %>% dplyr::filter(fips=="37133" | storm_dist>200)

# Create binary exposed variable 
dist <- dist %>% dplyr::mutate(exposed = ifelse(fips=="37133", 1,0))

# Check data
table(dist$exposed) # 1 exposed county (Onslow)

#############################################################################
## PART 2: ALL-CAUSE MORTALITY DATA
mortality <- readRDS(here("data/lab/05/county_mortality.RDS"))

# Restrict to counties in dist dataframe
mortality <- mortality %>% dplyr::filter(fips %in% dist$fips)

# Filter to before 2018 and after 1999
mortality <- mortality %>% dplyr::filter(year>1999 & year<2019) 

# # Filter to September (month of hurricane exposure in 2018)
mortality <- mortality %>% dplyr::filter(month==9) 

# Create state fips
mortality  <-  mortality  %>% dplyr::mutate(st_fips = substr(fips, 1, 2))

# Create a variable that is the death rate per 1,000
# mortality <- mortality %>% mutate(death_rate = ADD ME HERE)
mortality <- mortality %>% dplyr::mutate(death_rate = n_deaths/pop * 1000)


############################################################################
## PART 3: Other data to help generate the synthetic control
# Annual median income and unemployment
covar <- readRDS(here("data/lab/05/income_unemp.rds"))
mortality <- dplyr::left_join(mortality, covar)
mortality <- dplyr::left_join(mortality, dist)

############################################################################
## PART 4: the synthetic control
# library(tidysynth)   # Why load this again?

# Get rid of missing data because we need complete dataset for this code to work
summary(mortality)
mortality <- mortality %>% tidyr::drop_na(death_rate, unemp, med_inc)

# Look at the number of counties by state
table(mortality$state_name)

#Look at number of "exposed" counties by state
table(mortality$state_name, mortality$exposed)

# Also going to restrict to counties in Texas, Missouri, North Carolina
# because model takes too long to run
mortality <- mortality %>% dplyr::filter(
  state_name == "Georgia" |
    state_name == "Maryland" |
    state_name == "Virginia" |
    fips=="37133" ) #Need to keep county of interest that is outside of TX, MO, NC

# Just keep needed variables
mortality <- mortality %>% dplyr::select(fips, year, death_rate, unemp, med_inc)

# Also need counties to have complete years
mortality <- mortality %>% group_by(fips) %>% dplyr::mutate(n=n())
table(mortality$n)
table(mortality$year)
mortality <- mortality %>% dplyr::filter(n==18) # just keeping counties with data in all 18 years

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
    i_unit = "37133",
    # unit where the intervention occurred
    i_time = 2018,
    # time period when the intervention occurred
    generate_placebos = T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(
    time_window = 2000:2018,
    mmed_inc = mean(med_inc, na.rm = T),
    muemply = mean(unemp, na.rm = T)
  ) %>%
  
  # Lagged mortality
  generate_predictor(time_window = 2000,
                     death_2000 = death_rate) %>%
  generate_predictor(time_window = 2001,
                     death_2001 = death_rate) %>%
  generate_predictor(time_window = 2002,
                     death_2002 = death_rate) %>%
  generate_predictor(time_window = 2003,
                     death_2003 = death_rate) %>%
  generate_predictor(time_window = 2004,
                     death_2004 = death_rate) %>%
  generate_predictor(time_window = 2005,
                     death_2005 = death_rate) %>%
  generate_predictor(time_window = 2006,
                     death_2006 = death_rate) %>%
  generate_predictor(time_window = 2007,
                     death_2007 = death_rate) %>%
  generate_predictor(time_window = 2008,
                     death_2008 = death_rate) %>%
  generate_predictor(time_window = 2009,
                     death_2009 = death_rate) %>%
  generate_predictor(time_window = 2010,
                     death_2010 = death_rate) %>%
  generate_predictor(time_window = 2011,
                     death_2011 = death_rate) %>%
  generate_predictor(time_window = 2012,
                     death_2012 = death_rate) %>%
  generate_predictor(time_window = 2013,
                     death_2013 = death_rate) %>%
  generate_predictor(time_window = 2014,
                     death_2014 = death_rate) %>%
  generate_predictor(time_window = 2015,
                     death_2015 = death_rate) %>%
  generate_predictor(time_window = 2016,
                     death_2016 = death_rate) %>%
  generate_predictor(time_window = 2017,
                     death_2017 = death_rate) %>%

  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2000:2018,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>% # time to use in the optimization task
  
  # Generate the synthetic control
  generate_control() 

############################################################################
# Part 5 plotting etc.
# Plot Galveston County over time versus other counties included
mortality_out %>% plot_trends()

# Plot differences in counties over time
mortality_out %>% plot_differences()

# How balanced were we after implementing the synthetic control?
mortality_out %>% grab_balance_table()

# Which of our (limited) variables contributed most?
mortality_out %>% plot_weights()

# Which counties contribute most to the synthetic control?
weights <- mortality_out %>% grab_unit_weights()
View(weights)




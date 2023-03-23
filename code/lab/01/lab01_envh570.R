#Lab 01 ENV H/EPI 570
#Updated 23 Mar 2023 by Joan Casey

#Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, usethis, dplyr, readr, tidyr, rlang, ggplot2)

#Read in outcome data (fetal deaths; "spontaneous intrauterine death of a fetus at any time during pregnancy"), downloaded from CDC Wonder:
fetal_data <- read_csv("data/lab/01/fetal_death.csv")

# Explore the data
glimpse(fetal_data)

#How many states? 
n_distinct(fetal_data$state) # 50 states + DC

#Add another line of code to explore something else about the data
#ADD HERE

# Some states do not have data for each year/race/death combo
# If table shows two entries for a state, they have counts for both Black and white pregnant people
table(fetal_data$state, fetal_data$year)

# Expand so we have a row for each year/race/state combo
# We expect a dataframe 51 states x 4 years x 2 race/ethnicities = 408
fetal_data_expanded <- fetal_data %>% tidyr::expand(state, year, mom_race_eth)
dim(fetal_data_expanded)

# Add this back to our main dataframe, we now have NA where data is not reported due to small numbers
fetal_data_new <- left_join(
  fetal_data_expanded,
  fetal_data,
  by = c(
    "state" = "state",
    "year" = "year",
    "mom_race_eth" = "mom_race_eth"
  )
)

## Let's create some data visualizations:
# Look at the data in 2017
fetal_data_new %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  dplyr::filter(year == 2017) %>%
  ggplot(aes(
    x = fetal_deaths,
    y = reorder(state, fetal_deaths),
    color = mom_race_eth
  )) +
  geom_point() +
  theme_minimal() +
  ylab("") + xlab("Fetal deaths, N") +
  scale_color_brewer("Mom race/ethnicity", palette = "Set1")

# Look across years
fetal_data_new %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  ggplot(aes(
    x = fetal_deaths,
    y = reorder(state, fetal_deaths),
    color = mom_race_eth
  )) +
  geom_point() +
  theme_minimal(base_size = 9) +
  facet_wrap( ~ year) +
  ylab("") +
  theme(axis.text.y = element_text(size = 7)) +
  xlab("Fetal deaths, N") +
  scale_color_brewer("Mom race/ethnicity", palette = "Set1")

# That's pretty impossible to see change over time, what about another way?
fetal_data_new %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  filter(state == "Texas" |
           state == "California" |
           state == "Florida" | state == "New York") %>%
  ggplot(aes(
    x = factor(year),
    y = fetal_deaths,
    color = mom_race_eth,
    group = mom_race_eth
  )) +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ state, scales = "free") +
  ylab("") +
  xlab("Fetal deaths, N") +
  scale_color_brewer("Mom race/ethnicity", palette = "Set1")

# How does this look spatially?
states = st_read("data/US_State_Albers.shp")
head(states)

# Add fetal death data to states shapefile
states = left_join(states, fetal_data_new, by = c("STATE_NAME" = "state"))
head(states)

# Plot 2017, grey shows where we have missing values
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  dplyr::filter(year == 2017) %>%
  ggplot() +
  geom_sf(aes(fill = fetal_deaths),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis_c("Fetal deaths", na.value = "grey") +
  facet_wrap( ~ mom_race_eth) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        # Hide panel borders and remove grid lines
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")

# Plot all years
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = fetal_deaths),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis_c("Fetal deaths", na.value = "grey") +
  theme_minimal(base_size = 14) +
  facet_grid(year ~ mom_race_eth) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")

# Create manual breaks for the scale
summary(states$fetal_deaths)

# Let's create quintiles
quantile(states$fetal_deaths, c(.2, .4, .6, .8), na.rm = T)
states = states %>%
  mutate(
    fetal_death_q = case_when(
      fetal_deaths < 47 ~ 1,
      fetal_deaths >= 47 &
        fetal_deaths < 114 ~ 2,
      fetal_deaths >= 114 &
        fetal_deaths < 230 ~ 3,
      fetal_deaths >= 230 &
        fetal_deaths < 367 ~ 4,
      fetal_deaths >= 367 ~ 5
    )
  )
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = factor(fetal_death_q)),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis(
    "Fetal deaths",
    na.value = "grey",
    discrete = T,
    breaks = c(1, 2, 3, 4, 5),
    labels = c("< 47", "(47-114]", "(114-230]", "(230-367]", "\u2265 367")
  ) +
  theme_minimal(base_size = 14) +
  facet_grid(year ~ mom_race_eth) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

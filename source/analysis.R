library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(patchwork)
library(viridis)
options(scipen=999)

# The functions might be useful for A4
source("../source/a4-helpers.R")
data <- get_data(num_records = -1)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}


## Section 2  ---- 
#----------------------------------------------------------------------------#
# What is the average value of my variable across all the counties (in a given year)? 
# Where is my variable the highest or lowest?  
# How much has my variable change over the last N years?

# Your functions and variables might go here ... <todo: update comment>
highest_jail <- function() {
  data <- data %>%
    filter(state == "CA") %>%
    group_by(county_name) %>%
    summarize(total_jail_pop_county = max(total_jail_pop)) %>%
    filter(total_jail_pop_county == max(total_jail_pop_county, na.rm = TRUE)) %>%
    pull(county_name)
  return(data)   
}


black_jail_pop_2018 <- function() {
  data <- data %>% 
    filter(state == "CA") %>%
    filter(year == max(year)) %>%
    filter(county_name == highest_jail()) %>%
    pull(black_jail_pop)
  return(data)   
}

white_jail_pop_2018 <- function() {
  data <- data %>% 
    filter(state == "CA") %>%
    filter(year == max(year)) %>%
    filter(county_name == highest_jail()) %>%
    pull(white_jail_pop)
  return(data)   
}

latinx_jail_pop_2018 <- function() {
  data <- data %>% 
    filter(state == "CA") %>%
    filter(year == max(year)) %>%
    filter(county_name == highest_jail()) %>%
    pull(latinx_jail_pop)
  return(data)   
}



summary_info <- list(
  highest_jail(),
  black_jail_pop_2018(),
  white_jail_pop_2018(),
  latinx_jail_pop_2018()  
)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  data <- data %>%
    group_by(year, state, county_name) %>%
    summarize(change_per_year = total_jail_pop - lag(total_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year) %>%
    summarize(change_per_year = sum(change_per_year, na.rm = TRUE))
return(data)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  p <- ggplot(data = get_year_jail_pop(), aes(x=year, y=change_per_year)) +
    geom_bar(stat="identity") +
    labs(y = "Total Jail Population", x = "Year") 
  return(p)
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- data %>% 
    group_by(year, state, county_name) %>%
    summarize(change_per_year = total_jail_pop - lag(total_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year, state) %>%
    summarize(change_per_year = sum(change_per_year, na.rm = TRUE))
  
  jail_pop_by_states <- subset(jail_pop_by_states, state %in% states)
    
  return(jail_pop_by_states)
}

# https://stackoverflow.com/questions/9350025/filtering-a-data-frame-on-a-vector

plot_jail_pop_by_states <- function(states) {
  p <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=change_per_year, group=state, color=state)) +
    geom_line() +
    labs(y = "Total Jail Population", x = "Year", group = "States") 
  
  return(p)
}



## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

View(data)

Seattle_vs_Los_Angeles_black_pop <- function() {
  data1 <- data %>%
    filter(year >= 1985) %>%
    filter(state == "CA" | state == "WA") %>%
    filter(county_name == "Los Angeles County" | county_name == "King County") %>%
    group_by(year, county_name) %>%
    summarize(change_per_year_black_pop = black_jail_pop - lag(black_jail_pop, na.rm = TRUE, default = 0))  
  return(data1)
}


plot_Seattle_vs_Los_Angeles_black_pop <- function () {
  p  <- ggplot(Seattle_vs_Los_Angeles_black_pop(), aes(fill=county_name, y=change_per_year_black_pop, x=year)) + 
    geom_bar(position="dodge", stat="identity")  +
    labs(y = "Total Jail Population", x = "Year", title = "Incarcertation Rates of Black Americans in King County vs Los Angeles County") 
  
  return(p)
}


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

data1 <- data %>%
  filter(year == 2018) %>%
  group_by(year, state, county_name, fips) %>%
  summarize(black_jail_pop = sum(black_jail_pop))  %>%
  arrange(-black_jail_pop) %>%
  head(200)

county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(data1, by = "fips")
 
jail_map <- ggplot(map_data) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3 
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "green", high = "pink") 

jail_map

  ## Load data frame ---- 



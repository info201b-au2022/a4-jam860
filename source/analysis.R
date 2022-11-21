library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(patchwork)
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
# There are four sections in this section. The first function finds the county
# in California that has the highest number of people in jails. 
# The second function finds the number of black people 
# that are in jail in the county with the highest number of people in jail. 
# The third function finds the number of white people that are in
# in the county with the highest number of people in jail. 
# The fourth functions finds the number of latinx people in the county with the highest number
# of people in jail.

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
#----------------------------------------------------------------------------#
# This function finds the growth of the U.S. prison population every year from 1970 to 2018.
get_year_jail_pop <- function() {
  data <- data %>%
    group_by(year, state, county_name) %>%
    summarize(change_per_year = total_jail_pop - lag(total_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year) %>%
    summarize(change_per_year = sum(change_per_year, na.rm = TRUE))
return(data)   
}

# This function plots a bar graph of the growth of the U.S. prison population from 1970 to 2018.
plot_jail_pop_for_us <- function()  {
  p <- ggplot(data = get_year_jail_pop(), aes(x=year, y=change_per_year)) +
    geom_bar(stat="identity") +
    labs(y = "Total Jail Population", x = "Year", title = "Increase of Jail Population in U.S. (1970-2018)", 
         caption = "This bar graph represents the growth of the U.S. prison population from 1970 to 2018." ) 
  return(p)
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# This function returns a dataframe of the growth of specific US  states, specified by the parameters
# between 1970 to 2018.
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

# This function returns a plotted line graph of the growth of specific US states between 1970 to 2018.
plot_jail_pop_by_states <- function(states) {
  p <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=change_per_year, group=state, color=state)) +
    geom_line() +
    labs(y = "Total Jail Population", x = "Year", group = "States" , title = "Increase of Jail Population in Washington, Oregon and California (1970-2018)",
           caption = "This line graph represents the growth of \nWashington, Oregon and California's prison population from 1970 to 2018. ") 
  
  return(p)
}



## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>


# This function finds the growth of Black Americans that are incarcerated throughout the years in 
# King County vs Los Angeles County
Seattle_vs_Los_Angeles_black_pop <- function() {
  data1 <- data %>%
    filter(year >= 1985) %>%
    filter(state == "CA" | state == "WA") %>%
    filter(county_name == "Los Angeles County" | county_name == "King County") %>%
    group_by(year, county_name) %>%
    summarize(change_per_year_black_pop = black_jail_pop - lag(black_jail_pop, na.rm = TRUE, default = 0))  
  return(data1)
}

# This function plots a bar chart of the growth of Black Americans that are incarcerated throughout the years
# in King County vs Los Angeles County

plot_Seattle_vs_Los_Angeles_black_pop <- function () {
  p  <- ggplot(Seattle_vs_Los_Angeles_black_pop(), aes(fill=county_name, y=change_per_year_black_pop, x=year)) + 
    geom_bar(position="dodge", stat="identity")  +
    labs(y = "Total Jail Population", x = "Year", title = "Increase of incarcerations for Black Americans (King County vs Los Angeles County)", 
         caption = "This graphs represents a surprisingly large increasein the number of Black Americans \nin Los Angeles county's jail compared to King county's jail between 1975-2018.") 
  
  return(p)
}



#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# A map that shows potential patterns of inequality that vary geographically
#----------------------------------------------------------------------------#

# This function returns a map of the top 200 counties that have a high number
# of Black Americans in jail. We see that many of them are in major cities.
map_dataframe <- function() {
  data1 <- data %>%
    filter(year == 2018) %>%
    group_by(year, state, county_name, fips) %>%
    summarize(black_jail_pop = sum(black_jail_pop))  %>%
    arrange(-black_jail_pop) %>%
    head(200)
  
  return(data1)
}

map_inequality <- function() {
  county_shapes <- map_data("county") %>% 
    unite(polyname, region, subregion, sep = ",") %>%
    left_join(county.fips, by = "polyname")
  
  map_data <- county_shapes %>%
    left_join(map_dataframe(), by = "fips")
  
  jail_map <- ggplot(map_data) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
      color = "gray", size = 0.3 
    ) +
    coord_map() +
    scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "green", high = "pink") +
    labs(axes = FALSE, title = "Incarceration Rates for Black Americans across the U.S. (2018)",
         caption = "This graph represents the top 200 counties which have \nthe highest rates of incarceration for Black Americans in 2018.") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  return(jail_map)
  }



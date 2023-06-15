### Utils for mapping application

library(tidyverse)
library(ggplot2)
library(shiny)
library(tidycensus)
library(scales)

### Get shapefiles and Census data at county or state level
get_shapefiles <- function(country = "USA", level, state = "CA", county = "Los Angeles"){
  message("Retrieving shapefiles...")
  
  if (country == "USA"){
    if (level == "US States"){
      shp <- get_acs(
        #state = state,
        geography = "state",# "county", # "tract",
        variables = variables$varname,
        geometry = TRUE,
        year = 2020)
    } else if (level == "Counties within a state"){
      shp <- get_acs(
        state = state,
        geography = "county",
        variables = variables$varname,
        geometry = TRUE,
        year = 2020)
      
    } else if (level == "Subdivisions within a county"){
      shp <- get_acs(
        state = state,
        county = county,
        geography = "tract",
        variables = variables$varname,
        geometry = TRUE,
        year = 2020)
      # }
      
    } else stop("For USA; enter level 1 (states) or 2 (counties)")
    
  }
  
  message("Shapefiles retrieved.")
  return(shp)
}

### Mapping function for visualizing spatial distribution of a variable
create_map <- function(shp, level, outcome_name, varname){
  
  message("Creating map...")
  # produce plot
  map <- ggplot() + 
    geom_sf(data = subset(shp, variable == varname), aes(fill = estimate), 
            color = "white", size = 0.4) +
    # geom_text(data= points,aes(x=X, y=Y, label=NAME),
    #           color = "black", fontface = "bold", check_overlap = FALSE) +
    scale_fill_viridis_c(alpha = .75, option = "magma", name = outcome_name) +
    theme_bw(base_size = 18,
             base_rect_size = 0,
             base_line_size = 0) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  if (level == "US States") {
    map <- map + lims(x = c(-125, -65), y = c(24, 50))
  }
  
  message("Map created.")
  
  return(map)
}

### Create df with variable names and codes
variables <- data.frame(varname = c("B19013_001", "B25031_001"),
                        text = c("Median household income", "Median gross rent per bedroom"))


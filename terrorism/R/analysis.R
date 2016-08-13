# Author: Danny Vilela
# Task: This file serves as the driver for the report's charts and figures.

source("functions.R")

# Setup
packages <- c("dplyr", "ggplot2", "ggthemes")
invisible(lapply(packages, library, character.only = TRUE))

# Read, load, subset our data
df <- read.csv("data/enigma-terrorism.csv", header = T)
location <- select(df, year = iyear, month = imonth, day = iday,
                   country.id = country, country.name = country_txt,
                   state = provstate, city = city, latitude = latitude,
                   longitude = longitude, region = region_txt,
                   primary.target = targtype1_txt, all.fatalities = nkill,
                   all.wounded = nwound)

## Interpret regional terrorism ##

## Tabularize data on region count
regional.dist <- freq.and.proportion(data = location, on = "region")

# What regions are most stricken by terrorism?
head(regional.dist)

# What proportion do those regions account for?
sum(head(regional.dist)$proportion)

## Interpret yearly terrorist incident count ##

# Tabularize data on year
yearly.dist <- freq.and.proportion(data = location, on = "year")
yearly.dist$year <- as.numeric(yearly.dist$year) + 1970

# What percentage of terrorism since 1970 has occured since 2010? Since 2000?
sum(yearly.dist[yearly.dist$year > 2010, ]$proportion)
sum(yearly.dist[yearly.dist$year > 2000, ]$proportion)

# Visualize terrorism as a rough distribution
ggplot(data = yearly.dist, aes(x = year, y = frequency, color = frequency)) +
    geom_line() + theme_fivethirtyeight() +
    scale_color_continuous(guide = FALSE, low = "grey50", high = "firebrick1") +
    ggtitle(label = "2014 saw 35 times as many incidents of terrorism as 1972") +
    scale_x_continuous(breaks = seq(1970, 2015, 5))

## Explore geographic terrorism ##

# Setup ggplot2 world map layer
world.map <- borders("world", colour = "gray76", fill = "gray55")
map.layer <- ggplot() + world.map

# Get valid locations based on non-NA longitudes and latitudes
valid.coordinates <- filter(location, !is.na(longitude) | !is.na(latitude))

# Plot terrorism over time, from 1970 - 2015
map.layer + geom_point(data = valid.coordinates, alpha = 1/25,
                       aes(x = longitude, y = latitude, color = year)) +
    lims(y = c(-57, NA)) + theme_fivethirtyeight() +
    scale_color_continuous(low = "blue", high = "orange", breaks = c(seq(1970, 2015, 2))) +
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          axis.text.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.title.y = element_blank(),
          legend.background = element_rect(fill = "gray94", size = .5, linetype = "dotted"),
          legend.position = c(0.495, 0),
          legend.key.size = grid::unit(0.35, "cm"),
          legend.key.width = grid::unit(6, "cm")) +
    ggtitle(label = "Terrorism has clustered around the Middle East, North Africa, and Indian Subcontinent",
            subtitle = "Incidents of terrorism worldwide since 1970, as recorded by the Global Terrorism Database")

# Filter for, plot terrorist incidents in the 21st century
after.2000 <- filter(valid.coordinates, year >= 2000)

map.layer + geom_point(data = after.2000, alpha = 1/25,
                       aes(x = longitude, y = latitude, color = year)) +
    lims(y = c(-57, NA)) + theme_fivethirtyeight() +
    scale_color_continuous(low = "blue", high = "orange", breaks = c(2000:2015)) +
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          axis.text.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.title.y = element_blank(),
          legend.background = element_rect(fill = "gray94", size = .5, linetype = "dotted"),
          legend.position = c(0.495, 0),
          legend.key.size = grid::unit(0.35, "cm"),
          legend.key.width = grid::unit(6, "cm")) +
    ggtitle(label = "Modern terrorism has persisted around the Middle East and Indian Subcontinent")

# Which regions have experienced the most of modern terrorism?
region.dist <- freq.and.proportion(data = after.2000, on = "region")

ggplot(data = region.dist,
       mapping = aes(x = region, y = proportion, fill = proportion)) +
    geom_bar(stat = "identity") +
    scale_fill_continuous(guide = FALSE, low = "grey", high = "firebrick1") +
    coord_flip() + scale_x_discrete(limits = region.dist$region) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          axis.text.y = element_text(), axis.title.y = element_blank(),
          axis.text.x = element_text(), axis.title.x = element_text(),
          plot.title = element_text(),
          legend.title = element_blank(), legend.background = element_blank()) +
    labs(y = "Proportion of Terrorist Incidents") +
    ggtitle(label = "Top 2 regions make up about 70% of modern terrorist incidents",
            subtitle = "Relatively, they dwarf the remaining countries' incident count.")

# Now, which countries? Plot it.
country.dist <- freq.and.proportion(data = after.2000, on = "country.name")[1:20, ]
country.dist$country.name <- as.character(country.dist$country.name)

# Rename WB&GS to reduce y-axis whitespace
country.dist$country.name <-
    replace(country.dist$country.name,
            country.dist$country.name == "West Bank and Gaza Strip", "Gaza Strip")

ggplot(data = country.dist,
       mapping = aes(x = country.name, y = proportion, fill = proportion)) +
    geom_bar(stat = "identity") +
    scale_fill_continuous(guide = FALSE, low = "grey", high = "firebrick1") +
    coord_flip() + scale_x_discrete(limits = country.dist$country.name) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          axis.text.y = element_text(), axis.title.y = element_blank(),
          axis.text.x = element_text(), axis.title.x = element_text(),
          plot.title = element_text(),
          legend.title = element_blank(), legend.background = element_blank()) +
    labs(y = "Proportion of Terrorist Incidents") +
    ggtitle(label = "Iraq has seen 20% of all terrorist incidents since 2000")

# Explore 2014-only incidents of terrorism
in.2014 <- filter(location, year == 2014)
in.2014.dist <- freq.and.proportion(data = in.2014, on = "country.name")

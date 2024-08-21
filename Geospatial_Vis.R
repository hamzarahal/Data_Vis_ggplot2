########################Geospatial Visualizations####################################
########################################################################################

### Obtaining a Google Maps API Key


library(tidyverse)
library(ggmap)

# Insert your own API Key below
register_google(key="XXXXXXXXXXXXXXX")

# Working with Map Data

# Insert your own API key below
register_google(key="XXXXXXXXXXXXXXX")

# Use quick map to look at a city map
qmap("New York, NY", zoom=10)

# Zoom in and out
qmap("New York, NY", zoom=15)
qmap("New York, NY", zoom=18)
qmap("New York, NY", zoom=7)
qmap("New York, NY", zoom=10)

# Get the same map with get_map
nyc_map <- get_map("New York, NY", zoom=10)

# And plot it
ggmap(nyc_map)

#### Geocoding Points


# Geocode a few locations
nyc <- geocode("New York, NY")
nyc

lynda <- geocode("Lynda.com")
lynda

whitehouse <- geocode("White House")
whitehouse

# Map those points
nyc_map <- get_map(nyc)
ggmap(nyc_map)

# You don't have to store the map, wrapping get_map with ggmap is 
# similar to qmap
ggmap(get_map(lynda))

# And you can store a map as an object
whitehouse_map <- ggmap(get_map(whitehouse))
whitehouse_map

# Add a zoom factor as well
whitehouse_map <- ggmap(get_map(whitehouse, zoom=18))
whitehouse_map

### Changing Map Types

# Pull up a map of New York City again
nyc <- geocode("New York, NY")
ggmap(get_map(nyc))

# Experiment with different map types
ggmap(get_map(nyc, maptype = "terrain"))
ggmap(get_map(nyc, maptype = "roadmap"))
ggmap(get_map(nyc, maptype = "terrain-labels"))
ggmap(get_map(nyc, maptype = "terrain-lines"))
ggmap(get_map(nyc, maptype = "satellite"))
ggmap(get_map(nyc, maptype = "hybrid"))
ggmap(get_map(nyc, maptype = "toner"))
ggmap(get_map(nyc, maptype = "toner-lite"))
ggmap(get_map(nyc, maptype = "toner-background"))
ggmap(get_map(nyc, maptype = "watercolor"))

### Plotting Points on a Map

# Geocode a few locations
nyc <- geocode("New York, NY")
usa <- geocode("United States")

# Call up a map of the United States
ggmap(get_map(usa))
ggmap(get_map(usa, zoom=4))

# Plot a point on the map
ggmap(get_map(usa, zoom=4)) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=nyc)

# Create a tibble of locations
placenames <- c("New York, NY", "White House", "Lynda.com", "Mt. Rushmore", "The Alamo")
locations <- geocode(placenames)
places <- tibble(name=placenames, lat=locations$lat, lon=locations$lon)

# Plot the locations on the map
ggmap(get_map(usa, zoom=4)) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=places)

# Add their names using the text geometry
ggmap(get_map(usa, zoom=4)) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=places) +
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", data=places)

# Adjust the label position
ggmap(get_map(usa, zoom=4)) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=places) +
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", nudge_y=1, data=places)

# Try a simpler map
ggmap(get_map(usa, zoom=4, maptype = "toner-background")) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=places) +
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", nudge_y=1, data=places)

# Make a restaurant placemat
ggmap(get_map(usa, zoom=4, maptype="watercolor")) +
  geom_point(mapping=aes(x=lon, y=lat), color="red",data=places) +
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", nudge_y=1, data=places)

### Creating a US Map Manually


# Load the state map data 
states <- map_data("state")

# Inspect the state data
states

# Plot the state data
ggplot(data=states, mapping=aes(x=long, y=lat)) +
  geom_polygon()

# Add the group aesthetic
ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon()

# Fix the coordinate system
ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon() +
  coord_map()

# Remove the background
ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon() +
  coord_map() +
  theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank()) +
  theme(panel.background=element_blank())

### Creatinga Choropleth Map

# Load the college dataset
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Load the state map data 
states <- map_data("state")

# Determine the number of colleges in every state
college_summary <- college %>%
  group_by(state) %>%
  summarize(schools=n())

# Take a look at the result
college_summary
states

# Add the full state names to the college_summary tibble
college_summary <- college_summary %>%
  mutate(region=as.character(setNames(str_to_lower(state.name), state.abb)[as.character(state)]))

# Add DC back in
college_summary <- college_summary %>%
  mutate(region=ifelse(as.character(state)=="DC", "district of columbia",region)) 

# Join the college_summary and states tibbles
mapdata <- merge(states, college_summary, by="region")

# Plot a basic map
ggplot(mapdata) +
  geom_polygon(aes(x=long,y=lat,group=group)) +
  coord_map() +
  theme(plot.background=element_blank(), panel.background = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) 

# Convert to a choropleth map by using the schools variable as the fill
ggplot(mapdata) +
  geom_polygon(aes(x=long,y=lat,group=group, fill=schools)) +
  coord_map() +
  theme(plot.background=element_blank(), panel.background = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) 

# Change the color scheme
ggplot(mapdata) +
  geom_polygon(aes(x=long,y=lat,group=group, fill=schools)) +
  coord_map() +
  theme(plot.background=element_blank(), panel.background = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
  scale_fill_gradient(low = "beige", high = "red")

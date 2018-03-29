library(jsonlite)
library(stringr)
library(dplyr)
library(data.table)
library(plotly)


files = list.files(path = "./data", pattern="*.csv", full.names=TRUE)
journeys_raw = do.call(rbind, lapply(files, read.csv))
bikepoints_raw = fromJSON("https://api.tfl.gov.uk/bikepoint")

bikepoints = bikepoints_raw %>%
    select(id, lat, lon) %>%
    mutate(id = as.integer(str_extract(id, "\\d{1,3}$")))

journeys = journeys_raw %>% select(-EndStation.Name, -StartStation.Name)

journey_map = inner_join(journeys, bikepoints, by = c("EndStation.Id" = "id"))
setnames(journey_map, old=c("lat","lon"), new=c("EndLat", "EndLon"))
journey_map = inner_join(journey_map, bikepoints, by = c("StartStation.Id" = "id"))
setnames(journey_map, old=c("lat","lon"), new=c("StartLat", "StartLon"))

# find most common routes
routes = journey_map %>% 
    group_by(StartStation.Id, EndStation.Id) %>% 
    mutate(occurence = n()) %>%
    ungroup() %>%
    distinct(StartStation.Id, EndStation.Id, .keep_all = TRUE) %>%
    select(-Rental.Id, -Duration, -Bike.Id, -End.Date, -Start.Date)

routes_sample = routes[1:1000,]


minLat = min(routes$StartLat)
maxLat = max(routes$StartLat)
minLon = min(routes$StartLon)
maxLon = max(routes$StartLon)


# plot on the map
p <- plot_mapbox() %>%
    add_markers(
        data = routes_sample, x = ~StartLon, y = ~StartLat, text = ~StartStation.Id,
        size = ~occurence, hoverinfo = "text", alpha = 0.5, color = I("deepskyblue4")
    ) %>%
    add_segments(
        data = routes_sample,
        x = ~StartLon, xend = ~EndLon,
        y = ~StartLat, yend = ~EndLat,
        alpha = 0.3, color = I("darkturquoise"),
        size = I(1), hoverinfo = "none"
    ) %>%
    layout(
        title = 'Cycle routes',
        mapbox = list(
            style = 'light',
            zoom = 11,
            center = list(lat = (maxLat-minLat)/2+minLat,
                          lon = (maxLon-minLon)/2+minLon)
        ), 
        showlegend = FALSE
    )


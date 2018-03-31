library(jsonlite)
library(stringr)
library(dplyr)
library(reshape2)
library(cluster)
library(factoextra)
library(plotly)


# get bikepoints data
bikepoints = fromJSON("https://api.tfl.gov.uk/bikepoint")
# load journeys data
journeys_raw = read.csv("./data/journey.csv", stringsAsFactors = FALSE)

# select columns which we're interested in
bikepoints = bikepoints %>%
    select(id, commonName, lat, lon) %>%
    mutate(id = as.integer(str_extract(id, "\\d{1,3}$")))
rownames(bikepoints) = bikepoints$id

## Create a data for clustering - based on how busy the stations are at each hour
# 
# filter weekdays only
start_stations = journeys_raw %>% 
    select(StartStation.Id, Start.Date) %>%
    mutate(Start.Date = as.POSIXct(strptime(Start.Date, "%d/%m/%Y %H:%M"))) %>%
    mutate(is_weekday = (as.numeric(format(Start.Date, "%u"))) < 6) %>%
    filter(is_weekday == TRUE)

# the very first ride in the data set starts at an hour 00 and the very last starts at 23:51
# it means there's a full 24 hours cycle covered and we can use the number of days
# as the number of occurences of each hour
# 
# overall number of days
days_count = as.numeric(nrow(distinct(start_stations, format(Start.Date, "%F"))))

# count the yearly average of bikes taken per each hour on weekdays
bikes_taken_long = start_stations %>%
    group_by(StartStation.Id, format(Start.Date, "%H")) %>%
    summarise(bikes_per_hour = n()/days_count)
colnames(bikes_taken_long)[2] = "Hour"

# use Hour values as features describing the stations
a = dcast(bikes_taken_long, StartStation.Id ~ Hour, value.var = "bikes_per_hour")
rownames(a) = a$StartStation.Id

# the NA values mean there was 0 bikes taken within a particular hour
a[is.na(a)] = 0
bikes_taken = select(a, -StartStation.Id)


## Cluster data
# distance
stations_dist = get_dist(bikes_taken, method = "pearson")

# vizualize distance (takes long)
fviz_dist(stations_dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# find optimal number of clusters
fviz_nbclust(bikes_taken, kmeans, method = "gap_stat")

fviz_nbclust(bikes_taken, kmeans, method = "wss") +
    labs(subtitle = "Elbow method")

set.seed(123)
fviz_nbclust(bikes_taken, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")

# best candidates: 3 & 5 clusters
# k-means clustering
km_clusters <- kmeans(bikes_taken, 5, nstart = 25)
fviz_cluster(km_clusters, data = bikes_taken,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# hierarchical clustering
res.hc <- bikes_taken %>%
    dist(method = "euclidean") %>% # Compute dissimilarity matrix
    hclust(method = "ward.D2") 

fviz_dend(res.hc, k = 5, # Cut in four groups
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

bikepoints_clustered = merge(bikepoints, km_clusters$cluster, by="row.names", all = TRUE)
colnames(bikepoints_clustered)[6] = "Cluster"
bikepoints_clustered = na.omit(bikepoints_clustered)

minLat = min(bikepoints$lat)
maxLat = max(bikepoints$lat)
minLon = min(bikepoints$lon)
maxLon = max(bikepoints$lon)

# plot clusters on map
p <- bikepoints_clustered %>%
    plot_mapbox(lat = ~lat, lon = ~lon,
                split = ~Cluster, 
                marker = list(size = 10),
                type = 'scattermapbox', mode = 'markers',
                hoverinfo='text', text= ~commonName) %>%
    layout(title = 'Bike stations clusters (by frequency of bikes taken)',
           mapbox = list(style = 'light',
                         zoom = 11,
                         center=list(lat = (maxLat-minLat)/2+minLat,
                                     lon = (maxLon-minLon)/2+minLon)),
           legend = list(orientation = 'h',
                         font = list(size = 10))
           )

chart_link = api_create(p, filename="mapbox-clusters5")

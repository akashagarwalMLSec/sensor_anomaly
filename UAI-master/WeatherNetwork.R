library(tidyr)
library(corrr)
library(igraph)
library(ggraph)
library(network)  
library(maps) 
library(ggplot2)
library(mapdata)
library(GGally)
library(sna)
library(geomnet)
library(mapproj)
library(dplyr)

setwd('C:\\Users\\moniy\\Desktop\\GelLab\\Data')
#Canada_latlong <- read.csv('CanadaZipCodes.csv')
#LatLong <- Canada_latlong[,-2:-3]
#colnames(LatLong)[1] <- "PostalCode"

WeatherLatLong <- read.csv("WeatherDataNodes.csv")
colnames(WeatherLatLong)[1] <- "Code"

weather_W <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\weather_network_wideformat_v2.csv",header=TRUE)
df <- weather_W[-c(1)]


## Make coorelation matrix
mat=cor(df)

# Keep only high correlations
mat[mat<0.9] <- 0
mat[mat== 1] <-0
diag(mat) <- 0

# Make an Igraph object from this matrix:
network=graph_from_adjacency_matrix( mat, weighted=T, mode="undirected", diag=F)

#plot(network)

#g.Loss<- graph.adjacency(mat, mode="undirected",weighted=TRUE,diag=FALSE)

#E(g.Loss)$weight

#summary(E(g.Loss)$weight)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.5000  0.6378  0.8585  0.8084  0.9613  1.0000 

edge_W<-as.data.frame(cbind(get.edgelist(network) , round(E(network)$weight,5)))
colnames(edge_W)[1] <- "PostalCode.From"
colnames(edge_W)[2] <- "PostalCode.To"
colnames(edge_W)[3] <- "Weight"

node_W<-as.data.frame(vertex_attr(network))
Nodes_final <- merge(x = node_W, y = WeatherLatLong, by.x = "name" , by.y = "Code",all.x=T)
colnames(Nodes_final)[1] <- "Code"

## Edges
Edges_final <- merge(x = edge_W, y = WeatherLatLong, by.x = "PostalCode.From" , by.y = "Code",all.x=T)
colnames(Edges_final)[4] <- "Latitude.From"
colnames(Edges_final)[5] <- "Longitude.From"

Edges_final <- merge(x = Edges_final, y = WeatherLatLong, by.x = "PostalCode.To" , by.y = "Code",all.x=T)
colnames(Edges_final)[6] <- "Latitude.To"
colnames(Edges_final)[7] <- "Longitude.To"

Edges_reduced <- Edges_final[,1:3]



lat <- Nodes_final$latitude
lon <- Nodes_final$longitude

lon_max <- max(lon)
lat_max <- max(lat)
lon_min <- min(lon)
lat_min <- min(lat)

world_map <- map_data('world','Canada') 
ggplot() +
  # Plot map
  geom_map(data = world_map, map = world_map, aes(map_id = region),
           color = 'gray98',
           fill = 'gray85') +
  xlim(c(lon_min-5,lon_max+5)) + ylim(c(lat_min-3,lat_max+5)) +
  
  # Plot edges
  geom_segment(data = Edges_final, 
               alpha = 0.5,
               color = "red",
               aes(x = Longitude.From, y = Latitude.From,
                   xend = Longitude.To, yend = Latitude.To
               )) +
  scale_size(range = c(1,3)) +
  # Plot nodes
  geom_point(data = Nodes_final,
             aes(x = longitude,
                 y = latitude)) +
  
  coord_equal() +
  theme_bw()




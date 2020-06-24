
library(plotly)
packageVersion('plotly')
## [1] '4.5.2'
Sys.setenv("plotly_username"="moniyuv")
Sys.setenv("plotly_api_key"="w2NBdksdtxziaW5GHpbG")

Flight Paths Map
library(plotly)
library(dplyr)
library(later)
# airport locations
#air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
#flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
#flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'conic conformal'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_geo(color = I("red")) %>%
  add_markers(
    data = Nodes_final, x = ~Longitude, y = ~Latitude, text = ~PostalCode,
    hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = Edges_all,
    x = ~From.Latitude, xend = ~Latitude,
    y = ~From.Longitude, yend = ~Longitude,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  ) %>%
  layout(
    title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
    geo = geo, showlegend = FALSE, height=800
  )


chart_link = api_create(p, filename="map-flights")
chart_link



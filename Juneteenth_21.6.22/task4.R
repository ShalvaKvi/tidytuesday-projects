library(tidyverse)
library(ggtext)
library(extrafont)

# Pre-proccessing and Data Wrangling ----
# font used: https://www.1001fonts.com/yorktown-font.html

# Read data
## data <- tidytuesdayR::tt_load(x = tidytuesdayR::last_tuesday())
## route_data <- data[["slave_routes"]]

route_data <- read_csv("route_data.csv")

# Clean the route names a bit
route_data <- route_data |>
  mutate(port_arrival = str_remove_all(string = port_arrival, pattern = ", port unspecified"),
         port_origin = str_remove_all(string = port_origin, pattern = ", port unspecified"),
         place_of_purchase = str_remove_all(string = place_of_purchase, pattern = ", port unspecified"))


# Retrieve geolocation of ports

# library(tidygeocoder)
# route_data_unique <- route_data |>
#   select(port_arrival:port_origin) |> 
#   pivot_longer(cols = port_arrival:port_origin, values_to = "location", names_to = NULL) |> 
#   distinct()
# 
# 
# route_data_unique <- route_data_unique |> 
#   tidygeocoder::geocode(address = location)

route_data_unique <- read_csv("route_data_unique.csv") |> 
  mutate(location = ifelse(location == "Cap Francais", yes = "Cap-Haïtien", no = location))

# combine routes
route_data <- route_data |> 
  left_join(route_data_unique |>  select(location, "origin_lat" = lat, "origin_long" = long), 
            by = c("port_origin" = "location")) |> 
  left_join(route_data_unique |>  select(location, "purchase_lat" = lat, "purchase_long" = long), 
            by = c("place_of_purchase" = "location")) |> 
  left_join(route_data_unique |>  select(location, "arrival_lat" = lat, "arrival_long" = long), 
            by = c("port_arrival" = "location"))


# mutate trip names
route_data <- route_data |> 
  mutate(trip1 = paste(port_origin, "-", place_of_purchase),
         trip2 = paste(place_of_purchase, "-", port_arrival))

# drop NAs
route_data <- route_data |> 
  drop_na(origin_lat:arrival_long) |> 
  drop_na(n_slaves_arrived)


# Filter out routes which end at the same location, this causes errors with the plot
route_data <- route_data |> 
  filter(origin_long != purchase_long) |> 
  filter(purchase_long != arrival_long)

# Summarize route information
route_data_summ <- route_data |> 
  group_by(trip1, trip2) |> 
  summarise(n_trips = n(),
            n_slaves = sum(n_slaves_arrived)) |> 
  ungroup() |> 
  left_join(route_data |> select(trip1, origin_long, origin_lat, purchase_long, purchase_lat) |> distinct(),
            by = "trip1") |> 
  left_join(route_data |> select(trip2, arrival_long, arrival_lat) |> distinct(),
            by = "trip2")

# Map information, take out Antarctica
map <- map_data("world", interior = FALSE) |> 
  filter(region != "Antarctica")

# Take out trips that only occurred once, this is optional, makes final plot less hectic
route_data_summ <- route_data_summ |> 
  filter(n_trips > 1)

# Groups trips that occured more than once and sum the number of slaves
## For slave purchase (origin) location
route_data_purchase <- route_data_summ |> 
  group_by(purchase_long, purchase_lat) |> 
  summarise(n_slaves = sum(n_slaves))

route_data_purchase <- route_data_purchase |> 
  left_join(route_data |> select("loc_pur" = place_of_purchase, purchase_lat, purchase_long) |> distinct(),
            by = c("purchase_lat", "purchase_long"))

## For slave arrival location
route_data_arrival <- route_data_summ |> 
  group_by(arrival_long, arrival_lat) |> 
  summarise(n_slaves = sum(n_slaves))

route_data_arrival <- route_data_arrival |> 
  left_join(route_data |> select("loc_arr" = port_arrival, arrival_lat, arrival_long) |> distinct(),
            by = c("arrival_lat", "arrival_long"))
# Plot ----
plt <- ggplot() +
  
  #geoms
  ## Drawing map
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = "#d9b480", color = "#826c4d", size = 0.05)  +
  ## Drawing Curves
  geom_curve(data = route_data_summ, aes(x = purchase_long, y = purchase_lat,
                                         xend = arrival_long, yend = arrival_lat,
                                         group = trip2, alpha = n_slaves),
             color = "#2b2928", size = 0.2, linetype = 2, show.legend = FALSE) +
  ## Drawing points of purchase
  geom_point(data = route_data_purchase, aes(x = purchase_long, y = purchase_lat, size = n_slaves),
             color = "#612f2f",shape = 20, alpha = 0.8) +
  ## Drawing text for points of purchase
  geom_text(data = route_data_purchase, aes(x = purchase_long, y = purchase_lat, label = ifelse(n_slaves > 130000, loc_pur, "")),
            nudge_y = 3, nudge_x = 7, size = 3, color = "#2b2928", family = "Times New Roman") +
  ## Drawing points of arrival
  geom_point(data = route_data_arrival, aes(x = arrival_long, y = arrival_lat, size = n_slaves),
             color = "#54664f", shape = 18, alpha = 0.8) +
  ## Drawing text for points of arrival
  geom_text(data = route_data_arrival, aes(x = arrival_long, y = arrival_lat, label = ifelse(n_slaves > 200000, loc_arr, "")),
            nudge_y = 2, nudge_x = -6, size = 3, color = "#2b2928", family = "Times New Roman") +

  # scales
  scale_y_continuous(limits = c(-55, 100)) +
  scale_size_continuous(range = c(1,15)) +
  scale_alpha_continuous(range = c(0.05, 1)) +
  
  # Annotations
  ## Header
  annotate("text", x = 0, y = 95, size = 26, family = "Yorktown", color = "#2b2928", alpha = 0.9,
           label = "Slave Trade Routes 1562-1864") +
  ## Slave Departure Location
  annotate("point", x = c(-196,-184,-169), y = -27, size = c(3,10,15), shape = 20, alpha = 0.9, color = "#612f2f") +
  annotate("text", x = -181, y = -32, size = 4, alpha = 0.9, color = "#612f2f", family = "Yorktown", label = "100 - 300,000 - 600,000") +
  annotate("text", x = -161, y = -27, size = 8, alpha = 0.9, color = "#612f2f", family = "Yorktown", hjust = 0,
           label = "Slave Deprature Location") +
  ## Slave Arrival Location
  annotate("point", x = c(-196,-184,-169), y = -39, size = c(3,10,15), shape = 18, alpha = 0.9,color = "#54664f") +
  annotate("text", x = -181, y = -45, size = 4, alpha = 0.9,color = "#54664f", family = "Yorktown", label = "100 - 300,000 - 600,000") +
  annotate("text", x = -161, y = -39, size = 8, alpha = 0.9,color = "#54664f", family = "Yorktown", hjust = 0,
           label = "Slave Arrival Location") +
  ## Legend text
  annotate("text", x = -180, y = -21, size = 5, alpha = 0.9, color = "#2b2928", family = "Yorktown", label = "Number of Slaves") +
  ## Caption
  annotate("text", x = 120, y = -55, size = 4, alpha = 0.9, color = "#2b2928", family = "Times New Roman", 
           label = "#TidyTuesday 21/6/2022 | Coordinates retrieved via 'tidygeocoder' package | Only displaying routes with >1 trips") +
  
  # theme adjustments
  ggthemes::theme_map() +
  theme(panel.background = element_rect(fill = "#bac4ba"),
        panel.grid.major = element_line(color = "#93999b"),
        panel.grid.minor = element_line(color = "#93999b"),
        legend.position = "none")

  
plt

# save
ggsave("plt.jpeg", plt, device = "jpeg", height = 3000, width = 6000, units = "px")
  





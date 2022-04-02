### TidyTuesday 29.3.22: "Collegiate sports"
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-29/readme.md

# Load Libraries -
pacman::p_load(tidyverse, tidylog,
               rgdal,geojsonio, rgeos)


##### Data wrangling -----

# Load data
sports <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# Clean data
sports <- sports %>%
  filter(year==2019) %>% # 2019 only
  filter(!is.na(state_cd)) %>%
  select(institution_name,state_cd,sports,partic_men,partic_women) %>% # Relevant
  filter(!is.na(partic_men)) %>% # remove NA for men participants
  filter(!is.na(partic_women)) # remove NA for women participants

# Calculate the female athelete proportion
state_prop <- sports %>%
  group_by(state_cd) %>% # group by institution 
  summarise(women_prop = sum(partic_women)/(sum(partic_men)+sum(partic_women))) # calculate prop for every institution
  

##### Graphical presentation -----

### Geographical map 
# Loading geographical json file (https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map)
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Re-format the geojson file into dataframe so we can work with ggplot
spdf_df <- broom::tidy(spdf, region = "iso3166_2")

# Calculate the centroid of each hexagon to add the label
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Merge geospatial and numerical information
spdf_df <- spdf_df %>%
  left_join(. , state_prop, by=c("id"="state_cd")) 



### ggplot
hexplot <- ggplot() +
  geom_polygon(data = spdf_df,color="grey95",size=0.1,aes(fill=women_prop, x=long, y=lat, group=group)) +
  scale_fill_gradient2(low="steelblue4",high="tomato2",mid="grey80",midpoint=0.5,name="Proportion")+
  geom_text(data=centers, aes(x=x, y=y, label=id), color="grey95", size=3, alpha=1) +
  theme_void() +
  coord_map()+
  labs(title="Proportion of College Female Athletes 2019",
       caption = "TidyTuesday 29.3.22")+
  guides(fill = guide_colourbar(barwidth = 0.4, barheight = 5,ticks = 0,))+
  theme(
    text = element_text(color = "grey30",face = "bold"),
    plot.background = element_rect(fill = "grey95", color = NA), 
    panel.background = element_rect(fill = "grey95", color = NA), 
    legend.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size= 18, hjust=0.4,vjust=0.2),
    
  )

ggsave("hexplot.jpeg",units = "px", width=1920, height=1080, dpi=300)

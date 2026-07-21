setwd(file.path("2026","Spatial_Dem","day2"))
path <- file.path("shps_data")
images <- file.path("images")
## Exercise 0: Very basic data manipulation in R ####
# Create the data frame ####
library(tidyverse) # IM CALLING THE LIBRARY TIDY

df <- data.frame(
  id_census_tract = c(
    "0801901001", "0801901002", "0801901003",
    "0801902001", "0801902002", "2807901001",
    "2807901002", "2807901003", "2807901004",
    "2807902001", "2807902002", "2807903003"
  ),
  country_of_birth = c(
    "Spain", "Spain", "Other",
    "Spain", "Other", "Spain",
    "Spain", "Spain", "Other",
    "Spain", "Spain", "Other"
  ),
  sex = c(1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1)
)

df

# Add new columns to dataframe: mutate ######

df <- df |>
  mutate(id_mun=substr(id_census_tract,1,5))

df

# Subset a dataframe: Filter works over rows  ######

df_mad<-df|>
  filter(id_mun=="28079")

df_mad

df_bcn<-df|>
  filter(id_mun=="08019")

df_bcn

# Subset a dataframe: select works over columns  ######
names(df)

df1<-df|>
  select(1,2,4)

df1

df1b <- df |>
  select(c("id_census_tract","country_of_birth", "id_mun"))

df1b

df2 <- df |>
  select(-3)

df2

df2b <- df |>
  select(-"sex")

df2b
# Group your data: group_by, summarise and ungroup.  ######

df_mun <- df |>
  group_by(id_mun) |>
  summarise(pop=n()) |>
  ungroup()

df_mun

df_mun_origin <- df |>
  group_by(id_mun,country_of_birth) |>
  summarise(pop=n()) |>
  ungroup()

df_mun_origin

df_mun_origin_pro <- df |>
  group_by(id_mun,country_of_birth) |>
  summarise(pop=n()) |>
  mutate(prop= pop / sum(pop)) |>
  ungroup()

df_mun_origin_pro

df_mean_foreign <- df_mun_origin_pro |>
  mutate(country="Spain") |>
  filter(country_of_birth=="Other") |>
  group_by(country) |>
  summarise(mean_foreign=mean(prop))

df_mean_foreign

# COMPUTE QUANTILES OF A GIVEN DISTRIBUTION

random_numbers <- rnorm(100, mean = 23, sd = 7)  # Adjust mean/sd as needed
random_numbers <- round(pmin(pmax(random_numbers, 1), 45))

df<-data.frame(id=seq(1:100),
               value=random_numbers)

ggplot(df, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Values", x = "Value", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = value)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Values", x = "Value", y = "Frequency") +
  theme_minimal()


quartiles <- quantile(df$value, probs = seq(0, 1, 0.25), na.rm = TRUE)
quartiles

median_value <- quantile(df$value, probs = seq(0, 1, 0.5), na.rm = TRUE)
median_value

percentiles <- quantile(df$value, probs = seq(0, 1, 0.01), na.rm = TRUE)
percentiles


## Exercise 00: Dissolve geometries   ######
library(sf)

mad_ct_shp  <- read_sf(dsn = path, "mad_ct_shp")

ggplot(mad_ct_shp) +
  geom_sf() +
  theme_bw()

# dissolving for districts

mad_dis_shp <- mad_ct_shp |>
  mutate(districts=paste(CMUN,CDIS, sep=""))|>
  group_by(districts) |>
  summarise(NDIS = unique(CDIS)) |>
  st_buffer(0.5) |> # it is used to remove sliders (missing points in the process)
  st_cast()

ggplot(mad_dis_shp) +
  geom_sf() +
  theme_bw()

# dissolving for municipalities
mad_mun_shp <- mad_ct_shp |>
  group_by(CPRO) |>
  summarise(NMUN = unique(NMUN)) |>
  st_buffer(0.5) %>%
  st_cast()

ggplot(mad_mun_shp) +
  geom_sf() +
  theme_bw()

# plot them together
ggplot() +
  geom_sf(data=mad_ct_shp, linewidth=.1) +
  geom_sf(data=mad_dis_shp,fill=NA, linewidth=.5) +
  geom_sf(data=mad_mun_shp,fill=NA, linewidth=1) +
  theme_bw()


ggsave(
  paste(images, "Madrid0.png", sep = ""),# name of the file of the image
  scale = 1,
  dpi = 300,
  height = 10,
  width = 11
)

## Exercise 1: Crop a map   ######

# It could be the case that we want to zoom in into a particular area of a bigger sf_objet. Let's see how we can achieve this in R. For this exercise we use a map of the world from [gisco](https://ec.europa.eu/eurostat/web/gisco), the Geographic Information System of the European Commission. We can easily access the spatial information of gisco using the [giscoR](https://ropengov.github.io/giscoR/) library. **giscoR** is an API package that helps to retrieve data from Eurostat - GISCO (the Geographic Information System of the European Commission).

library(giscoR)
library(sf)
library(tidyverse)

world <- gisco_get_countries(
  resolution = "03", region = NULL,
  epsg = 4326
)
st_crs(world)

ggplot(data = world) +
  geom_sf(fill="antiquewhite")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"))

# For this example I'm using the bbox of Spain. If you need the coordinates
# of a bbox of any part of the world you can get them from https://www.openstreetmap.org/

ggplot(data = world) +
  geom_sf(fill="antiquewhite")+
  theme_bw()+
  # cood_sf() is used to define the bounding box to zoom in.
  coord_sf(
    xlim = c(-11.54, 8.90),
    ylim = c(35.26, 44.32))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"))

## Choropleth maps   ######


## Exercise 2: Choropleth maps (Foreign-born population by my municipalty, Madrid 2022)   ######

setwd(file.path("2026","Spatial_Dem","day2"))
path <- file.path("shps_data")
images <- file.path("images")

#### 1. Read statistical data into R   ######

library(sf)
library(tidyverse)

load(file.path(path,"PC2022.Rda")) # read data

# compute the proportion and group them by municipality and place they were born
mad_muns_data <- data |>
  filter(PROVINCIA == "28") |>
  mutate(PAISNAC2 = if_else(PAISNAC == "108", "108", "999")) |>
  group_by(MUNICIPIO, PAISNAC2) |>
  summarise(pop = n()) |>
  ungroup() |>
  group_by(MUNICIPIO) |>
  mutate(prop = pop / sum(pop)) |>
  ungroup() |>
  filter(PAISNAC2 == "999") |>
  mutate(
    prop_foreign_cat = case_when(
      prop <= .05 ~ "<5%",
      prop <= .10 ~ "(5-10%]",
      prop <= .15 ~ "(10-15%]",
      prop <= .20 ~ "(15-20%]",
      prop <= .25 ~ "(20-25%]",
      prop <= .30 ~ "(25-30%]",
      prop > .30 ~ ">30%"
    ),
    prop_foreign_cat = fct_relevel(
      factor(prop_foreign_cat),
      "<5%",
      "(5-10%]",
      "(10-15%]",
      "(15-20%]",
      "(20-25%]",
      "(25-30%]",
      ">30%"
    )
  )

head(mad_muns_data)

quantile(mad_muns_data$prop, probs = seq(0,1,.1)) # checking the distribution

rm(data)

#### 2. Read spatial data into R ########

# Read the spatial data into R and create a sf object with the division of Madrid by municipalities.

shp_ct  <- read_sf(dsn = path, "SECC_CE_20220101")

# dissolving census tracts to have municipalities
mad_muns_shp <- shp_ct |>
  filter(CPRO == "28") |>
  group_by(CUMUN) |>
  summarise(NMUN = unique(NMUN)) |>
  st_buffer(0.5) |>
  st_cast()

colnames(mad_muns_shp)[1]<-"MUNICIPIO" # rename municipality code

# note: the st_buffer function helps us cleaning sliver, however it can create some misalignment between layers.

ggplot(mad_muns_shp) +
  geom_sf() +
  theme_bw()

#### 3. Join statistical and spatial data #######
# we never join the spatial data to the statistical one, it is always the other way around
mad_muns_shp <- mad_muns_shp |>
  left_join(mad_muns_data, by = "MUNICIPIO")

head(mad_muns_shp)

#### 4. Create a color palette to associate it with the data####

library(RColorBrewer)

display.brewer.all()

myColors <- c(brewer.pal(7, "YlOrRd")) # create palette

barplot(rep(length(myColors), length(myColors)),
        col = c(myColors));myColors

#### 5. Plot a choropleth map (categories) with municipal boundaries and save it as an image ######

Map1 <- ggplot(mad_muns_shp, aes(fill = prop_foreign_cat)) +
  geom_sf(colour = "black", linewidth = .1) +
  scale_fill_manual(name = "Foreign-born (%)", values = myColors) +
  labs(
    x = "\nLongitude",
    y = "Latitude\n",
    title = "Foreign-born population by municipality",
    subtitle = "Madrid 2022",
    caption = "Elaboration: Juan Galeano\nData: Population Register (INE)"
  ) +
  theme_bw()

Map1

ggsave(
  file.path(images, "Madrid1.png"),# name of the file of the image
  scale = 1,
  dpi = 300,
  height = 10,
  width = 11
)

#### 6. Plot a choropleth map (categories) with municipal boundaries, add north row and scale bar. Updated theme. #####
library(ggspatial) # adding spatial information for the context

Map2 <- ggplot(mad_muns_shp, aes(fill = prop_foreign_cat)) +
  geom_sf(colour = "black", linewidth = .1) +
  scale_fill_manual(
    name = "Foreign-born (%)",
    values = myColors,
    # Legend on the bottom part of the graph

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  labs(
    x = "\nLongitude",
    y = "Latitude\n",
    title = "Foreign-born population by municipality",
    subtitle = "Madrid 2022",
    caption = "Elaboration: Juan Galeano | Data: Population Register (INE)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    plot.caption  = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) +
  # adding range part
  annotation_scale(location = "bl", width_hint = 0.5) +
  # adding an north arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.75, "in"),
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  )

Map2

ggsave(file.path(images,"Madrid2.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 9)


#### 7. Provide Context: Add a subplot in map.####

CCAASHP <- read_sf(dsn = path, "ESP_PROV_CAN")

ggplot(data = CCAASHP) +
  geom_sf() +
  theme_bw()

# add to Madrid a specific value
CCAASHP <- CCAASHP %>%
  mutate(col = if_else(ID_1 == 8, 1, 0))

myColors3 <- c("#BDBDBD", "#FF0000")
CCAASHP$col <- as.factor(CCAASHP$col)

# create a map for Spain and print Madrid in red
ESP <- ggplot(CCAASHP, aes(fill = col)) +
  geom_sf(colour = "Black", linewidth = .015) +
  coord_sf() +
  scale_fill_manual(name = "", values = myColors3) +
  labs(title = "Spain") +
  theme(
    plot.title = element_text(
      lineheight = 5.6,
      size = 15,
      face = "bold"
    ),
    legend.title = element_text(
      angle = 0,
      vjust = 0.5,
      size = 15,
      colour = "black",
      face = "bold"
    ),
    legend.text = element_text(colour = "black", size = 15),
    legend.position = 'none',
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = NA),
    legend.key.size = unit(1.5, 'lines'),
    strip.text = element_text(
      angle = 0,
      vjust = 0.5,
      size = 15,
      colour = "black",
      face = "bold"
    ),
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = NA),
    plot.background =  element_rect(fill = NA),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.border = element_rect(
      color = "black",
      linewidth   = .5,
      fill = NA
    )
  )

ESP

# add the Spanish map to the other one
Map3 <- ggplot(mad_muns_shp,
               aes(fill = prop_foreign_cat)) +
  geom_sf(colour = "black",linewidth=.1) +
  scale_fill_manual(name="Foreign-born (%)",
                    values=myColors,
                    # Legend

                    guide = guide_legend(
                      title.position = "top",
                      title.hjust=0.5,
                      direction = "horizontal",
                      nrow = 1,
                      keywidth=4,
                      keyheight=.5,
                      label.position = "bottom"))+
  labs(x="\nLongitude",
       y="Latitude\n",
       title="Foreign-born population by municipality",
       subtitle="Madrid 2022",
       caption="Elaboration: Juan Galeano | Data: Population Register (INE)")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"),
        plot.caption  = element_text(hjust=0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl",
                         which_north = "true",
                         pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  # to add Spanish map, we need to have the unit of the map
  annotation_custom(grob = ggplotGrob(ESP),
                    xmin = 359000,
                    xmax = 415000,
                    ymin = 4457310,
                    ymax = 4627310)

Map3
st_crs(mad_muns_shp)

ggsave(file.path(images,"Madrid3.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 9)


#### 8. Provide Context: Get map tile the **ggmap** approach ####

library(ggmap)
# note osm for getting bbox

st_bbox(mad_muns_shp)
st_crs(mad_muns_shp)
# Transform CRS
mad_muns_shp_t <- st_transform(mad_muns_shp, 4326)
st_bbox(mad_muns_shp_t)

# setting API Key (we need to do it just once!)
register_stadiamaps(key = "add90c3f-e565-432f-ad1f-e0331077cafd", write=TRUE)

# get the maptile
mad_terrain<-get_stadiamap(bbox = c(left = as.numeric(st_bbox(mad_muns_shp_t)[1]),
                                    bottom = as.numeric(st_bbox(mad_muns_shp_t)[2]),
                                    right = as.numeric(st_bbox(mad_muns_shp_t)[3]),
                                    top = as.numeric(st_bbox(mad_muns_shp_t)[4])),
                             zoom = 10, # from 0 to 18, the higher it is, the more detailed we have
                             maptype = c("stamen_terrain"), # which kind of maptiles we want to have
                             crop = TRUE,
                             messaging = FALSE)

# MAPTYPES
#stamen_terrain, stamen_toner, stamen_toner_lite, stamen_watercolor, stamen_terrain_background, stamen_toner_background, stamen_terrain_lines, stamen_terrain_labels, stamen_toner_lines, stamen_toner_labels.

# transform ggmap object to a ggplot element
mad_terrain <- ggmap(mad_terrain,
                     extent = "panel",
                     legend = "topleft",
                     darken = 0)
mad_terrain

# use it directly on ggplot

Map4 <- mad_terrain +
  geom_sf(
    data = mad_muns_shp_t,
    aes(fill = prop_foreign_cat),
    colour = "black",
    linewidth = .1,
    inherit.aes = FALSE # WE HAVE TO SET IT AS FALSE
  ) +
  scale_fill_manual(
    name = "Foreign-born (%)",
    values = myColors,
    # Legend

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      keywidth = 4,
      keyheight = .5,
      label.position = "bottom"
    )
  ) +
  labs(
    x = "\nLongitude",
    y = "Latitude\n",
    title = "Foreign-born population by municipality",
    subtitle = "Madrid 2022",
    caption = "Elaboration: Juan Galeano | Data: Population Register (INE)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    plot.caption  = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) +
  annotation_scale(location = "bl", width_hint = 0.45) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.75, "in"),
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  )

Map4

ggsave(file.path(images,"Madrid4.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 9)

#### 9. Provide more information: add and histogram to your choropleth map ####

ggplot(mad_muns_data, aes(x=prop)) +
  geom_histogram()

# Create a theme for plotting #####
theme_fancy_map <- function() {
  theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.13,
        size = rel(1.4)
      ),
      plot.subtitle = element_text(hjust = 0.13, size = rel(1.1)),
      plot.caption = element_text(
        hjust = 0.13,
        size = rel(0.8),
        color = "grey50"
      ),
    )
}

hist_legend <- ggplot(mad_muns_data, aes(x = prop)) +
  # Fill each histogram bar using the x axis category that ggplot creates
  geom_histogram(
    aes(fill = after_stat(factor(x))),
    binwidth = .05,
    boundary = 0,
    color = "black",
    linewidth = 0.1
  ) +
  geom_text(
    stat = "bin",
    binwidth = .05,
    boundary = 0,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 3.5
  ) +
  coord_cartesian(ylim = c(0, 85)) +
  # Fill with the same palette as the map
  scale_fill_brewer(palette = "YlOrRd", guide = "none") +
  scale_x_continuous(
    breaks = seq(0, .35, by = .05),
    labels = c("0%", "5", "10", "15", "20", "25", "30", "35%")
  ) +
  labs(x = "Foreign-born") +
  # Theme adjustments
  theme_fancy_map() +
  theme(
    axis.text.x = element_text(size = rel(1)),
    axis.title.x = element_text(
      size = rel(1),
      margin = margin(t = 1, b = 1),
      face = "bold"
    )
  )
hist_legend

mad_toner<-get_stadiamap(bbox = c(left = as.numeric(st_bbox(mad_muns_shp_t)[1]),
                                  bottom = as.numeric(st_bbox(mad_muns_shp_t)[2]),
                                  right = as.numeric(st_bbox(mad_muns_shp_t)[3]),
                                  top = as.numeric(st_bbox(mad_muns_shp_t)[4])),
                         zoom = 11,
                         maptype = c("stamen_toner"),
                         crop = TRUE,
                         messaging = FALSE)

# MAPTYPES
#stamen_terrain, stamen_toner, stamen_toner_lite, stamen_watercolor, stamen_terrain_background, stamen_toner_background, stamen_terrain_lines, stamen_terrain_labels, stamen_toner_lines, stamen_toner_labels.

mad_toner <- ggmap(mad_toner,
                   extent = "panel",
                   legend = "topleft",
                   darken = 0)

mad_toner

map1 <- mad_toner +
  geom_sf(
    data = mad_muns_shp_t,
    aes(fill = prop),
    linewidth = .1,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_fill_stepsn(
    colours = scales::brewer_pal(palette = "YlOrRd")(7),
    breaks = seq(0, 0.35, by = .05),
    limits = c(0, .35),
    labels = c("0%", "5", "10", "15", "20", "25", "30", "35%")
  ) +
  # Theme stuff
  theme_fancy_map() +
  theme(legend.position = "none")

# Add the histogram to the map
library(patchwork)

combined_map_hist <- map1 +
  inset_element(
    hist_legend,
    left = 0.025,
    bottom = 0.025,
    right = 0.4,
    top = 0.25
  )

combined_map_hist

ggsave(
  file.path(images, "Madrid4b.png"),
  plot = combined_map_hist,
  scale = 1,
  dpi = 300,
  height = 13,
  width = 10
)

## Cholera outbreak 1854: The Jhon Snow map ####

## Excercise 3: Heatmaps, The cholera outbreak in Soho ####
library(ggmap)

shp_deaths <- read_sf(path, "Cholera_Deaths")
shp_pumps <- read_sf(path, "Pumps")

# check CRS for them
st_crs(shp_deaths)
st_crs(shp_pumps)

# transform crs
shp_deaths <- st_transform(shp_deaths, 4326)
shp_pumps <- st_transform(shp_pumps, 4326)

bbox <- st_bbox(shp_deaths)

# get some margins for the origin margin bounding box to have more margin
margin <- 1.5
left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))

# separate columns for geometry to create the heatmap
shp_deaths <- cbind(shp_deaths, st_coordinates(shp_deaths))

# get stadiamap for the neighborhood of Soho in London
soho <- get_stadiamap(
  bbox = c(
    left = left,
    bottom = bottom,
    right = right,
    top = top
  ),
  zoom = 18, # zoom in the map for more detailed data
  maptype = c("alidade_smooth"),
  crop = TRUE,
  messaging = FALSE
)

soho <- ggmap(soho)
soho

soho1 <- soho +
  geom_sf(
    data = shp_deaths,
    size = 3.5,
    color = "red",
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = shp_pumps,
    size = 5,
    shape = 15,
    color = "blue",
    inherit.aes = FALSE
  ) +
  theme_bw()

soho1

ggsave(
  file.path(images, "9_POINT_MAP.png"),
  scale = 1,
  height = 12,
  width = 20,
  dpi = 300
)

theme_snow<-list(theme(plot.title = element_text(lineheight=1, size=30, face="bold",hjust = 0.5),
                     plot.subtitle = element_text(lineheight=1, size=25, face="bold",hjust = 0.5),
                     plot.caption = element_text(lineheight=1, size=15, hjust=0.5),
                     legend.title = element_blank (),
                     legend.text = element_text(colour="black", size = 13),
                     legend.position="bottom",
                     legend.background = element_rect(fill=NA, colour = NA),
                     legend.key.size = unit(1.5, 'lines'),
                     legend.key = element_rect(colour = NA, fill = NA),
                     axis.title.x = element_blank (),
                     axis.text.x  = element_blank (),
                     axis.title.y =  element_blank (),
                     axis.text.y  = element_blank (),
                     axis.ticks= element_blank (),
                     strip.text = element_text(size=15, face="bold",margin = margin(.1,0,.1,0, "cm")),
                     plot.background =  element_rect(fill = "white"),
                     panel.grid.major=element_line(colour="white",linewidth=.5),
                     panel.grid.minor=element_line(colour="white",linewidth=.15),
                     panel.border = element_rect(colour = "white", fill=NA, linewidth=.75),
                     panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF")))

soho2 <- soho +
  # this is a function to create a density based on parameters
  stat_density2d(
    aes(x = X, y = Y, fill = ..level..),
    #colour= ..level..
    alpha = .15,
    bins = 15,
    color = "black",
    linewidth = 0.1,
    data = shp_deaths,
    geom = "polygon"
  ) +
  scale_fill_gradient2 (
    'Cholera Deaths\n2D Density',
    low = '#2b83ba',
    mid = '#ffffbf',
    high = '#d7191c',
    midpoint = 50000
  ) +
  geom_sf(
    data = shp_pumps,
    size = 5,
    shape = 15,
    color = "blue",
    inherit.aes = FALSE
  ) +
  labs(title = "Cholera Outbreak 1834",
       subtitle = "Soho, London (UK)",
       caption = "\nElaboration: Juan Galeano for the BSSD\nData: https://search.r-project.org/CRAN/refmans/HistData/html/Snow.html") +
  theme_snow +
  theme(legend.position = "bottom",
        legend.key.width = unit(5, "cm"))

soho2

ggsave(file.path(images,"10_HEATMAP.png"),
       scale = 1,
       height = 12,
       width=13,
       dpi = 300)


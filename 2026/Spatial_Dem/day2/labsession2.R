#'------------------------
#'CONSOLIDATION EXCERSICES
#'day2
#'------------------------
invisible(gc())
rm(list = ls())
# directory and packages --------------------------------------------------

library(sf)
library(tidyverse)
library(RColorBrewer)
library(giscoR)
library(ggmap)
library(patchwork)

# directory
setwd(file.path("2026","Spatial_Dem","day2"))
path <- file.path("shps_data")
images <- file.path("images")


# 1 - Violence around the globe -----------------------------------------------

### 1. Read csv in R

df_violence <- read_csv(file.path(path,"violence_women.csv"))

# Replace na values

df_violence <- df_violence |>
  mutate(Value = replace_na(Value, 0))

### 2. Join csv data with world sf object

# Import world data

world <- gisco_get_countries(
  resolution = "03", region = NULL,
  epsg = 4326
)

st_crs(world)

ggplot(data = world) +
  geom_sf(fill="antiquewhite")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"))

# Join it with statistic data

shp_violence <- world |>
  left_join(
    df_violence,
    by = join_by(ISO3_CODE)
  )

### 3. Categorizing values

shp_violence <- shp_violence |>
  arrange(Value) |>
  mutate(
    rank = ntile(Value, 10),
    rank = if_else(Value == 0 | is.na(Value), 0, rank)
  ) |>
  mutate(rank = factor(rank,
                       levels = 0:10,
                       labels = c("No Data","P10","P20","P30","P40","P50",
                                  "P60","P70","P80","P90","P100")))

### 4. Plot it for Eastern Asia

# function for theme

theme_lab2 <- function(){
  theme(
    plot.title = element_text(lineheight=1, size=30, face="bold",hjust = 0.5),
    plot.subtitle = element_text(lineheight=1, size=25, hjust = 0),
    plot.caption = element_text(lineheight=1, size=15, hjust=0.5),
    legend.title = element_blank (),
    legend.text = element_text(colour="black", size = 13),
    legend.position="bottom",
    legend.background = element_rect(fill=NA, colour = NA),
    legend.key.size = unit(1.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.title.x = element_blank (),
    axis.text.x  = element_text(colour = "grey80"),
    axis.title.y =  element_blank (),
    axis.text.y  = element_text(colour = "grey80"),
    axis.ticks= element_blank (),
    strip.text = element_text(size=15, face="bold",margin = margin(.1,0,.1,0, "cm")),
    plot.background =  element_rect(fill = "white"),
    panel.grid.major=element_line(colour="grey95",linewidth=.5),
    panel.grid.minor=element_line(colour="grey95",linewidth=.15),
    panel.border = element_rect(colour = "grey95", fill=NA, linewidth=.75),
    panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF")
  )
}

# Set the palette
display.brewer.all()

myColors <- c("grey80",rev(brewer.pal(10, "RdBu"))) # create palette

barplot(rep(length(myColors), length(myColors)),
        col = c(myColors));myColors

# set the centroid
sf_use_s2(FALSE)
centroids_world <- st_centroid(shp_violence)
shp_violence <- cbind(shp_violence, st_coordinates(st_centroid(centroids_world$geometry)))
sf_use_s2(TRUE)

shp_violence |>
  ggplot() +
  aes(fill = rank) +
  geom_sf(colour = "black", linewidth = .1) +
  scale_fill_manual(name = "Percentiles",
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
                    )) +
  geom_label(aes(x=X, y=Y, label=NAME_ENGL),
             fill = "grey95",
             color = "grey20",
             alpha = .5,
             fontface = "bold",
             size=2.5,
             check_overlap = TRUE) +
  labs(
    x = "\nLongitude",
    y = "Latitude\n",
    title = "Ranking of the highest Self-reported violence against women - Asia",
    subtitle = "Violence: physical and/or sexual",
    caption = "Elaboration: Thiago Cordeiro-Almeida\nData: OECD(2020)."
  ) +
  coord_sf(
    xlim = c(50.89, 155.13),
    ylim = c(1.68, 56.85)) +
  theme_lab2()

# save plot

ggsave(file.path(images,"Exercise_ViolenceAgainstWomen.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =10, #25  #10
       width = 15)


# 2. AirBnbs in Bcn ----------------------------------------------------------

### 1. Read shps and correct CRS

# Read shp for districts in BCN

bcn_districts <- read_sf(dsn = path, "bcn_districts")

bcn_districts |>
  ggplot() +
  geom_sf()

st_crs(bcn_districts)
st_bbox(bcn_districts)

# read Airbnb file

load(file.path(path,"AIRBNB_BCN.Rdata"))
head(AIRBNB_BCN)

st_crs(AIRBNB_BCN)
st_bbox(AIRBNB_BCN)

# transforming projections of bcn districts object
bcn_districts <- st_transform(bcn_districts, crs = 4326)
st_crs(bcn_districts)

### 2. Heatmap

# Set margins to get external map
bbox <- st_bbox(AIRBNB_BCN)
margin <- .8
left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))

# get stadiamap
bcn_bkg <- get_stadiamap(
  bbox = c(
    left = left,
    bottom = bottom,
    right = right,
    top = top
  ),
  zoom = 15, # zoom in the map for more detailed data
  maptype = c("stamen_toner_lite"),
  crop = TRUE,
  messaging = FALSE
)

bcn_bkg <- ggmap(bcn_bkg)
bcn_bkg

# centroid
centroid <- st_centroid(bcn_districts)
bcn_districts <- cbind(bcn_districts, st_coordinates(st_centroid(centroid$geometry)))

# creating heatmap
bcn_bkg +
  # this is a function to create a density based on parameters
  stat_density2d(
    aes(x = X, y = Y, fill = ..level..),
    #colour= ..level..
    alpha = .15,
    bins = 15,
    color = "black",
    linewidth = 0.2,
    data = AIRBNB_BCN,
    geom = "polygon"
  ) +
  geom_sf(
    data = AIRBNB_BCN,
    size = .5,
    alpha = .25,
    # shape = 15,
    color = "grey90",
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2 (
    'Number of Airbnb\n(2D Density)',
    low = '#2b83ba',
    mid = '#ffffbf',
    high = '#d7191c',
    midpoint = 600
  ) +
  geom_sf(
    data = bcn_districts,
    fill = "red3",
    alpha = .02,
    color = "red3",
    linewdith = .5,
    inherit.aes = FALSE
  ) +
  geom_label(data = bcn_districts,
            aes(x=X, y=Y, label=NOM),
            color = "grey20",
            fill = "grey80",
            fontface = "bold",
            size=3,
            check_overlap = TRUE) +
  labs(title = "Airbnb",
       subtitle = "Barcelona, Spain",
       caption = "\nElaboration: Thiago Cordeiro-Almeida for the BSSD\nData: Inside Airbnb") +
  theme(legend.position = "bottom",
        legend.key.width = unit(5, "cm")) +
  theme_lab2()

ggsave(file.path(images,"Exercise2_AirbnbBcn.png"),
       scale = 1,
       height = 12,
       width=13,
       dpi = 300)

#'------------------------
#'CONSOLIDATION EXCERSICES
#'day3
#'------------------------
invisible(gc())
rm(list = ls())
# directory and packages --------------------------------------------------

# packages
library(sf)
library(tidyverse)
library(RColorBrewer)
library(giscoR)
library(ggmap)
library(patchwork)

# directory
setwd(file.path("2026","Spatial_Dem","day3"))
path <- file.path("data")
images <- file.path("images")


# Exeercise 1 - hexagons in Madrid ----------------------------------------

# Import data of neightborhoods in Madrid

mad_nbh <- read_sf(dsn = path, "Barrios_madrid")

mad_nbh |>
  ggplot() +
  geom_sf()

st_bbox(mad_nbh)
st_crs(mad_nbh)

# Import data of bars in Madrid

mad_bars <- read_sf(dsn = path, "bars_madrid_municipality")

mad_bars |>
  ggplot() +
  geom_sf()

st_bbox(mad_bars)
st_crs(mad_bars)

# Transform CRS into the same one for all of them

mad_bars <- st_transform(mad_bars, crs = 4326)
mad_nbh <- st_transform(mad_nbh, crs = 4326)

mad_bars |> st_crs() # check it out
mad_nbh |> st_crs() # check it out

# dissolve neightborhoods to have a layer for madrid

mad_mun <- mad_nbh |>
  group_by(mun) |>
  summarise(mun = unique(mun)) |>
  ungroup() |>
  st_buffer(.00002) |>
  st_cast()

mad_mun |>
  ggplot() +
  geom_sf()

# set boundaries
bbox <- st_bbox(mad_nbh)
margin <- .8
left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))

# Geom tiles for Madrid

mad_tile <- get_stadiamap(bbox = c(left = left,
                                   bottom = bottom,
                                   right = right,
                                   top = top ),
                          zoom = 13,
                          maptype = c("stamen_toner_lite"),
                          crop = TRUE,
                          messaging = FALSE)

mad_tile <- ggmap(mad_tile)
mad_tile

# create hexagons

mad_bars_hex <- st_make_grid(
  mad_bars,
  c(.0025, .0025), # size of the grids, big numbers are big hexagons
  what = "polygons",
  square = FALSE # set that we don't have squares, but hexagons
) |>
  st_sf() |>
  mutate(grid_id = row_number())

mad_bars_hex |>
  ggplot() +
  geom_sf(linewidth=.1)

# intersection of hexagons overlapping Madrid boundaries
mad_bars_hex <- mad_bars_hex |>
  st_filter(mad_mun, .predicate = st_intersects) # this is faster!

# mad_bars_hex1 <- st_intersection(mad_mun,mad_bars_hex)

mad_bars_hex |>
# mad_bars_hex1 |>
# mad_mun |>
  ggplot() +
  geom_sf(linewidth=.1)

# insert counting into hexagon layer

mad_bars_hex$count <- lengths(st_intersects(mad_bars_hex,mad_bars))

# replace NA
mad_bars_hex <- mad_bars_hex |>
  mutate(count = if_else(count == 0, NA_integer_,count))

mad_bars_hex <- mad_bars_hex |> drop_na() # drop na values

mad_tile +
  geom_sf(data = mad_bars_hex,
          linewidth=.025,
          aes(fill = count),
          inherit.aes = FALSE) +
  scale_fill_distiller(palette = "Spectral",
                       name="Distribution of bars",
                       na.value = "#EBEBEB") + # it allows a NA value being attribute for a color
  labs(x="\nLongitude",
       y="Latitude\n",
       title="Distribution of bars in Madrid",
       subtitle="Madrid (Spain)",
       caption="Elaboration: Thiago Cordeiro-Almeida for the BSSD\nData: .") +
  theme_light() +
  theme(legend.position = "right",
        plot.title = element_text(lineheight=1, size=10, face="bold"),
        plot.subtitle = element_text(vjust=0.5, size=8,colour="black"),
        plot.caption = element_text(vjust=0.5, size=6,colour="black"),
        legend.title = element_text(angle = 0,vjust=0.5, size=8,colour="black",face="bold"),
        legend.text = element_text(vjust=0.5, size=8,colour="black"),
        axis.line=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank())

# save it
ggsave(file.path(images,"Exercise_BarsInMadrid.png"),
       scale = 1,
       height = 12,
       width=20,
       dpi = 300)


# 2. Immigrant deaths in the Mediterranean  -------------------------------

# Read data of immigrant deaths

df_immig <- readxl::read_xlsx(file.path(path,"Missing_Migrants.xlsx"))

# data handling

df_immig <- df_immig |>
  filter(
    Region_Incident == "Mediterranean"
  ) |>
  drop_na()

# separate coordinates

df_immig$lon<-as.numeric(sub("^.*?,", "", df_immig$Coordinates))
df_immig$lat<-as.numeric(gsub(",.*$", "", df_immig$Coordinates))

# more data handle

df_immig <- df_immig |>
  select(everything(), "DEATHS" = 8, "MISSING OUT" = 9) |>
  # drop cases with missing data for deaths
  filter(!is.na(DEATHS), DEATHS > 0) |>
  pivot_longer(c(DEATHS,`MISSING OUT`), names_to = "incident", values_to = "n") |>
  mutate(
    prop = n / sum(n),
    .by = c(incident)
  )

# create cutoff points based on their distribution
quantile(df_immig$prop, probs = c(.25,.5,.75,.90))

# create categories

df_immig <- df_immig |>
 mutate(
   incident_fct = case_when(prop < .05 ~ "<5%",
                            prop >= .05 & prop < .1 ~ "[5-10%)",
                            TRUE ~ ">10%"),
   incident_fct = fct_relevel(incident_fct,
                              "<5%",
                              "[5-10%)",
                              ">10%")
 )

# attribute st as sf

df_immig <- st_as_sf(df_immig,
                     coords=c("lon", "lat"),
                     crs=4326)

# import gisco dataset for the world

res <- "03"
target_crs <- 4326
world <- gisco_get_countries(
  resolution = res, region = NULL,
  epsg = target_crs
)

# get boundaries
bbox <- st_bbox(df_immig)
margin <- 1.5
left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))

# other parameters
mysizes <- c(1,2,4)

# plot it

ggplot() +
  # plot world map
  geom_sf(data = world,
          fill="antiquewhite") +
  # tricky way of putting bubbles with bold contours
  geom_sf(data=df_immig,
          aes(size = incident_fct, color=incident),
          shape =21) +
  geom_sf(data=df_immig,
          aes(size = incident_fct, color=incident),
          alpha=.2) +
  scale_fill_brewer(palette="Set1")+
  scale_color_brewer(palette="Set1")+
  scale_size_manual(
    values = mysizes * 8,
    name = "Prop. of Incidents (%)",
    guide = guide_legend(
      direction = "horizontal",
      nrow = 1,
      keywidth = 5,
      label.position = "bottom",
      override.aes = list(
        shape = 21,
        fill = "grey80",     # or NA if you want hollow circles
        colour = "grey80",
        alpha = .3
      )
    )
  ) +
  guides(colour = "none") +
  coord_sf(
    xlim = c(left, right),
    ylim = c(bottom, top)
  ) +
  facet_wrap(~incident)+
  labs(title="\nLeft at the margins of the Mediterranean",
       subtitle = "\nDistribution of deaths and missing people in their route to European continent\n\n",
       caption="\nElaboration: Thiago Cordeiro-Almeida for the BSSD\n\nData: OIM, 2014-2023\n") +
  theme_void()+
  theme(plot.title = element_text(lineheight=1, size=24,color  =  "grey80", face="bold", hjust = 0.5),
        plot.subtitle = element_text(lineheight=1, size=15,color  =  "grey80", face="bold", hjust = 0.5),
        plot.caption = element_text(vjust=0.5, size=12,colour="grey80", hjust = 0.5),
        strip.text = element_text(lineheight=1, size=15, color  =  "grey80", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size=15,color  =  "grey80", face="bold"),
        legend.title = element_text(size=15,color  =  "grey80", face="bold"),
        # legend.key = element_rect(fill = alpha("red",0)),
        legend.background = element_rect(fill = "transparent"),
        # panel.background = element_rect(fill = "white", color  =  "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill ="black", color  = "black"),
        panel.grid.major = element_line(color = "white",
                                        linewidth = 0.15,
                                        linetype = 2))

# save it
ggsave(file.path(images,"Exercise_LeftAtMargins.png"),
       scale = 1,
       height = 12,
       width=20,
       dpi = 300)

# 3. Foreign-born pop in Madrid -------------------------------------------

# read data of population

load(file.path(path,"mad_pop.Rda"))

# read census track divisions for madrid

spain_ct <- read_sf(dsn = path, "SECC_CE_20220101")

# filtering countries
mad <- mad |>
  filter(PAISNAC %in% c(302,351,340,128)) |>
  mutate(PAISNAC = factor(PAISNAC,
                          levels = c(302,351,340,128),
                          labels = c("Uhited States","Venezuela",
                                     "Argentina","Romania"))) |>
  # create id for census tract
  mutate(id_tracts = as.numeric(paste0(MUNICIPIO,DISTRITO, SECCION))) |>
  # compute proportions
  summarise(pop = n(),
         .by = c(PAISNAC, id_tracts)) |>
  mutate(prop = pop / sum(pop),
         .by = PAISNAC)


#'------------------------
#'CONSOLIDATION EXCERSICES
#'day1
#'------------------------

# directory and packages --------------------------------------------------

library(sf)
library(tidyverse)
library(terra)
library(RColorBrewer)

# directory
shapes<- file.path("shps")
images<-file.path("IMAGES_class")


# 1 - Provinces and regions -----------------------------------------------

### 1. read data

esp <- read_sf(dsn = shapes, "ESP_adm2")

# let's see what we have

esp |>
  ggplot() +
  geom_sf(data = esp)

### 2. moving the Canarian Island to closer to the continental land
# obs: we dont have to resize them, we just have to move them closer

cannarians <- esp |>
  filter(NAME_1=="Islas Canarias")

cannarians |>
  ggplot() +
  geom_sf(data = cannarians)

# getting the geometries

cannarians_g <- st_geometry(cannarians)

# change geometries

cannarians_g <- cannarians_g+c(5,7.5)

# set the CRS

cannarians_g <- st_as_sf(cannarians_g, crs=st_crs(esp)) # set CRS

# change name of the column to geometry

st_geometry(cannarians_g) <- "geometry"

# drop the geometry column

df_cannarians <- st_drop_geometry(cannarians)

# join geometry column

cannarians_new <- bind_cols(df_cannarians, cannarians_g)

# new complete document

esp_new <- esp |>
  filter(NAME_1!="Islas Canarias") |>
  bind_rows(cannarians_new)

# save it as a shapefile

write_sf(esp_new, file.path(shapes,"ESP_adm2_cannariasnew.shp"))

### 3. Creating discrete map

# setting centroids

centroids_provinces <- st_centroid(esp_new)
esp_new <- cbind(esp_new, st_coordinates(st_centroid(centroids_provinces$geometry)))

# setting the theme for the map

theme_S1<-list(
  theme(plot.title = element_text(lineheight=1, size=20, face="bold",hjust = 0.5),
        plot.subtitle = element_text(lineheight=1, size=15, face="bold",hjust = 0.5),
        plot.caption = element_text(lineheight=1, size=13, hjust=0.5),
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
        panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF"))
)

# plotting it

ggplot() +
  geom_sf(data = esp_new,
          aes(fill = NAME_1),
          show.legend = FALSE) +
  geom_text(data = esp_new,
            aes(x=X, y=Y, label=NAME_2),
            color = "grey50",
            fontface = "bold",
            size=2.5,
            check_overlap = TRUE) +
  scale_fill_viridis_d(option = "F") +
  labs(title="Spain",
       subtitle="Provinces and regions (in colors)",
       caption="Elaboration: Thiago Cordeiro-Almeida for the BSSD\nData: INE España") +
  theme_S1

# save it

ggsave(file.path(images,"Exercise_Spain_provinceANDregions_map.png"),
       scale = 1,
       height = 8,
       width=12,
       dpi = 300)


# Exercise 2 - Madrid census tracts ---------------------------------------

### 1. read data

mad <- read_sf(dsn = shapes, "Seccionado_2019")

### 2. answering questions

# 2.1 - How many rows is the shp composed?

dim(mad)[1]

# 2.2 - CRS associated with the shp

st_crs(mad) # EPSG 25830

# 2.3 - boinding box corresponding to it

st_bbox(mad)

### 3. creating object with municipality of madrid

# we will filter it

mad_mun <- mad |>
  filter(CDMUNI == "079")

### 4. Create a new object with the division by neightbohoods

mad_neightborhoods <- mad_mun |>
  group_by(CDTBARRIO) |>
  summarise(
    neightborhoods = unique(CDTBARRIO)
  ) |>
  st_cast() |>
  select(-neightborhoods)

# 4.1 - how many neightborhoods is Madrid composed?

dim(mad_neightborhoods)[1]

### 5. Create a new object with the division by districts

mad_districts <- mad_mun |>
  group_by(CDDISTRITO) |>
  summarise(
    districts = unique(CDDISTRITO)
  ) |>
  st_cast() |>
  select(-districts)

# 5.1 - how many districts is Madrid composed?

dim(mad_districts)[1]

# 5.2 - districts with more neightborhoods in Madrid

mad_mun |>
  group_by(CDDISTRITO) |>
  summarise(
    n_neightborhoods = n_distinct(CDTBARRIO)
  ) |>
  arrange(desc(n_neightborhoods)) # district 15! Ciudad Lineal

### 6. Read file with name of districts in Madrid and joining it to our madrid sf

# read it
name_districts <- readxl::read_xlsx(file.path("shps","nombres_distritos_madrid.xlsx"),
                                    col_names = TRUE)

# join it to madrid mun

mad_mun <- mad_mun |>
  left_join(
    name_districts,
    by = join_by(CDDISTRITO)
  )

# doing similar for the districts

mad_districts <- mad_districts |>
  left_join(
    name_districts,
    by = join_by(CDDISTRITO)
  )

# save it

write_sf(mad_mun, file.path(shapes,"madrid_withNames.shp"))

### 7. create a map of it...

# centroids

mad_centroids <- st_centroid(mad_districts)
mad_districts <- bind_cols(mad_districts, st_coordinates(st_centroid(mad_centroids$geometry)))

# plot it!

ggplot() +
  geom_sf(data = mad_districts,
          aes(fill = CDDISTRITO),
          show.legend = FALSE) +
  geom_sf(data = mad_mun,
          fill=NA) +
  geom_text(data = mad_districts,
            aes(x=X, y=Y, label= NAME),
            color = "grey90",
            fontface = "bold",
            size=3.5,
            check_overlap = TRUE) +
  scale_fill_viridis_d(option = "F") +
  labs(title="Madrid",
       subtitle="Districts (colors) and census tracts",
       caption="Elaboration: Thiago Cordeiro-Almeida for the BSSD\nData: INE España") +
  theme_S1

ggsave(file.path(images,"Exercise_Madrid_districtsANDcensustract_map.png"),
       scale = 1,
       height = 8,
       width=12,
       dpi = 300)

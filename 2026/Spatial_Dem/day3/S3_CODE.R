setwd(file.path("2026","Spatial_Dem","day3"))
path <- file.path("shps_data")
images <- file.path("images")

## 1. Heatmaps: AIRBNB BARCELONA.

#### 1.1 Read sf object from a R library.######

library(tidyverse)
library(sf)
library(ggmap)
library(RColorBrewer)

theme_HS<-list(theme(plot.title = element_text(lineheight=1, size=30, face="bold",hjust = 0.5),
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


AIRBNB_BCN<- read_sf(".", "airbnb_data_bcn_2022")

DIST_BCN <- read_sf(".", "bcn_districts")
st_crs(DIST_BCN)


DIST_BCN2<-st_transform(
  DIST_BCN,4326)

ggplot()+
  geom_sf(data=DIST_BCN2)

bbox<-st_bbox(DIST_BCN2)

left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*1.1+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*1.1+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*1.1+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*1.1+mean(as.numeric(bbox[c(2,4)]))

BCN_TILE<-get_stadiamap(bbox = c(left = left,
                                 bottom = bottom,
                                 right = right,
                                 top = top ),
                        zoom = 14,
                        maptype = c("stamen_toner_lite"),
                        crop = TRUE,
                        messaging = FALSE)

BCN_TILE <- ggmap(BCN_TILE)
#BCN_TILE

AIRBNB_BCN<- cbind (AIRBNB_BCN,st_coordinates(AIRBNB_BCN))

points_BCN <- cbind(DIST_BCN2, st_coordinates(st_centroid(DIST_BCN2)))


BCN_HEATMAP <- BCN_TILE +
  stat_density2d(aes(x = X,
                     y = Y,
                     fill = after_stat(level)), #colour= ..level..
                 color="black",
                 alpha=.45,
                 bins = 12,
                 linewidth=0.1,
                 data = AIRBNB_BCN,
                 geom = "polygon")+
  scale_fill_gradient2 ('Cholera Deaths\n2D Density',
                        low = '#2b83ba',
                        mid = '#ffffbf',
                        high = '#d7191c',
                        midpoint=500)+
  labs(title="AIRBNB ACCOMODATIONS",
       subtitle="Barcelona, 2022",
       caption="\nElaboration: Juan Galeano for the BSSD\nData: http://insideairbnb.com/")+
  geom_sf(data=DIST_BCN2, linewidth=.5,fill=NA,color="black",inherit.aes = FALSE)+
  geom_sf(data=AIRBNB_BCN, size=.25, shape=15, alpha=.5,color="black",inherit.aes = FALSE)+
  geom_label(data= points_BCN,
             aes(x=X, y=Y, label=NOM),
             color = "black",
             fontface = "bold",
             size=7,
             fill = "lightgrey")+
  coord_sf(xlim = c(2.12, 2.22), ylim = c(41.365, 41.42))+
  theme_HS+
  theme(legend.position="bottom",
        legend.key.width=unit(6.3,"cm"))

BCN_HEATMAP

ggsave(file.path(images,"1_HEATMAP.png"),
       scale = 1,
       height = 12,
       width=13,
       dpi = 300)

#### 1.2: Hexabin maps, count point in polygons ######

# dissolving to have the outside boundaries of BCN
bcn_CITY <-DIST_BCN2 |>
  group_by(ID_ANNEX) |>
  summarise(NAME_prov = unique(ID_ANNEX)) |>
  #st_buffer(0.25) |>
  st_cast()

ggplot(data = bcn_CITY) +
  geom_sf(linewidth=.1)

# creating hexagon grid layer
bcn_hexa_grid <- st_make_grid(
  AIRBNB_BCN,
  c(.0025, .0025), # size of the grids, big numbers are big hexagons
  what = "polygons",
  square = FALSE # set that we don't have squares, but hexagons
)

bcn_hexa_grid_sf <- st_sf(bcn_hexa_grid) |>
  # add grid ID
  mutate(grid_id = 1:length(lengths(bcn_hexa_grid)))

ggplot(data = bcn_hexa_grid_sf) +
  geom_sf(linewidth=.1)


#### 1.3: Intersect layers (clip) ######

# we want to keep only those hexagons that are inside of the boundary of BCN
bcn_hexa <- st_intersection(bcn_CITY,bcn_hexa_grid_sf) # order matter here!

ggplot(data = bcn_hexa) +
  geom_sf(linewidth=.1)

#### 1.4: Hexabin Map ######

bcn_hexa$pt_count <- lengths(st_intersects(bcn_hexa, AIRBNB_BCN))

#myColors <- c("#BFBFBF", rev(brewer.pal(7, "Spectral")))

bcn_hexa$pt_count<-ifelse(bcn_hexa$pt_count==0,NA,bcn_hexa$pt_count)

MAP4 <- ggplot(bcn_hexa,
               aes(fill = pt_count)) +
  geom_sf(colour = "black",linewidth=.025)+
  scale_fill_distiller(palette = "Spectral",
                       name="Airbnb offers",
                       na.value = "#EBEBEB") + # it allows a NA value being attribute for a color
  labs(x="\nLongitude",
       y="Latitude\n",
       title="Foreign-born population by municipalities",
       subtitle="Barcelona 2022",
       caption="Elaboration: BSSD\nData: Population Register (INE)")+
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

MAP4

ggsave(file.path(images,"2_AIRBNB_HEXA.png"),
       scale = 1,
       height = 12,
       width=20,
       dpi = 300)


#### 1.5: Hexabin Map using a map tile ######

MAP4b <- BCN_TILE+
  geom_sf(data=bcn_hexa,
          aes(fill = pt_count),
          colour = "black",
          size=.01,
          inherit.aes = FALSE)+
  scale_fill_distiller(palette = "Spectral",
                       name="Airbnb offers",
                       na.value = "#EBEBEB")+
  labs(x="\nLongitude",
       y="Latitude\n",
       title="AIRBNB Accommodations",
       subtitle="Barcelona 2022",
       caption="Elaboration: BSSD\nData: Population Register (INE)")+
  theme_HS+
  theme(legend.position="bottom",
        legend.key.width=unit(4.5,"cm"))

MAP4b

ggsave(file.path(images,"3_AIRBNB_HEXA_tile.png"),
       scale = 1,
       height = 12,
       width=20,
       dpi = 300)

## 2. Bubbles maps #########

load("population_register_catalonia_2020.Rdata")

countries<-c(407,110) # 407: china, 110 France

df_mun <- pc2020cat |>
  filter(PAISNAC%in%countries) |>
  group_by(MUNICIPIO,PAISNAC) |>
  summarise(pop=n()) |>
  ungroup() |>
  mutate(prop=pop/sum(pop)*100,.by=c(PAISNAC),
         PAISNAC2 = case_when(PAISNAC == 407 ~ "CHINA",
                              PAISNAC == 110 ~ "FRANCE"))


df_mun <- df_mun |>
  mutate(prop_cat=as.factor(
    ifelse(prop<.5, "<0.5%",
           ifelse(prop<=1, "(0.5-1%]",
                  ifelse(prop<=3, "(1-3%]",
                         ifelse(prop<=5, "(3-5%]",
                                ifelse(prop<=10, "(5-10%]",
                                       ">10%")))))),
    prop_cat=fct_relevel(prop_cat,
                         "<0.5%",
                         "(0.5-1%]",
                         "(1-3%]",
                         "(3-5%]",
                         "(5-10%]",
                         ">10%"))

mysizes <-rev(c(8,5,3.5,2,1,.5,.1))
names(mysizes) <- levels(df_mun$prop_cat)

load("DATA_SPAIN.Rdata")
head(DATOS)

dt<-DATOS|>
  filter(YEAR==2016,COM=="Catalonia")|>select(1:3,13:15)

colnames(df_mun)[1]<-"CODMUN"

df_mun <- df_mun |>
  left_join(dt,by ="CODMUN")

# Force the df to become a sf object
df_mun_sf <- st_as_sf(df_mun,
                      coords=c("LON", "LAT"),
                      crs=4326)

# some other libraries
library(tidyterra)
library(slippymath)
library(mapSpain)
# note osm for getting bbox

Basemap <- esp_getTiles(df_mun_sf,
                        "IGNBase",
                        bbox_expand = 0.1,
                        zoom =10)


ggplot() +
  # function to plot raster images
  geom_spatraster_rgb(data = Basemap, maxcell = 10e6)+
  # tricky way of putting bubbles with bold contours
  geom_sf(data=df_mun_sf,aes(size = prop_cat,color=PAISNAC), shape =21)+
  geom_sf(data=df_mun_sf,aes(size = prop_cat,color=PAISNAC), #shape =21,
          alpha=.2)+
  scale_fill_brewer(palette="Set1")+
  scale_color_brewer(palette="Set1")+
  scale_size_manual(values=mysizes*8,name = "Population",
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keywidth=5,
                                         label.position = "bottom"))+
  guides(colour = "none")+
  facet_wrap(~PAISNAC2)+
  labs(title="\nPopulation distribution (%) by municipalities",
       subtitle = "Catalonia 2020",
       caption="\nElaboration: Juan Galeano for the Barcelona Summer School of Demography\n\nData: INE (https://missingmigrants.iom.int/downloads)\n")+
  theme_void()+
  theme(plot.title = element_text(lineheight=1, size=24,color  =  "#bdbdbd", face="bold", hjust = 0.5),
        plot.subtitle = element_text(lineheight=1, size=15,color  =  "#bdbdbd", face="bold", hjust = 0.5),
        plot.caption = element_text(vjust=0.5, size=12,colour="#bdbdbd", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size=15,color  =  "#bdbdbd", face="bold"),
        legend.title = element_text(size=15,color  =  "#bdbdbd", face="bold"),
        panel.background = element_rect(fill = "white", color  =  "white"),
        plot.background = element_rect(fill ="white", color  = "white"),
        panel.grid.major = element_line(color = "white",
                                        linewidth = 0.15,
                                        linetype = 2))

ggsave(file.path(images,"4_BUBBLES_MAP.png"),
       scale = 1,
       height = 12,
       width=20,
       dpi = 300)

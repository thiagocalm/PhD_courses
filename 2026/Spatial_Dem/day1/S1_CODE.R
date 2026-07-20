
### Exercise 1: abortion status by states in the US, 2023
# setwd(file.path("2026","Spatial_Dem","day1"))

shapes<- file.path("shps")
images<-file.path("IMAGES_class")

library(sf) # this is the spatial library for spatial analysis
library(tidyverse)

# read shape object to R
eeuu <- read_sf(dsn = shapes, "gadm41_USA_3")

# Explore your sf object

# Get the projection:
st_crs(eeuu) # we have the EPSG 4326 (Mercator projection)

# Get the coordinates of the bounding box:
st_bbox(eeuu)

# Info about type of variables
eeuu |> sapply(class)

# SIMPLIFY GEOMETRIES - the more detailed we have the shp, the heavier the shp is.
object.size(eeuu)
eeuu_simply <- st_simplify(eeuu, preserveTopology = TRUE, dTolerance = 1000)
object.size(eeuu_simply)
object.size(eeuu_simply)/object.size(eeuu)*100

# plot your sf object
ggplot()+
  geom_sf(data=eeuu_simply)

#### 2. Create new sf objects with the polygons to resize and move
# We can operate sf objects in R similarly to a normal dataframe

alaska<-eeuu_simply|>
  filter(NAME_1 =="Alaska")

ggplot()+
  geom_sf(data=alaska)

hawaii<-eeuu_simply|>
  filter(NAME_1 =="Hawaii")

ggplot()+
  geom_sf(data=hawaii)

#### 3. Create a new sf object of the US without the polygons to resize and move.

eeuu1 <- eeuu_simply |>
  filter(NAME_1 !="Alaska",NAME_1 !="Hawaii")

ggplot()+
  geom_sf(data=eeuu1)

#### 4. Resize and move polygons

# extract geometries
alaska_g<-st_geometry(alaska)
hawaii_g<-st_geometry(hawaii)

alaska_g<-alaska_g  * .4 # make alaska to be 40% of its original size

ggplot() +
  geom_sf(data=alaska_g)

alaska_g<- alaska_g+c(-60,0) # change the position of alaska
ggplot() +
  geom_sf(data=alaska_g)


st_crs(alaska_g) #check coordinate reference system

# setting a CRS to alaska forcing it to be a sf with the same srs as before
alaska_g <- st_as_sf(alaska_g, crs=st_crs(eeuu1)) # set CRS

# doing similarly to Hawaii
ggplot() +
  geom_sf(data=hawaii_g)

hawaii_g<- hawaii_g+ c(50,5)
st_crs(hawaii_g)
hawaii_g <- st_as_sf(hawaii_g, crs=st_crs(eeuu1))

ggplot() +
  geom_sf(data=eeuu1)+
  geom_sf(data=alaska_g)+
  geom_sf(data=hawaii_g)

#### 5. Create a new sf object with the new resized and moved polygons

# change name to geometry column
st_geometry(alaska_g) <- "geometry"
st_geometry(hawaii_g) <- "geometry"

# drop geometry column
df_alaska <- st_drop_geometry(alaska)
df_hawaii <- st_drop_geometry(hawaii)

# bind new geometries
new_alaska<-bind_cols(alaska_g,df_alaska)
new_hawaii<-bind_cols(hawaii_g,df_hawaii)

america_final<-rbind(eeuu1,new_alaska,new_hawaii)

ggplot() +
  geom_sf(data=america_final)

#### 6. Save the new sf object as a shapefile

america_final <- america_final |>
  select(NAME_1,VARNAME_1)

st_write(america_final, "america_final.shp") # save this object

#### 7. Create a vector with the abortion status for each state

abor<-c("Legal with\nnew protections",# vermont
        "Legal with\nnew protections", #minnesota
        "Full ban", # tenesse
        "Full ban", #Missisippi
        "15-20 weeks", #arizona
        "Legal with\nnew protections",#illinois
        "Legal with\nnew protections",#colorado
        "Full ban", #oklahoma
        "Legal with\nnew protections",#Maine
        "Full ban", #missuri
        "12 weeks",#Nebraska
        "Ban blocked", # indiana
        "Legal with\nnew protections",#Oregon
        "Full ban",# kentucky
        "Legal with\nnew protections",#District of columbia
        "Legal with\nnew protections",#california
        "Legal with\nnew protections",#Maryland
        "Legal with\nnew protections",#Delaware
        "Legal with\nnew protections",#washington
        "Full ban",# West virginia
        "Legal",#Kansas
        "Ban blocked", # Wyoming
        "Full ban",# Alabama
        "Legal with\nnew protections",#NY
        "Legal with\nnew protections",#Michigan
        "Full ban",# South dakota
        "Ban blocked", # Ohio
        "Ban blocked", # South carolina
        "Full ban",# Louisnba
        "Legal with\nnew protections",#Connecticut
        "Legal with\nnew protections",#New mexico
        "15-20 weeks",# North carolina
        "Full ban",# Arkansa
        "Legal",#Iowa
        "15-20 weeks",# Utha
        "Full ban",# North dakota
        "Legal", #virginia
        "Full ban",# texas
        "Legal", #New ham
        "Legal with\nnew protections",#Pennsylvania
        "Full ban",# wisconsin
        "Legal with\nnew protections",#Rodhe island
        "Legal with\nnew protections",#New Jersey
        "Legal with\nnew protections",#Nevada
        "Six weeks", #Georgia
        "Legal with\nnew protections",#Massachuset
        "Full ban",# Idaho
        "Ban blocked",# Montana
        "15-20 weeks",# Florida
        "Legal",#Alaska
        "Legal with\nnew protections")#Hawaii

#### 8. Very basic data manipulation

# Create a new variable with the 2 digits name of each state (name_short).
# Create a new variable (abortion) by adding the data on abortion status (abor).
# Create a new variable (general) classifying the 8 abortion status into 3 categories:
#  Legal, Banned and Ban blocked.
# Re-level the factors of variables abortion and general.

america_final <- america_final |>
  mutate(name_short=str_sub(VARNAME_1,1,2),
         abortion=abor,
         general= case_when(abortion=="Legal with\nnew protections"~"Legal",
                            abortion=="Legal"~"Legal",
                            abortion=="Full ban"~"Banned",
                            abortion=="Six weeks"~"Banned",
                            abortion=="12 weeks"~"Banned",
                            abortion=="15-20 weeks"~"Banned",
                            abortion=="Ban blocked"~"Ban blocked"),
         abortion=fct_relevel(abortion,"Legal with\nnew protections",
                              "Legal",
                              "Ban blocked",
                              "Full ban",
                              "Six weeks",
                              "12 weeks",
                              "15-20 weeks"),
         general=fct_relevel(general,"Legal",
                             "Ban blocked",
                             "Banned"))


# Correct the the 3 Common... cases.
america_final[14,4]<-"KY"
america_final[40,4]<-"PA"
america_final[46,4]<-"MA"

#### 9. Create a color palette and a theme for plotting

colors<-c("#70A096","#B7CDC9",
          "#F7D358",
          "#8A0001","#AC422E","#CA745E","#DEA896")


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

#### 10. Extract the centroids of polygons and plot a discrete map

centroid_states<- st_centroid(america_final) # extract the centroids of the polygons
america_final <- cbind(america_final, st_coordinates(st_centroid(centroid_states$geometry)))

general_map <- ggplot(america_final,
                    aes(fill = abortion)) +
  geom_sf(colour = "white",linewidth=.025) +
  scale_fill_manual(name="NA",
                    values=colors,
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keywidth=7,
                                         label.position = "bottom"))+
  geom_text(aes(x=X, y=Y, label=name_short),
            color = "white",
            fontface = "bold",
            size=2.5,
            check_overlap = TRUE)+
  labs(title="Abortion status by states",
       subtitle="United States 2023",
       caption="Elaboration: BSSD\nData: New York Times")+
  theme_S1

general_map

ggsave(file.path(images,"1_Abortion_categorical_map.png"),
       scale = 1,
       height = 8,
       width=12,
       dpi = 300)


#### 11. Plot a facet discrete map

basemap <- america_final
basemap <- basemap |> select(-general)


facet <- ggplot(america_final) +
  geom_sf(data=basemap,
          fill = "#E8E4E6",
          colour = "white",size=.025)+
  geom_sf(aes(fill = abortion),colour = "white",size=.025) +
  facet_wrap(~general)+
  scale_fill_manual(name="NA",
                    values=colors,
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keywidth=7,
                                         label.position = "bottom"))+

  labs(title="",
       subtitle="",
       caption="Elaboration: BSSD\nData: New York Times")+
  theme_S1

facet

ggsave(file.path(images,"2_Abortion_categorical_map_facet.png"),
       scale = 1,
       height = 8,
       width=12,
       dpi = 300)

#### 12. Composed image discrete map

# create an updated version of general removing some elements from the theme
general_update <- general_map+theme(legend.position = "none",
                                  plot.caption =element_blank())


library(gridExtra)
library(cowplot)

g <- grid.arrange(general_update, facet, nrow = 2)

ggdraw(g) + # Set up a drawing layer on top of a ggplot
  theme(plot.background = element_rect(fill="#FFFFFF", color = NA))

ggsave(file.path(images,"3_Abortion_categorical_map_composition.png"),
       scale = 1,
       height = 8,
       width=12,
       dpi = 300)

### Exercise 2: Rivers of Spain

esp  <- read_sf(dsn = shapes, "ESP_adm2")

esp <- esp |> filter(NAME_1!="Islas Canarias")

ggplot(esp) +
  geom_sf()+
  theme_bw()

# moving to a upper administrative level from provinces to regions - dissolving boundaries
esp_regions <- esp  |>
  group_by(ID_1) |>
  summarise(NAME_prov = unique(NAME_1))  |>
  # st_buffer(0.5) |>
  st_cast()

# note: the st_buffer function helps us cleaning sliver, however it can create some misalignment between layers.

ggplot(esp_regions) +
  geom_sf()+
  theme_bw()

# Let's get the bbox of our map to filter in the shape of European rivers
st_bbox(esp_regions)

bbox_wkt <- "POLYGON((
  -9.301806  35.170582,
  -9.301806  43.791527,
   4.328195  43.791527,
   4.328195  35.170582,
  -9.301806  35.170582
))"


country_rivers  <- read_sf(dsn = shapes, "HydroRIVERS_v10_eu",
                           wkt_filter = bbox_wkt)

ggplot()+
  geom_sf(data=esp_regions)+
  geom_sf(data=country_rivers)+
  theme_bw()

country_rivers <- country_rivers |>
  # setting the width of different lines of the order flow categories
  mutate(ORD_FLOW=as.factor(ORD_FLOW))

mysizes <-c(.1,
            .075,
            .05,
            .025,
            .012,
            .007)

ggplot()+
  geom_sf(data=esp_regions,linewidth=1)+
  geom_sf(data=country_rivers,aes(linewidth=ORD_FLOW))+
  scale_linewidth_manual(values=mysizes*15)+
  theme_void()+
  theme(legend.position = "none")

ggsave(file.path(images,"Rivers_1.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =25, #25  #10
       width = 24)

### intersection of layers
# Alternative 1 - we have a st_intersect(), which take a bit longer to do that
#rivers_esp<- st_intersection(esp_regions,country_rivers)
#rivers_esp  <- read_sf(dsn = shapes, "spain_rivers")

library(terra)

# Convert to SpatVector
esp_vect <- vect(esp_regions)
river_vect <- vect(country_rivers)

# Perform intersection to combine layers
rivers_esp_vect <- intersect(river_vect, esp_vect)
rivers_esp <- st_as_sf(rivers_esp_vect)


rivers_esp <- rivers_esp |>
  mutate(ORD_FLOW=as.factor(ORD_FLOW))

ggplot() +
  geom_sf(data=rivers_esp,aes(linewidth=ORD_FLOW,
                              color=NAME_prov))+
  scale_linewidth_manual(values=mysizes*30)+
  geom_sf(data=esp_regions, fill=NA,colour="#fafafa", linewidth=.5)+
  theme_void()+
  theme(legend.position = "none",
        plot.background =  element_rect(fill = "black"))

ggsave(file.path(images,"Rivers_2.png"), # name of the file of the image
       scale = 1,
       dpi = 300,
       height =25, #25  #10
       width = 24)


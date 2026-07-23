setwd(file.path("2026","Spatial_Dem","day4"))
path <- file.path("shps_data")
images <- file.path("images")

## 0. Load necessary libraries
options(scipen = 999)

library(sf)
library(tidyverse)
library(ggmap)
library(RColorBrewer)
library(Hmisc)
library(spdep)
library(sfdep)
library(biscale)

## 1. Bivariate maps


load(file.path(path,"Spain_bivarite.Rda"))

# 1 Create a function to compute the diversity index #######
compute_diversity_indices <- function(group_sizes) {
  # Ensure group sizes are positive
  if (any(group_sizes < 0)) {
    stop("Group sizes must be non-negative")
  }
  # Total population size
  total_size <- sum(group_sizes)

  # Proportions of each group
  p <- group_sizes / total_size
  # Maximum possible entropy (for normalization)
  K <- length(group_sizes)  # Number of groups

  # Simpson's diversity index
D <- sum(p^2)
Simpson_evenness <- (1 / D) / K # Adjusted for evenness
Simpson_evenness

}

# 2 Group data by municipalities and place of birth ######
df <- df |>
  mutate(
    mun= substr(id,1,5)
  ) |>
  group_by(mun,cont) |>
  summarise(pop=sum(pop)) |>
  ungroup()

# 3 We must be sure that all population groups have a row in the data frame in all municipalities ####
# If they don't have we create one and fill it with 0

df_a <- df %>%
  distinct(mun) %>%
  crossing(cont = 1:7) %>%
  left_join(df, by = c("mun", "cont"))|>
  mutate(pop = replace_na(pop, 0))

# 4 Create a new variable (born) to differentiate Spaniards from foreigners, #####
# Compute the share of foreigners and Spaniards in each municipality (mun)

df_b <- df_a |>
  mutate(
    born = case_when(cont == 1 ~ 1, TRUE ~ 9)
  ) |>
  group_by(mun, born,cont) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  # rename(id = 1)|>
  group_by(mun, born) |>
  mutate(pop_m = sum(pop)) |>
  ungroup() |>
  group_by(mun) |>
  mutate(total = sum(unique(pop_m))) |>
  ungroup() |>
  mutate(share_id = pop_m / total)

# 5 Create a data frame only with the counts of the different foreign groups #####
# And compute the diversity index at the municipal level

df_diversity <- df_b |>
  filter(born!=1)|>
  select(mun,cont, pop)|>
  group_by(mun) |>
  summarise(div_id = compute_diversity_indices(pop)) |>
  ungroup()

# replace NaN values by 0
df_diversity$div_id[is.nan(df_diversity$div_id)] <- 0

# 6 Create a data frame with share of foreign-born pop by municipalities #####
df_share <- df_b %>%
  filter(born==9)%>%
  distinct(mun,share_id)

# 7 Put both indicators (share and diversity) in the same dataframe ######

df_final <- df_share |>
  left_join(df_diversity, by = "mun")

# 8 Define quantile breaks for both share and diversity index #######
div_breaks <- quantile(df_final$div_id, probs = seq(0, 1, by = 1/3), na.rm = TRUE)
share_breaks <- quantile(df_final$share_id, probs = seq(0, 1, by = 1/3), na.rm = TRUE)

# 9 Assign terciles groups using cut ######
df_final <- df_final |>
  mutate(
    diversity_terciles = cut(div_id,
                             breaks = div_breaks,
                             labels = FALSE,
                             include.lowest = TRUE),
    share_terciles = cut(share_id,
                         breaks = share_breaks,
                         labels = FALSE,
                         include.lowest = TRUE)
  )


# 10 read a shapefile of spain by municipalities ######
shp_mun <- read_sf(dsn = path,"recintos_municipales_inspire_peninbal_etrs89")

# 11 Create a municipality id (mun) in the sf object and left join the statistical data
shp_mun <- shp_mun |>
  mutate(mun=substr(NATCODE,7,11)) |>
  select(mun) |>
  left_join(df_final, by="mun")

# 12 The biscale library ######
# We can split the categories into 3 or 4 dimensions

library(biscale)
bi_pal("GrPink", dim = 3, preview = TRUE, flip_axes = FALSE, rotate_pal = FALSE)
bi_pal("GrPink", dim = 3, preview = FALSE, flip_axes = FALSE, rotate_pal = FALSE) # to get the HEX codes

# 13. Choropleth map of share and diversity and Bivariate map ######

data <- bi_class(shp_mun, x = share_id, y = div_id, style = "quantile", dim = 3)

map_share <- ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = as.factor(share_terciles)),
          color = "#bdbdbd",
          linewidth = 0.1) +
  scale_fill_manual(name="Terciles",
                    values=c("#e8e8e8","#e4acac","#c85a5a"))+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("map_share.png", # name of the file of the image
       plot = map_share, #comentarle al chaval que me equivoque el nombre del argumento
       scale = 1,
       dpi = 300,
       height =12, #25  #10
       width = 12)

map_share

map_div<-ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = as.factor(diversity_terciles)),
          color = "#bdbdbd",
          linewidth = 0.1,)+
  scale_fill_manual(values=c("#e8e8e8","#b0d5df","#64acbe"))+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("map_div.png", # name of the file of the image
       plot = map_div,
       scale = 1,
       dpi = 300,
       height =12, #25  #10
       width = 12)

map_div

map_bivariate<-ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = bi_class),
          color = "#bdbdbd",
          linewidth = 0.1,
          show.legend = FALSE,
          inherit.aes = FALSE) +
  # we have to use this argument to work with bivariate maps
  bi_scale_fill(pal = "GrPink", dim = 3) +
  theme_bw() +
  theme(axis.title = element_blank())

# create the legends adhoc
legend <- bi_legend(pal = "GrPink",
                    pad_color=NA,
                    dim = 3,
                    xlab = "Higher % foreign ",
                    ylab = "Higher Diversity ",
                    size=10)

library(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map_bivariate, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, .15, .2, .2)

ggsave("map_bivariate.png", # name of the file of the image
       plot = finalPlot,
       scale = 1,
       dpi = 300,
       height =12, #25  #10
       width = 12)

finalPlot


### 2. Location quotients


load(file.path(path,"bcn_2020.Rdata")) # read data into R
names(BCN2020) # get columns names

BCN2020_f <- BCN2020 %>% # select necessary variables
  select(SECCIO, LA:AS, total)

# create a data frame in long format

BCN2020_long <- BCN2020_f |>
  pivot_longer(cols=c(LA:AS), names_to = "group")

# Create a function for computing LQs
LQ <- function(x,y) {
  result <- (x/y)/(sum(x)/sum(y))
  result2 <- round(result , digits = 2)
  return(result2)
}

# Apply your lq function

BCN2020_long <- BCN2020_long |>
  mutate(lq = LQ(value,total), .by=group)

# Set same nice intervals using function cut and change name of column 1 to ID

BCN2020_long <- BCN2020_long |>
  mutate(LQCAT=cut2(lq,
                    c(0,0.2,0.4,0.6,0.8,1.2,
                      1.4,1.6,1.8,2,3,100)))

colnames(BCN2020_long)[1]<-"ID"

# Read shapefile of Catalonia by census tracts into R
SHPCAT2020 <- read_sf(dsn = path,"bseccenv10sh1f1_20200101_0")

# Create ids for different administrative levels
bcnshp <- SHPCAT2020 |>
  mutate(PROV=str_sub(MUNICIPI, 1,2),
         MUN =str_sub(MUNICIPI, 1,5),
         ID = paste(PROV,str_sub(MUNICIPI, 3,5),DISTRICTE, SECCIO, sep="-")) |>
  filter(MUN=="08019")

colnames(bcnshp)
st_crs(bcnshp)
bcnshp <- st_transform(bcnshp, 4326)

# Join statistical and spatial data for each group
df_sf_final <- bcnshp %>%
  left_join(BCN2020_long,by ="ID")

# Coerce variable group to be a factor, recode factors and Relevel levels of factor
df_sf_final <- df_sf_final |>
  mutate(group=recode_factor(group, "LA"="Latin-America",
                             "EE"="Eastern-Europe",
                             "AF"="Africa",
                             "WE"="Western-Europe",
                             "AS"="Asia"),
         group=fct_relevel(group,"Latin-America",
                           "Western-Europe",
                           "Eastern-Europe",
                           "Africa",
                           "Asia"))


levels(df_sf_final$group) # Check levels

# Create a color palette for plotting
colorsPaleta <- c(rev(brewer.pal(11, "Spectral")))
colorsPaleta

# Get a map tile for using it as a background map
bbox<-st_bbox(bcnshp)

left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*1+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*1+mean(as.numeric(bbox[c(2,4)]))

RASTERCATGOOGLE <- ggmap(get_stadiamap(bbox = c(left =left,
                                                bottom = bottom,
                                                right = right,
                                                top = top),
                                       zoom = 14,
                                       maptype = c("stamen_terrain"),
                                       crop = TRUE, messaging = FALSE))

# Read shapefile with neighborhood delimitation of Barcelona
BCNBARRIS <- st_read(dsn = path, "BCN_Barri_ETRS89_SHP")
# Change the CRS
BCNBARRIS<-st_transform(BCNBARRIS,4326)

# Create your facet map and save it as an image.
LQs <-RASTERCATGOOGLE +
  geom_sf(data = df_sf_final,
          colour = "grey",
          size=.15,
          aes(fill = LQCAT),
          alpha = 0.9,
          inherit.aes = FALSE)+
  geom_sf(data = BCNBARRIS,
          fill=NA,
          colour="black",
          size=.25,
          inherit.aes = FALSE)+
  scale_fill_manual(name="LQ",
                    values=colorsPaleta)+
  facet_wrap(~ group, ncol = 3)+
  labs(x="",
       y="",
       title="Location Quotients",
       subtitle="Barcelona by census tracts 2020\n",
       caption="Data: Population Register (INE)\nElaboration: BSSD\nBasemap: http://maps.stamen.com/")+
  theme_bw()


ggsave("1_lqs_bcn.png",
       scale = 1,
       height = 12,
       width=18,
       dpi = 300)

LQs


### 3. Creating neighborhood matrixes

#### 3.1 Get coordinates from reprojected Shapefile with LotLang-projection

coords.ll <-st_coordinates(st_centroid(bcnshp$geometry))# We need the coordinates to plot the different neighbors matrices

#### 3.2 Create neighbors weight matrices: Contiguity based

### Queen
nb.FOQ <- poly2nb(bcnshp, queen=TRUE) #create a queen matrix using function poly2nb
summary(nb.FOQ) # get a summary (3 regions (398,682,828 plotOrder) with just 2 links and 1 region (251 plotOrder) with 23 links)
nb.FOQ # call the nb object and get the average number of links (6.437323)
plot(st_geometry(bcnshp), border="grey") # plot the basemaps with borders being grey
plot(nb.FOQ, coords.ll, add=TRUE, col="red") # add the queen neighbors matrix

### Rook

nb.rk <- poly2nb(bcnshp, queen=F) #create a rook matrix using function poly2nb
summary(nb.rk)# get a summary
nb.rk# call the nb object and get the average number of links (5.321395 )
plot(st_geometry(bcnshp), border="grey") # plot the basemaps with borders being grey
plot(nb.rk, coords.ll, add=TRUE, col="blue") # add the rook neighbors matrix

#### 3.3 Differences between Queen and Rook

# plot the basemaps with borders being grey
plot(st_geometry(bcnshp), border="grey")
# add the fifferences between queen and rook matices using the setdiff.nb function
plot(setdiff.nb(nb.FOQ, nb.rk), coords.ll,add=TRUE,col="green")

#### 3.4 Nearest neighborhood weight matrix

#### 1 neighborhood

l.1NN <- knearneigh(coords.ll, k=1, longlat=T) #create a matrix with the indices of points belonging to the set of the k nearest neighbours of each other.
nb.1NN <- knn2nb(l.1NN, row.names=bcnshp$ID) # convert the matrix into a nb objet using function knn2nb
nb.1NN # call the nb object
plot(st_geometry(bcnshp), border="grey") # plot the basemaps with borders being grey
plot(knn2nb(l.1NN), coords.ll, add=T,col="red") # add the k nearest neighbors matrix

#### 5 neighborhood
l.5NN <- knearneigh(coords.ll, k=5, longlat=T)
nb.5NN <- knn2nb(l.5NN, row.names=bcnshp$ID)
nb.5NN
plot(st_geometry(bcnshp), border="grey")
plot(knn2nb(l.5NN), coords.ll, add=T,col="red")

#### 3.5 Distance based matrix (distance between centroids)

#### 0.25 km
d025km <- dnearneigh(coords.ll, 0, 0.25, row.names = bcnshp$ID, longlat=T) # create a distnace matrix using function dnearneigh (0,0.25 indicates the lower and upper distance bounds)
d025km # call the nb object,get the average number of links and the ID of polygons with no neighbors (under this distance definition)
plot(st_geometry(bcnshp), border="grey")  # plot the basemaps with borders being grey
plot(d025km, coords.ll, add=TRUE,col="red") # add the distance neighbors matrix

#### 0.5 km
d05km <- dnearneigh(coords.ll, 0, 0.5, row.names = bcnshp$ID, longlat=T)
d05km
plot(st_geometry(bcnshp), border="grey")
plot(d05km, coords.ll, add=TRUE,col="red")

#### 1 km
d1km <- dnearneigh(coords.ll, 0, 1, row.names = bcnshp$ID, longlat=T)
d1km
plot(st_geometry(bcnshp), border="grey")
plot(d1km, coords.ll, add=TRUE,col="red")

#### 3.6  Union between two different matrices

Ud05kmnb.1NN <- union.nb(d05km,nb.1NN) # set a union between to different matrices using function union.nb
Ud05kmnb.1NN # call the nb object and get the average number of links ( 16.1065  )
plot(st_geometry(bcnshp), border="grey") # plot the basemaps with borders being grey
plot(Ud05kmnb.1NN, coords.ll, add=TRUE,col="red") # add neighbors matrix


#### 3.7 Higher order matrices

#### Queen (2nd order)

nb.FOQ <- poly2nb(bcnshp, queen=TRUE) # First create a queen matrix of order 1
nb.SOQ <- nblag(nb.FOQ,2) # 2 is the lag, if you want 6th order neighbors you'd have nblag(nb,6)
nb.SOQ #call the nb object and get the average number of links ( 15.43827 )
plot(st_geometry(bcnshp), border="grey")# plot the basemaps with borders being grey
plot(nb.SOQ[[2]], coords.ll, add=TRUE, col="red", lty=2)# add neighbors matrix (nb.SOQ[[2]]=we are calling the 2nd element of the list nb.SOQ)

#### Rook (2nd order)

nb.rk <- poly2nb(bcnshp, queen=F)
nb.SOR <- nblag(nb.rk,2) # 2 is the lag, if you want 6th order neighbors you'd have nblag(nb,6)
nb.SOR
plot(st_geometry(bcnshp), border="grey")
plot(nb.SOR[[2]], coords.ll, add=TRUE, col="red", lty=2)


### 4. Local Moran's I

## Read shapfile with population data ######

BCN <- st_read(path, "bc")
head(BCN) # see head of data associated to shp
ggplot(BCN)+geom_sf()      # plot shape

st_crs(BCN) # check original projection
BCN <- st_transform(BCN, 4326) #change it to "+proj=longlat"

#### 4.1. Compute the relative distribution of each population group among census tracts#####

BCN <- BCN |>
  mutate(PROPLA=LA/sum(LA),
         PROPWE=WE/sum(WE),
         PROPEE=EE/sum(EE),
         PROPAF=AF/sum(AF),
         PROPAS=AS/sum(AS))

#### 4.2 Create a weighted matrix: Union #####

coords.ll <- st_coordinates(st_centroid(st_geometry(BCN)))

Ud05kmnb.1NN <- poly2nb(BCN, queen=TRUE)

listw <- nb2listw(Ud05kmnb.1NN)
listw

#### 4.3 Moran's Test ######

globalMoran_LA <- moran.test(BCN$PROPLA, listw)
globalMoran_LA

# High Spatial Autocorrelation (Clustering)
# The Moran I statistic (0.3004644789) is substantially higher than its expectation (-0.0009372071).
# The standard deviate (Z-score) of 17.588 indicates the value is nearly 17 standard deviations above expected.
# With p-value < 2.2e-16, the null hypothesis of spatial randomness is rejected.
# Conclusion: The variable 'PROPLA' displays statistically significant positive spatial autocorrelation (similar values cluster together geographically across Barcelona).

globalMoran_WE <- moran.test(BCN$PROPWE, listw)
globalMoran_WE
globalMoran_EE <- moran.test(BCN$PROPEE, listw)
globalMoran_EE
globalMoran_AF <- moran.test(BCN$PROPAF, listw)
globalMoran_AF
globalMoran_AS <- moran.test(BCN$PROPAS, listw)
globalMoran_AS

#### 4.4 Moran Plot ######

moran_WE <- moran.plot(BCN$PROPWE, listw = nb2listw(Ud05kmnb.1NN, style = "W"))

#### 4.5 Local Moran's #######

local_LA <- localmoran(x = BCN$PROPLA, listw = nb2listw(Ud05kmnb.1NN, style = "W"))
local_WE <- localmoran(x = BCN$PROPWE, listw = nb2listw(Ud05kmnb.1NN, style = "W"))
local_EE <- localmoran(x = BCN$PROPEE, listw = nb2listw(Ud05kmnb.1NN, style = "W"))
local_AF <- localmoran(x = BCN$PROPAF, listw = nb2listw(Ud05kmnb.1NN, style = "W"))
local_AS <- localmoran(x = BCN$PROPAS, listw = nb2listw(Ud05kmnb.1NN, style = "W"))

#### 4.6 Moran Map (plot using tmap library) #######

moran.map <- cbind(BCN, local_WE)

library(tmap)
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")

#### 4.7 Define quadrants/clusters ######

quadrant_LA <- vector(mode="numeric",length=nrow(local_LA))
quadrant_WE <- vector(mode="numeric",length=nrow(local_WE))
quadrant_EE <- vector(mode="numeric",length=nrow(local_EE))
quadrant_AF <- vector(mode="numeric",length=nrow(local_AF))
quadrant_AS <- vector(mode="numeric",length=nrow(local_AS))

# center the variable of interest around its mean
m.qualification_LA <- BCN$PROPLA - mean(BCN$PROPLA)
m.qualification_WE <- BCN$PROPWE - mean(BCN$PROPWE)
m.qualification_EE <- BCN$PROPEE - mean(BCN$PROPEE)
m.qualification_AF <- BCN$PROPAF - mean(BCN$PROPAF)
m.qualification_AS <- BCN$PROPAS - mean(BCN$PROPAS)

# center the local Moran's around the mean
m.local_LA <- local_LA[,1] - mean(local_LA[,1])
m.local_WE <- local_WE[,1] - mean(local_WE[,1])
m.local_EE <- local_EE[,1] - mean(local_EE[,1])
m.local_AF <- local_AF[,1] - mean(local_AF[,1])
m.local_AS <- local_AS[,1] - mean(local_AS[,1])

# significance threshold
signif <- 0.05
# build a data quadrant
quadrant_LA[m.qualification_LA >0 & m.local_LA>0] <- 4
quadrant_LA[m.qualification_LA <0 & m.local_LA<0] <- 1
quadrant_LA[m.qualification_LA <0 & m.local_LA>0] <- 2
quadrant_LA[m.qualification_LA >0 & m.local_LA<0] <- 3
quadrant_LA[local_LA[,5]>signif] <- 0

quadrant_WE[m.qualification_WE >0 & m.local_WE>0] <- 4
quadrant_WE[m.qualification_WE <0 & m.local_WE<0] <- 1
quadrant_WE[m.qualification_WE <0 & m.local_WE>0] <- 2
quadrant_WE[m.qualification_WE >0 & m.local_WE<0] <- 3
quadrant_WE[local_WE[,5]>signif] <- 0

quadrant_EE[m.qualification_EE >0 & m.local_EE>0] <- 4
quadrant_EE[m.qualification_EE <0 & m.local_EE<0] <- 1
quadrant_EE[m.qualification_EE <0 & m.local_EE>0] <- 2
quadrant_EE[m.qualification_EE >0 & m.local_EE<0] <- 3
quadrant_EE[local_EE[,5]>signif] <- 0

quadrant_AF[m.qualification_AF >0 & m.local_AF>0] <- 4
quadrant_AF[m.qualification_AF <0 & m.local_AF<0] <- 1
quadrant_AF[m.qualification_AF <0 & m.local_AF>0] <- 2
quadrant_AF[m.qualification_AF >0 & m.local_AF<0] <- 3
quadrant_AF[local_AF[,5]>signif] <- 0

quadrant_AS[m.qualification_AS >0 & m.local_AS>0] <- 4
quadrant_AS[m.qualification_AS <0 & m.local_AS<0] <- 1
quadrant_AS[m.qualification_AS <0 & m.local_AS>0] <- 2
quadrant_AS[m.qualification_AS >0 & m.local_AS<0] <- 3
quadrant_AS[local_AS[,5]>signif] <- 0

#### 4.8 Add results to the sf object ######

BCN$LMI_LA<-quadrant_LA
BCN$LMI_WE<-quadrant_WE
BCN$LMI_EE<-quadrant_EE
BCN$LMI_AF<-quadrant_AF
BCN$LMI_AS<-quadrant_AS

#### 4.8.1 Create a long format version of data. Define a color palette, order levels and plot a facet_map #####

BCNlonlatF1 <- BCN |>
  select(geometry,ID,LMI_LA, LMI_WE,LMI_EE, LMI_AF,LMI_AS) |>
  pivot_longer(cols=c(LMI_LA, LMI_WE,LMI_EE, LMI_AF,LMI_AS), names_to = "group") |>
  mutate(group=recode_factor(group, "LMI_LA"="Latin-America",
                             "LMI_EE"="Eastern-Europe",
                             "LMI_AF"="Africa",
                             "LMI_WE"="Western-Europe",
                             "LMI_AS"="Asia"),
         group=fct_relevel(group,"Latin-America",
                           "Western-Europe",
                           "Eastern-Europe",
                           "Africa",
                           "Asia"),
         LMI=case_when(value==0 ~ "Not significant",
                       value==1 ~ "Low-Low",
                       value==2 ~ "Low-High",
                       value==3 ~ "High-Low",
                       value==4 ~ "High-high"),
         LMI=fct_relevel(LMI,"Not significant",
                         "Low-Low",
                         "Low-High",
                         "High-Low",
                         "High-high"))

levels(BCNlonlatF1$group)
levels(BCNlonlatF1$LMI)

colorsPaleta <- c("#F2F2F2","blue","lightblue","pink","red" )

# Get a map tile for using it as a background map
bbox<-st_bbox(BCN)

left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*1+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*1+mean(as.numeric(bbox[c(2,4)]))

RASTERCATGOOGLE <- ggmap(get_stadiamap(bbox = c(left =left,
                                                bottom = bottom,
                                                right = right,
                                                top = top),
                                       zoom = 14,
                                       maptype = c("stamen_terrain"),
                                       crop = TRUE, messaging = FALSE))



LMI_MAP<-RASTERCATGOOGLE +
  geom_sf(data = BCNlonlatF1,
          colour = "black",
          size=.15,
          aes(fill = LMI),
          alpha = 0.9,
          inherit.aes = FALSE)+
  scale_fill_manual(name="Moran's I",
                    values=colorsPaleta,
                    labels=levels(BCNlonlatF1$LMI))+
  facet_wrap(~ group, ncol = 3)+
  labs(x="",
       y="",
       title="Local Moran's I for different foreign-born groups",
       subtitle="Barcelona by census tracts 2020\n",
       caption="Data: Populatdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCion Register (INE)\nElaboration: BSSD\nBasemap: http://maps.stamen.com/")+
  theme_bw()

LMI_MAP

ggsave("2_LISA_bcn.png",
       scale = 1,
       height = 12,
       width=18,
       dpi = 300)


lisa_results <- BCN %>%
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    lisa = local_moran(PROPLA, nb, wt)
  ) %>%
  unnest(lisa) %>%
  mutate(
    p_folded_sim = p_folded_sim,
    mean_val = PROPLA - mean(PROPLA, na.rm = TRUE),
    spatial_lag = st_lag(PROPLA, nb, wt),
    lag_mean_val = spatial_lag - mean(spatial_lag, na.rm = TRUE),
    quadrant = case_when(
      p_folded_sim >= 0.05 ~ "Not Significant",
      mean_val > 0 & lag_mean_val > 0 ~ "High-High",
      mean_val < 0 & lag_mean_val < 0 ~ "Low-Low",
      mean_val > 0 & lag_mean_val < 0 ~ "High-Low",
      mean_val < 0 & lag_mean_val > 0 ~ "Low-High",
      TRUE ~ "Not Significant"
    )
  )

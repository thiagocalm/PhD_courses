#'------------------------
#'CONSOLIDATION EXCERSICES
#'day4
#'------------------------
invisible(gc())
rm(list = ls())
# directory and packages --------------------------------------------------

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
library(cowplot)


# Auxiliarly functions ----------------------------------------------------

# Compute the diversity index #######
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

# 1. Bivariate map --------------------------------------------------------

# import statistic data

load(file.path(path, "Spain_bivarite.Rda"))

# group data by municipalities and place of birth ######
df <- df |>
  mutate(mun = substr(id,1,5)) |>
  filter(mun == "08019") |>
  group_by(id,cont) |>
  summarise(pop=sum(pop)) |>
  ungroup()

# we must be sure that all population groups have a row in the data frame in all municipalities ####
# If they don't have we create one and fill it with 0

df <- df %>%
  distinct(id) %>%
  crossing(cont = 1:7) %>%
  left_join(df, by = c("id", "cont"))|>
  mutate(pop = replace_na(pop, 0))

# create a new variable (born) to differentiate Spaniards from foreigners, #####
# Compute the share of foreigners and Spaniards in each municipality (mun)

df <- df |>
  mutate(
    born = case_when(cont == 1 ~ 1, TRUE ~ 9)
  ) |>
  group_by(id, born,cont) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  # rename(id = 1)|>
  group_by(id, born) |>
  mutate(pop_m = sum(pop)) |>
  ungroup() |>
  group_by(id) |>
  mutate(total = sum(unique(pop_m))) |>
  ungroup() |>
  mutate(share_id = pop_m / total)

# create a data frame only with the counts of the different foreign groups #####
# And compute the diversity index at the municipal level

df_diversity <- df |>
  filter(born!=1)|>
  select(id,cont, pop)|>
  group_by(id) |>
  summarise(div_id = compute_diversity_indices(pop)) |>
  ungroup()

# replace NaN values by 0
df_diversity$div_id[is.nan(df_diversity$div_id)] <- 0

# create a data frame with share of foreign-born pop by municipalities #####
df_share <- df %>%
  filter(born==9)%>%
  distinct(id,share_id)

# put both indicators (share and diversity) in the same dataframe ######

df_final <- df_share |>
  left_join(df_diversity, by = "id")

# define quantile breaks for both share and diversity index #######
div_breaks <- quantile(df_final$div_id, probs = seq(0, 1, by = 1/4), na.rm = TRUE)
share_breaks <- quantile(df_final$share_id, probs = seq(0, 1, by = 1/4), na.rm = TRUE)

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
shp_tracts <- read_sf(dsn = path,"SECC_CE_20220101")
st_crs(shp_tracts)
shp_tracts <- st_transform(shp_tracts, crs = 4326)

# 11 Create a municipality id (mun) in the sf object and left join the statistical data
shp_tracts <- shp_tracts |>
  filter(NMUN == "Barcelona") |>
  # filter(id %in% df_final$id) |>
  select(id) |>
  left_join(df_final, by="id")

# 13. Choropleth map of share and diversity and Bivariate map ######

data <- bi_class(shp_tracts, x = share_id, y = div_id, style = "quantile", dim = 4)

map_share <- ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = as.factor(share_terciles)),
          color = "#bdbdbd",
          linewidth = 0.1) +
  scale_fill_manual(name="Terciles",
                    values=c("#e8e8e8","#e4acac","#c85a5a","red4"))+
  theme_bw()+
  theme(legend.position = "bottom")

map_share

map_div<-ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = as.factor(diversity_terciles)),
          color = "#bdbdbd",
          linewidth = 0.1,)+
  scale_fill_manual(values=c("#e8e8e8","#b0d5df","#64acbe","#64acff"))+
  theme_bw()+
  theme(legend.position = "bottom")

map_div

### plot it

# get map for the background

# set boundaries
bbox <- st_bbox(data)
margin <- 1
left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*margin+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*margin+mean(as.numeric(bbox[c(2,4)]))

# Geom tiles

bcn_tile <- get_stadiamap(bbox = c(left = left,
                                   bottom = bottom,
                                   right = right,
                                   top = top ),
                          zoom = 14,
                          maptype = c("stamen_toner_lite"),
                          crop = TRUE,
                          messaging = FALSE) |>
  ggmap()


map_bivariate <- bcn_tile +
  geom_sf(data = data,
          mapping = aes(fill = bi_class),
          color = "#bdbdbd",
          linewidth = 0.1,
          show.legend = FALSE,
          inherit.aes = FALSE) +
  # we have to use this argument to work with bivariate maps
  bi_scale_fill(pal = "GrPink2", dim = 4) +
  theme_bw() +
  theme(axis.title = element_blank())

# create the legends adhoc
legend <- bi_legend(pal = "GrPink2",
                    pad_color=NA,
                    dim = 4,
                    xlab = "Higher % foreign ",
                    ylab = "Higher Diversity ",
                    size=9)

finalPlot <- ggdraw() +
  draw_plot(map_bivariate, 0, 0, 1, 1) +
  draw_plot(legend, 0.72, .08, .2, .2)

ggsave("Exercise1_DiversityOfForeignInBcn.png", # name of the file of the image
       plot = finalPlot,
       scale = 1,
       dpi = 300,
       height =12, #25  #10
       width = 12)

finalPlot

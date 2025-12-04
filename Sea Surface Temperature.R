###########################################################################################
##
## R source code - Temperature map (Sea Surface Temperature, Robinson)
##
##
###########################################################################################

#Package Loading --------------------------------------------------------
library(raster)
library(rasterVis)
library(rworldxtra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(extrafont)
library(RColorBrewer)

x11(width = 60, height = 40)


# Load masked SST file (already ocean-only)
temp_nc <- raster("sst1.nc")

# Define Robinson projection ----------------------------------------------
target_crs_robinson <- "ESRI:54030"

# Basemap ---------------------------------------------------------------
world_countries <- ne_countries(scale = 'medium', returnclass = 'sf')
world_oceans <- ne_download(scale = 'medium', type = 'ocean',
                            category = 'physical', returnclass = 'sf')
world_lakes <- ne_download(scale = "medium", type = "lakes",
                           category = "physical", returnclass = 'sf')

world_countries_robinson <- st_transform(world_countries, crs = target_crs_robinson)
world_oceans_robinson   <- st_transform(world_oceans,    crs = target_crs_robinson)
world_lakes_robinson    <- st_transform(world_lakes,     crs = target_crs_robinson)

# -----------------------------------------------------------------------
# Project masked SST raster to Robinson projection
# -----------------------------------------------------------------------
temp_robinson <- projectRaster(temp_nc, crs = target_crs_robinson, method = "ngb")

# Convert to dataframe for ggplot2 --------------------------------------
temp_df <- as.data.frame(temp_robinson, xy = TRUE, na.rm = TRUE)
names(temp_df) <- c("x", "y", "temp")

# Create graticules ------------------------------------------------------
graticules_robinson <- st_graticule(
  lat = seq(-90, 90,  by = 10),
  lon = seq(-180, 180, by = 10),
  crs = st_crs(4326)
) |> st_transform(crs = target_crs_robinson)

# Create degree labels ----------------------------------------------------
create_degree_labels <- function() {
  
  lon_breaks <- seq(-180, 180, by = 20)
  lon_labels <- ifelse(lon_breaks == 0, "0°",
                       ifelse(lon_breaks == 180, "180°",
                              ifelse(lon_breaks > 0, paste0(lon_breaks, "°E"),
                                     paste0(lon_breaks, "°W"))))
  
  lat_breaks <- seq(-80, 80, by = 10)
  lat_labels <- ifelse(lat_breaks == 0, "0°",
                       ifelse(lat_breaks > 0, paste0(lat_breaks, "°N"),
                              paste0(lat_breaks, "°S")))
  
  lon_points_top <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, 85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  lon_points_bottom <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, -85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  lat_points_left <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(-179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  lat_points_right <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  return(list(
    lon_labels_top    = st_sf(geometry = lon_points_top,    label = lon_labels),
    lon_labels_bottom = st_sf(geometry = lon_points_bottom, label = lon_labels),
    lat_labels_left   = st_sf(geometry = lat_points_left,   label = lat_labels),
    lat_labels_right  = st_sf(geometry = lat_points_right,  label = lat_labels)
  ))
}

degree_labels <- create_degree_labels()

# -----------------------------------------------------------------------
# 9-class legend (equal intervals)
# -----------------------------------------------------------------------
breaks_9 <- seq(min(temp_df$temp, na.rm = TRUE),
                max(temp_df$temp, na.rm = TRUE),
                length.out = 10)

# -----------------------------------------------------------------------
# Create final SST map
# -----------------------------------------------------------------------
ggplot() +
  
  # ocean temperature values (already masked)
  geom_raster(data = temp_df, aes(x = x, y = y, fill = temp)) +
  
  # lakes (light blue)
  geom_sf(data = world_lakes_robinson, fill = "#CAE1FF", color = NA, alpha = 0.7) +
  
  # land (grey)
  geom_sf(data = world_countries_robinson,
          fill = "grey80", color = "grey40", linewidth = 0.25) +
  
  # graticules
  geom_sf(data = graticules_robinson, color = "black", linewidth = 0.4, alpha = 0.4) +
  
  # degree labels
  geom_sf_text(data = degree_labels$lon_labels_top,    aes(label = label),
               color = "black", size = 2.8, family = "Times New Roman",
               fontface = "bold", nudge_y = 500000) +
  geom_sf_text(data = degree_labels$lon_labels_bottom, aes(label = label),
               color = "black", size = 2.8, family = "Times New Roman",
               fontface = "bold", nudge_y = -500000) +
  geom_sf_text(data = degree_labels$lat_labels_left,   aes(label = label),
               color = "black", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_x = -800000) +
  geom_sf_text(data = degree_labels$lat_labels_right,  aes(label = label),
               color = "black", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_x = 800000) +
  
  # color scale
  scale_fill_distiller(
    palette = "RdYlBu",
    name = "Sea Surface Temperature (K)",
    direction = -1,
    na.value = "transparent",
    breaks = breaks_9,
    labels = sprintf("%.2f", breaks_9)
  ) +
  
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 20,
    barheight = 1.5
  )) +
  
  ggtitle(paste("Sea Surface Temperature\n 2022-01-01")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  )

# Save -------------------------------------------------------------------
ggsave(paste0("SST", period, ".png"),
       width = 16, height = 10, dpi = 300, bg = "white")

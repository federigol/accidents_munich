#1. Load Packages -------------------------------------------------------

packages <- c("tidyverse", "plyr", "sf",
              "leaflet", "htmltools", "sp", "raster",
              "mgsub", "htmlwidgets")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}

library(tidyverse)
library(plyr)
library(sf)
library(leaflet)
library(raster)
library(sp)
library(htmltools)
library(mgsub)
library(htmlwidgets)


# 2. Import data --------------------------------------------

lkr <- st_read("RKI_Corona_Landkreise-shp/Landkreise.shp")
unfaelle_19a <- read.csv("unfaelle_2019.csv")
unfaelle_18a <- read.csv("unfaelle_2018.csv")
unfaelle_17a <- read.csv("unfaelle_2017.csv")
unfaelle_16a <- read.csv("unfaelle_2016.csv")

#Data Codebook and Source: https://opendata-esri-de.opendata.arcgis.com/datasets/esri-de-content::verkehrsunfälle-2019

muc <- subset(lkr, county == "SK München")
muc <- as_Spatial(muc[, "geometry"])
rm(lkr)

# 3. Filter and transform vars --------------------------------------------
#Filter vars
unfaelle_19 <- unfaelle_19a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_18 <- unfaelle_18a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_17 <- unfaelle_17a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_16 <- unfaelle_16a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]

#Transform vars
unfaelle_19 <- unfaelle_19 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig)) 
unfaelle_19 <- unfaelle_19[, !names(unfaelle_19) %in% c("IstGkfz")]
unfaelle_18 <- unfaelle_18 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig))
unfaelle_18 <- unfaelle_18[, !names(unfaelle_18) %in% c("IstGkfz")]
unfaelle_16 <- dplyr::rename(unfaelle_16, UTYP1 = UTYP)

#Join Dataframes
unfaelle <- rbind(unfaelle_19, unfaelle_18, unfaelle_17, unfaelle_16)
rm(unfaelle_16, unfaelle_16a, unfaelle_17, unfaelle_17a, unfaelle_18, unfaelle_18a, unfaelle_19, unfaelle_19a)

#Rename vars
unfaelle <- setNames(unfaelle, c("kategorie", "bundesland", "regierungsbezirk", "kreis", "jahr", "monat", "stunde", "wochentag", "unfallart", "unfalltyp", "fahrrad", "pkw", "fussgaenger", "kraftrad", "sonstig", "lon", "lat"))

#Change values
unfaelle$kategorie <- mapvalues(unfaelle$kategorie, from = c("1", "2", "3"), to = c("Tödlich", "Schwerverletzt", "Leichtverletzt"))


#Create values for types of accidents
unfaelle <- unfaelle %>% 
  mutate(nur_pkw = ifelse(pkw == 1 & fahrrad == 0 & fussgaenger ==0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(nur_rad = ifelse(pkw == 0 & fahrrad == 1 & fussgaenger ==0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(pkw_rad = ifelse(pkw == 1 & fahrrad == 1 & fussgaenger == 0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(pkw_fussgaenger = ifelse(pkw == 1 & fahrrad == 0 & fussgaenger == 1 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(rad_fussgaenger = ifelse(pkw == 0 & fahrrad == 1 & fussgaenger == 1 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(sonstig_2 = ifelse(nur_pkw == 0 & nur_rad == 0 & pkw_rad == 0 & pkw_fussgaenger == 0 & rad_fussgaenger == 0, 1, 0))

#Filter for accidents that happened in Munich
unfaelle <- unfaelle %>% 
  filter(bundesland == 9 & regierungsbezirk == 1 & kreis == 62) 

#Change CRS
unfaelle <- st_as_sf(unfaelle, coords = c("lon", "lat"))
st_crs(unfaelle) <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
unfaelle <- st_transform(unfaelle, proj4string(muc))

#Store lon and lat in columns
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

unfaelle <- sfc_as_cols(unfaelle)

#Create backup dataset and filter for serious accidents that happened in Munich
unfaelle_muc <- unfaelle
unfaelle <- unfaelle %>%
  filter(kategorie == "Tödlich" | kategorie == "Schwerverletzt")

# 4. Make raster and first map --------------------------------------------

#Make raster
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(unfaelle$lon, unfaelle$lat)
r <- rasterize(pos, r, fun="count")

#Make palettes
raster_pal <- colorNumeric("Reds", c(3, 7), na.color = "#00000000")
points_pal <- colorFactor(palette = c("#fcbf49", "#d62828"),
  levels = c("Schwerverletzt", "Tödlich"))

#Legend
colors <- c("#fcbf49", "#d62828", "#FFF5F0", "#FB6A4A", "#67000D")
labels <- c("Schwerverletzt", "Tödlich", "3 Unfälle", "5 Unfälle", "7 Unfälle")
sizes <- c(2, 2, 10, 10, 10)
shapes <- c("circle", "circle", "square", "square", "square")
borders <- c("#fcbf49", "#d62828", "#FFF5F0", "#FB6A4A", "#67000D")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", opacity = 0.7){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='float: right; display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, position = position, opacity = opacity))
}


#Make map
m1 <- leaflet(unfaelle) %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "markers", zIndex = 410) %>% 
  addMapPane(name = "labels", zIndex = 420) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(~lon, ~lat, 
                   radius= 2, 
                   color= ~points_pal(kategorie), 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "markers")) %>%
  addPolygons(data=muc,
              fillOpacity = 0, 
              weight = 1, 
              smoothFactor = 0.7, 
              stroke=TRUE,
              color = "white") %>%
  addRasterImage(r,
                 color = raster_pal,
                 opacity = 0.8,
                 project = FALSE) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = c(providerTileOptions(opacity = 0.9), 
                               leafletOptions(pane = "labels"))) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders) %>%
  addTiles(attribution = "© Statistische Ämter des Bundes und der Länder",
           options = providerTileOptions(opacity = 0))
saveWidget(m1, "unfaelle_muc1.html")
m1

# 5. Second Map ------------------------------------------------------
pkw_df <- filter(unfaelle, nur_pkw == 1)
rad_df <- filter(unfaelle, nur_rad == 1)
pkw_rad_df <- filter(unfaelle, pkw_rad == 1)
pkw_fussgaenger_df <- filter(unfaelle, pkw_fussgaenger == 1)
rad_fussgaenger_df <- filter(unfaelle, rad_fussgaenger == 1)
sonstig_df <- filter(unfaelle, sonstig_2 == 1)

colors <- c("#d62828", "#e9ff70", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")
labels <- c("Nur PKW", "Nur Fahrrad", "PKW und Fahrrad", "PKW und Fußgänger", "Fahrrad und Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle", "circle", "circle")
borders <- c("#d62828", "#e9ff70", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Beteiligte", opacity = 0.7){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='float: right; display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, position = position, title = title, opacity = opacity))
}


m2 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "nur_pkw", zIndex = 410) %>% 
  addMapPane(name = "nur_rad", zIndex = 420) %>%
  addMapPane(name = "pkw_rad", zIndex = 430) %>%
  addMapPane(name = "pkw_fussgaenger", zIndex = 440) %>%
  addMapPane(name = "rad_fussgaenger", zIndex = 450) %>%
  addMapPane(name = "sonstige", zIndex = 460) %>%
  addMapPane(name = "labels", zIndex = 470) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_df$lon, pkw_df$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_pkw"),
                   group = "PKWs") %>%
  addCircleMarkers(rad_df$lon, rad_df$lat, 
                   radius= 2, 
                   color= "#e9ff70", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_rad"),
                   group = "Fahrräder") %>%
  addCircleMarkers(pkw_rad_df$lon, pkw_rad_df$lat, 
                   radius= 2, 
                   color= "#fe7f2d", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_rad"),
                   group = "PKWs und Fahrräder") %>%
  addCircleMarkers(pkw_fussgaenger_df$lon, pkw_fussgaenger_df$lat, 
                   radius= 2, 
                   color= "#16db93", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_fussgaenger"),
                   group = "PKWs und Fußgänger") %>%
  addCircleMarkers(rad_fussgaenger_df$lon, rad_fussgaenger_df$lat, 
                   radius= 2, 
                   color= "#7f5539", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "rad_fussgaenger"),
                   group = "Fahrräder und Fußgänger") %>%
  addCircleMarkers(sonstig_df$lon, sonstig_df$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "sonstige"),
                   group = "Sonstige") %>%
  addPolygons(data=muc,
              fillOpacity = 0, 
              weight = 1, 
              smoothFactor = 0.7, 
              stroke=TRUE,
              color = "white") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = c(providerTileOptions(opacity = 0.9), 
                               leafletOptions(pane = "labels"))) %>%
  addTiles(attribution = "© Statistische Ämter des Bundes und der Länder",
           options = providerTileOptions(opacity = 0)) %>%
  addLayersControl(overlayGroups = c("Nur PKWs", "Nur Fahrräder", "PKWs und Fahrräder", "PKWs und Fußgänger", "Fahrräder und Fußgänger", "Sonstige"),
                   position = "topright") %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

saveWidget(m2, "unfaelle_muc2.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)

# 6. Third Map
# Filter data
pkw <- unfaelle_muc %>%
  filter(nur_pkw==1)
pkw_leicht <- pkw %>%
  filter(kategorie == "Leichtverletzt")
pkw_schwer <- pkw %>%
  filter(kategorie == "Schwerverletzt")
pkw_toedlich <- pkw %>%
  filter(kategorie == "Tödlich")

#Make raster
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(pkw$lon, pkw$lat)
r <- rasterize(pos, r, fun="count")

#Make palettes
raster_pal <- colorNumeric("Reds", c(12, 20), na.color = "#00000000")

colors <- c("#808080", "#fcbf49", "#d62828")
labels <- c("Leichtverletzt", "Schwerverletzt", "Tödlich")
sizes <- c(2, 2, 2)
shapes <- c("circle", "circle", "circle")
borders <- c("#808080", "#fcbf49", "#d62828")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "PKWs only", opacity = 0.7){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='float: right; display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, position = position, title = title, opacity = opacity))
}


m3 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "Leichtverletzt", zIndex = 410) %>% 
  addMapPane(name = "Schwerverletzt", zIndex = 420) %>% 
  addMapPane(name = "Tödlich", zIndex = 430) %>% 
  addMapPane(name = "labels", zIndex = 440) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_leicht$lon, pkw_leicht$lat, 
                   radius= 2, 
                   color= "#808080", 
                   stroke = FALSE, 
                   fillOpacity = 0.4,
                   options = leafletOptions(pane = "Leichtverletzt"),
                   group = "PKWs") %>%
  addCircleMarkers(pkw_schwer$lon, pkw_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Schwerverletzt"),
                   group = "PKWs") %>%
  addCircleMarkers(pkw_toedlich$lon, pkw_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Tödlich"),
                   group = "PKWs") %>%
  addPolygons(data=muc,
              fillOpacity = 0, 
              weight = 1, 
              smoothFactor = 0.7, 
              stroke=TRUE,
              color = "white") %>%
  addRasterImage(r,
                 color = raster_pal,
                 opacity = 0.8,
                 project = FALSE) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = c(providerTileOptions(opacity = 0.9), 
                               leafletOptions(pane = "labels"))) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders) %>%
  addTiles(attribution = "© Statistische Ämter des Bundes und der Länder",
           options = providerTileOptions(opacity = 0))
saveWidget(m3, "unfaelle_muc3.html")
m3


# 10. Chloropleth of accidents per population ------------------------------
lkr <- st_read("RKI_Corona_Landkreise-shp/Landkreise.shp")
unfaelle_19a <- read.csv("unfaelle_2019.csv")
unfaelle_18a <- read.csv("unfaelle_2018.csv")
unfaelle_17a <- read.csv("unfaelle_2017.csv")
unfaelle_16a <- read.csv("unfaelle_2016.csv")

unfaelle_19 <- unfaelle_19a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UGEMEINDE", "LINREFX", "LINREFY")]
unfaelle_18 <- unfaelle_18a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UGEMEINDE", "LINREFX", "LINREFY")]
unfaelle_17 <- unfaelle_17a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UGEMEINDE", "LINREFX", "LINREFY")]
unfaelle_16 <- unfaelle_16a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UGEMEINDE", "LINREFX", "LINREFY")]

#Join Dataframes
unfaelle_deutschland <- rbind(unfaelle_19, unfaelle_18, unfaelle_17, unfaelle_16)

#Rename vars
unfaelle_deutschland <- setNames(unfaelle_deutschland, c("kategorie", "bundesland", "regierungsbezirk", "kreis", "gemeinde", "lon", "lat"))

#Filter for serious accidents 
unfaelle_deutschland <- unfaelle_deutschland %>% 
  filter(kategorie == 1 | kategorie == 2)

#Add crs
unfaelle_deutschland <- st_as_sf(unfaelle_deutschland, coords = c("lon", "lat"))
st_crs(unfaelle_deutschland) <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
unfaelle_deutschland <- st_transform(unfaelle_deutschland, crs(lkr))

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

unfaelle_deutschland <- sfc_as_cols(unfaelle_deutschland)

#Create var for accidents by population
lkr$pt_count <- lengths(st_intersects(lkr, unfaelle_deutschland))
lkr$unfaelle_pro_ew <- lkr$pt_count/lkr$EWZ*100000

#Calculate extra for Berlin and add to lkr
berlin <- filter(lkr, grepl("Berlin", county))
berlin$pt_count <- lengths(st_intersects(berlin, unfaelle_deutschland))
berlin_unfaelle_pro_ew <- sum(berlin$pt_count)/sum(berlin$EWZ)*100000
berlin_df <- data.frame(AGS = as.factor("11000"), county = "SK Berlin", unfaelle_pro_ew = berlin_unfaelle_pro_ew)
lkr <- st_set_geometry(lkr, NULL)
lkr <- lkr[, c("AGS", "county", "unfaelle_pro_ew")]
lkr <- rbind(lkr, berlin_df)
lkr <- lkr %>%
  filter(AGS != "NA")
write.csv(lkr, "unfaelle_lkr.csv")

pal <- colorNumeric("Blues", lkr$unfaelle_pro_ew, na.color = "#00000000")

leaflet() %>%
  addPolygons(data=lkr,
              fillOpacity = 0.5, 
              weight = 1, 
              smoothFactor = 0.7, 
              stroke=TRUE,
              color = "white",
              fillColor = pal(lkr$unfaelle_pro_ew),
              popup = lapply(lkr$unfaelle_pro_ew, HTML))



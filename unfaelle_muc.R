#1. Load Packages -------------------------------------------------------

packages <- c("tidyverse", "plyr", "sf",
              "leaflet", "htmltools", "sp", "raster",
              "mgsub", "htmlwidgets", "stringr", "rgdal")
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
library(stringr)
library(rgdal)


# 2. Import data --------------------------------------------

lkr <- st_read("RKI_Corona_Landkreise-shp/Landkreise.shp")
unfaelle_20a <- read.csv("unfaelle_2020.csv", sep = ";")
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
unfaelle_20 <- unfaelle_20a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "ULICHTVERH", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstige", "LINREFX", "LINREFY")]
unfaelle_19 <- unfaelle_19a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "ULICHTVERH", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_18 <- unfaelle_18a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "ULICHTVERH", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_17 <- unfaelle_17a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP1", "LICHT", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_16 <- unfaelle_16a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "UTYP", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]

#Transform vars
unfaelle_20 <- unfaelle_20 %>%
  mutate(IstSonstige = ifelse(IstSonstige == 0, IstGkfz, IstSonstige))
unfaelle_20 <- unfaelle_20[, !names(unfaelle_20) %in% c("IstGkfz")]
unfaelle_20 <- dplyr::rename(unfaelle_20, IstSonstig = IstSonstige)
unfaelle_20$LINREFX <- as.character(unfaelle_20$LINREFX)
unfaelle_20$LINREFX <- sub(",", ".", unfaelle_20$LINREFX)
unfaelle_20$LINREFX <- str_sub(unfaelle_20$LINREFX, start = 1, end = 8)
unfaelle_20$LINREFX <- as.numeric(unfaelle_20$LINREFX)
unfaelle_20$LINREFY <- as.character(unfaelle_20$LINREFY)
unfaelle_20$LINREFY <- sub(",", ".", unfaelle_20$LINREFY)
unfaelle_20$LINREFY <- str_sub(unfaelle_20$LINREFY, start = 1, end = 8)
unfaelle_20$LINREFY <- as.numeric(unfaelle_20$LINREFY)
unfaelle_19 <- unfaelle_19 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig)) 
unfaelle_19 <- unfaelle_19[, !names(unfaelle_19) %in% c("IstGkfz")]
unfaelle_18 <- unfaelle_18 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig))
unfaelle_18 <- unfaelle_18[, !names(unfaelle_18) %in% c("IstGkfz")]
unfaelle_17 <- dplyr::rename(unfaelle_17, ULICHTVERH = LICHT)
unfaelle_16 <- dplyr::rename(unfaelle_16, UTYP1 = UTYP)

#Join Dataframes
unfaelle <- rbind(unfaelle_20, unfaelle_19, unfaelle_18, unfaelle_17)
rm(unfaelle_16, unfaelle_16a, unfaelle_17, unfaelle_17a, unfaelle_18, unfaelle_18a, unfaelle_19, unfaelle_19a, unfaelle_20, unfaelle_20a)

#Rename vars
unfaelle <- setNames(unfaelle, c("kategorie", "bundesland", "regierungsbezirk", "kreis", "jahr", "monat", "stunde", "wochentag", "unfallart", "unfalltyp", "lichtv", "fahrrad", "pkw", "fussgaenger", "kraftrad", "sonstig", "lon", "lat"))

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
unfaelle <- unfaelle %>%
  mutate(rad_sonstig = ifelse(fahrrad == 1 & nur_rad == 0 & fussgaenger == 0, 1, 0))
unfaelle <- unfaelle %>%
  mutate(sonstig_ohne_r_und_f = ifelse(nur_pkw == 0 & pkw_rad == 0 & nur_rad == 0 & rad_sonstig == 0 & fussgaenger == 0, 1, 0))


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

#Create additional values for types of accidents
unfaelle_muc <- unfaelle_muc %>% 
  mutate(pkw_final = ifelse(pkw == 1 & fahrrad == 0 & fussgaenger ==0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(rad_final = ifelse(fahrrad == 1 & fussgaenger ==0 , 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(fussgaenger_final = ifelse(fussgaenger == 1, 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(sonstig_final = ifelse(pkw_final == 0 & rad_final == 0 & fussgaenger_final == 0 , 1, 0))

pkw_leicht <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Leichtverletzt")
pkw_schwer <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Schwerverletzt")
pkw_toedlich <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Tödlich")
rad_leicht <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Leichtverletzt")
rad_schwer <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Schwerverletzt")
rad_toedlich <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Tödlich")
fussgaenger_leicht <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Leichtverletzt")
fussgaenger_schwer <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Schwerverletzt")
fussgaenger_toedlich <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Tödlich")
sonstig_leicht <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Leichtverletzt")
sonstig_schwer <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Schwerverletzt")
sonstig_toedlich <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Tödlich")

#Create datasets for Stefan
unfaelle_schwer <- unfaelle[c("kategorie", "lon", "lat")]
unfaelle_schwer <- as(unfaelle_schwer, "Spatial")
writeOGR(unfaelle_schwer, "unfaelle_schwer", layer="unfaelle_schwer", driver="GeoJSON")

unfaelle_pkw <- unfaelle_muc %>%
  filter(nur_pkw == 1)
unfaelle_pkw <- unfaelle_pkw[c("kategorie", "lon", "lat")]
unfaelle_pkw <- as(unfaelle_pkw, "Spatial")
writeOGR(unfaelle_pkw, "unfaelle_pkw", layer="unfaelle_pkw", driver="GeoJSON")

unfaelle_rad <- unfaelle_muc %>%
  filter(rad_final == 1)
unfaelle_rad <- unfaelle_rad[c("kategorie", "lon", "lat")]
unfaelle_rad <- as(unfaelle_rad, "Spatial")
writeOGR(unfaelle_rad, "unfaelle_rad", layer="unfaelle_rad", driver="GeoJSON")

unfaelle_fussgaenger <- unfaelle_muc %>%
  filter(fussgaenger == 1)
unfaelle_fussgaenger <- unfaelle_fussgaenger[c("kategorie", "lon", "lat")]
unfaelle_fussgaenger <- as(unfaelle_fussgaenger, "Spatial")
writeOGR(unfaelle_fussgaenger, "unfaelle_fussgaenger", layer="unfaelle_fussgaenger", driver="GeoJSON")


# 4. Make raster and first map --------------------------------------------

#Make raster
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(unfaelle$lon, unfaelle$lat)
r <- rasterize(pos, r, fun="count")

#Make palettes
raster_pal <- colorNumeric("Reds", c(4, 10), na.color = "#00000000")
points_pal <- colorFactor(palette = c("#fcbf49", "#d62828"),
  levels = c("Schwerverletzt", "Tödlich"))

#Legend
colors <- c("#fcbf49", "#d62828", "#FFF5F0", "#FB6A4A", "#67000D")
labels <- c("Schwerverletzt", "Tödlich", "4 Unfälle", "7 Unfälle", "10 Unfälle")
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

unfaelle$label <- with(unfaelle, paste(jahr, kategorie))

#Make map
m1 <- leaflet(unfaelle) %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "markers", zIndex = 410) %>% 
  addMapPane(name = "labels", zIndex = 420) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(~lon, ~lat, 
                   label = ~label,
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
saveWidget(m1, "unfaelle_muc_schwer.html")
m1

# 5. Second Map ------------------------------------------------------
pkw_df <- filter(unfaelle, nur_pkw == 1)
rad_df <- filter(unfaelle, nur_rad == 1)
pkw_rad_df <- filter(unfaelle, pkw_rad == 1)
pkw_fussgaenger_df <- filter(unfaelle, pkw_fussgaenger == 1)
rad_fussgaenger_df <- filter(unfaelle, rad_fussgaenger == 1)
sonstig_df <- filter(unfaelle, sonstig_2 == 1)

colors <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")
labels <- c("Nur PKW", "Nur Fahrrad", "PKW und Fahrrad", "PKW und Fußgänger", "Fahrrad und Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")

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

pkw_df$label <- with(pkw_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_df$label <- with(rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
pkw_rad_df$label <- with(pkw_rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
pkw_fussgaenger_df$label <- with(pkw_fussgaenger_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_fussgaenger_df$label <- with(rad_fussgaenger_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
sonstig_df$label <- with(sonstig_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))

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
                   label = pkw_df$label,
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_pkw"),
                   group = "PKWs") %>%
  addCircleMarkers(rad_df$lon, rad_df$lat, 
                   label = rad_df$label,
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_rad"),
                   group = "Fahrräder") %>%
  addCircleMarkers(pkw_rad_df$lon, pkw_rad_df$lat, 
                   label = pkw_rad_df$label,
                   radius= 2, 
                   color= "#fe7f2d", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_rad"),
                   group = "PKWs und Fahrräder") %>%
  addCircleMarkers(pkw_fussgaenger_df$lon, pkw_fussgaenger_df$lat, 
                   label = pkw_fussgaenger_df$label,
                   radius= 2, 
                   color= "#16db93", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_fussgaenger"),
                   group = "PKWs und Fußgänger") %>%
  addCircleMarkers(rad_fussgaenger_df$lon, rad_fussgaenger_df$lat, 
                   label = rad_fussgaenger_df$label,
                   radius= 2, 
                   color= "#7f5539", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "rad_fussgaenger"),
                   group = "Fahrräder und Fußgänger") %>%
  addCircleMarkers(sonstig_df$lon, sonstig_df$lat, 
                   label = sonstig_df$label,
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

# 6. Third Map for PKW only
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
                   label = pkw_leicht$jahr,
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
saveWidget(m3, "unfaelle_muc_autos.html")
m3

#7. Fourth map for bikes only
# Filter data
fahrrad <- unfaelle_muc %>%
  filter(rad_final==1)
fahrrad_leicht <- fahrrad %>%
  filter(kategorie == "Leichtverletzt")
fahrrad_schwer <- fahrrad %>%
  filter(kategorie == "Schwerverletzt")
fahrrad_toedlich <- fahrrad %>%
  filter(kategorie == "Tödlich")

#Make raster
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(fahrrad$lon, fahrrad$lat)
r <- rasterize(pos, r, fun="count")

#Make palettes
raster_pal <- colorNumeric("Reds", c(12, 24), na.color = "#00000000")

colors <- c("#808080", "#fcbf49", "#d62828")
labels <- c("Leichtverletzt", "Schwerverletzt", "Tödlich")
sizes <- c(2, 2, 2)
shapes <- c("circle", "circle", "circle")
borders <- c("#808080", "#fcbf49", "#d62828")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Bikes only", opacity = 0.7){
  
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


m4 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "Leichtverletzt", zIndex = 410) %>% 
  addMapPane(name = "Schwerverletzt", zIndex = 420) %>% 
  addMapPane(name = "Tödlich", zIndex = 430) %>% 
  addMapPane(name = "labels", zIndex = 440) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(fahrrad_leicht$lon, fahrrad_leicht$lat, 
                   radius= 2, 
                   color= "#808080", 
                   stroke = FALSE, 
                   fillOpacity = 0.4,
                   options = leafletOptions(pane = "Leichtverletzt"),
                   group = "Fahrräder") %>%
  addCircleMarkers(fahrrad_schwer$lon, fahrrad_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Schwerverletzt"),
                   group = "Fahrräder") %>%
  addCircleMarkers(fahrrad_toedlich$lon, fahrrad_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Tödlich"),
                   group = "Fahrräder") %>%
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
saveWidget(m4, "unfaelle_muc_fahrrad.html")
m4

#7. Fifth map for pedestrians only
# Filter data
fussgaenger <- unfaelle_muc %>%
  filter(fussgaenger==1)
fussgaenger_leicht <- fussgaenger %>%
  filter(kategorie == "Leichtverletzt")
fussgaenger_schwer <- fussgaenger %>%
  filter(kategorie == "Schwerverletzt")
fussgaenger_toedlich <- fussgaenger %>%
  filter(kategorie == "Tödlich")

#Make raster
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(fussgaenger$lon, fussgaenger$lat)
r <- rasterize(pos, r, fun="count")

#Make palettes
raster_pal <- colorNumeric("Reds", c(7, 14), na.color = "#00000000")

colors <- c("#808080", "#fcbf49", "#d62828")
labels <- c("Leichtverletzt", "Schwerverletzt", "Tödlich")
sizes <- c(2, 2, 2)
shapes <- c("circle", "circle", "circle")
borders <- c("#808080", "#fcbf49", "#d62828")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Pedestrians only", opacity = 0.7){
  
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


m5 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "Leichtverletzt", zIndex = 410) %>% 
  addMapPane(name = "Schwerverletzt", zIndex = 420) %>% 
  addMapPane(name = "Tödlich", zIndex = 430) %>% 
  addMapPane(name = "labels", zIndex = 440) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(fussgaenger_leicht$lon, fussgaenger_leicht$lat, 
                   radius= 2, 
                   color= "#808080", 
                   stroke = FALSE, 
                   fillOpacity = 0.4,
                   options = leafletOptions(pane = "Leichtverletzt"),
                   group = "Fussgänger") %>%
  addCircleMarkers(fussgaenger_schwer$lon, fussgaenger_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Schwerverletzt"),
                   group = "Fussgänger") %>%
  addCircleMarkers(fussgaenger_toedlich$lon, fussgaenger_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "Tödlich"),
                   group = "Fussgänger") %>%
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
saveWidget(m5, "unfaelle_muc_fussgaenger.html")
m5

#8 Sixth Map for all accidents
pkw_df <- filter(unfaelle_muc, nur_pkw == 1)
rad_df <- filter(unfaelle_muc, nur_rad == 1)
pkw_rad_df <- filter(unfaelle_muc, pkw_rad == 1)
pkw_fussgaenger_df <- filter(unfaelle_muc, pkw_fussgaenger == 1)
rad_fussgaenger_df <- filter(unfaelle_muc, rad_fussgaenger == 1)
sonstig_df <- filter(unfaelle_muc, sonstig_2 == 1)

colors <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")
labels <- c("Nur PKW", "Nur Fahrrad", "PKW und Fahrrad", "PKW und Fußgänger", "Fahrrad und Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")

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

pkw_df$label <- with(pkw_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_df$label <- with(rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
pkw_rad_df$label <- with(pkw_rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
pkw_fussgaenger_df$label <- with(pkw_fussgaenger_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_fussgaenger_df$label <- with(rad_fussgaenger_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
sonstig_df$label <- with(sonstig_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))

m6 <- leaflet() %>%
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
                   label = pkw_df$label,
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_pkw"),
                   group = "PKWs") %>%
  addCircleMarkers(rad_df$lon, rad_df$lat,
                   label = rad_df$label,
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_rad"),
                   group = "Fahrräder") %>%
  addCircleMarkers(pkw_rad_df$lon, pkw_rad_df$lat, 
                   label = pkw_rad_df$label,
                   radius= 2, 
                   color= "#fe7f2d", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_rad"),
                   group = "PKWs und Fahrräder") %>%
  addCircleMarkers(pkw_fussgaenger_df$lon, pkw_fussgaenger_df$lat, 
                   label = pkw_fussgaenger_df$label,
                   radius= 2, 
                   color= "#16db93", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_fussgaenger"),
                   group = "PKWs und Fußgänger") %>%
  addCircleMarkers(rad_fussgaenger_df$lon, rad_fussgaenger_df$lat, 
                   label = rad_fussgaenger_df$label,
                   radius= 2, 
                   color= "#7f5539", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "rad_fussgaenger"),
                   group = "Fahrräder und Fußgänger") %>%
  addCircleMarkers(sonstig_df$lon, sonstig_df$lat, 
                   label = sonstig_df$label,
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

saveWidget(m6, "unfaelle_alle.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)

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

pkw_df <- filter(unfaelle, nur_pkw == 1)
rad_df <- filter(unfaelle, nur_rad == 1)
pkw_rad_df <- filter(unfaelle, pkw_rad == 1)
rad_sonstig_df <- filter(unfaelle, rad_sonstig == 1)
fussgaenger_df <- filter(unfaelle, fussgaenger == 1)
sonstig_ohne_r_und_f_df <- filter(unfaelle, sonstig_ohne_r_und_f == 1)

colors <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")
labels <- c("Nur PKW", "Nur Fahrrad", "PKW und Fahrrad", "Fahrrad sonstig", "Fußgänger", "Sonstige ohne R und F")
sizes <- c(2, 2, 2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#fe7f2d", "#16db93", "#7f5539", "#c5c3c6")

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

pkw_df$label <- with(pkw_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_df$label <- with(rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
pkw_rad_df$label <- with(pkw_rad_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
rad_sonstig_df$label <- with(rad_sonstig_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
fussgaenger_df$label <- with(fussgaenger_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))
sonstig_ohne_r_und_f_df$label <- with(sonstig_ohne_r_und_f_df, paste0(jahr, ", ", kategorie, ", Tag (1=Sonntag): ", wochentag, ", Stunde: ", stunde, ", UTyp: ", unfalltyp, ", Licht (0 = hell): ", lichtv))

m2 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "nur_pkw", zIndex = 410) %>% 
  addMapPane(name = "nur_rad", zIndex = 420) %>%
  addMapPane(name = "pkw_rad", zIndex = 430) %>%
  addMapPane(name = "rad_sonstig", zIndex = 440) %>%
  addMapPane(name = "fussgaenger", zIndex = 450) %>%
  addMapPane(name = "sonstige_ohne_r_und_f", zIndex = 460) %>%
  addMapPane(name = "labels", zIndex = 470) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_df$lon, pkw_df$lat, 
                   label = pkw_df$label,
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_pkw"),
                   group = "Nur PKWs") %>%
  addCircleMarkers(rad_df$lon, rad_df$lat, 
                   label = rad_df$label,
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "nur_rad"),
                   group = "Nur Fahrräder") %>%
  addCircleMarkers(pkw_rad_df$lon, pkw_rad_df$lat, 
                   label = pkw_rad_df$label,
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "pkw_rad"),
                   group = "PKW und Fahrrad") %>%
  addCircleMarkers(rad_sonstig_df$lon, rad_sonstig_df$lat, 
                   label = rad_sonstig_df$label,
                   radius= 2, 
                   color= "#fe7f2d", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "rad_sonstig"),
                   group = "Fahrrad sonstige") %>%
  addCircleMarkers(fussgaenger_df$lon, fussgaenger_df$lat, 
                   label = fussgaenger_df$label,
                   radius= 2, 
                   color= "#16db93", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "fussgaenger"),
                   group = "Fußgänger") %>%
  addCircleMarkers(sonstig_ohne_r_und_f_df$lon, sonstig_ohne_r_und_f_df$lat, 
                   label = sonstig_ohne_r_und_f_df$label,
                   radius= 2, 
                   color= "#7f5539", 
                   stroke = FALSE, 
                   fillOpacity = 0.7,
                   options = leafletOptions(pane = "sonstig_ohne_r_und_f"),
                   group = "Sonstige ohne R und F") %>%
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
  addLayersControl(overlayGroups = c("Nur PKWs", "Nur Fahrräder", "PKW und Fahrrad", "Fahrrad sonstige", "Fußgänger", "Sonstige ohne R und F"),
                   position = "topright") %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

saveWidget(m2, "unfaelle_muc2.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)

#Additional map with layer filters by type and category
#Create values for types of accidents
unfaelle_muc <- unfaelle_muc %>% 
  mutate(pkw_final = ifelse(pkw == 1 & fahrrad == 0 & fussgaenger ==0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(rad_final = ifelse(fahrrad == 1 & fussgaenger ==0 , 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(fussgaenger_final = ifelse(fussgaenger == 1, 1, 0))
unfaelle_muc <- unfaelle_muc %>% 
  mutate(sonstig_final = ifelse(pkw_final == 0 & rad_final == 0 & fussgaenger_final == 0 , 1, 0))

pkw_leicht <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Leichtverletzt")
pkw_schwer <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Schwerverletzt")
pkw_toedlich <- filter(unfaelle_muc, pkw_final == 1 & kategorie == "Tödlich")
rad_leicht <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Leichtverletzt")
rad_schwer <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Schwerverletzt")
rad_toedlich <- filter(unfaelle_muc, rad_final == 1 & kategorie == "Tödlich")
fussgaenger_leicht <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Leichtverletzt")
fussgaenger_schwer <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Schwerverletzt")
fussgaenger_toedlich <- filter(unfaelle_muc, fussgaenger_final == 1 & kategorie == "Tödlich")
sonstig_leicht <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Leichtverletzt")
sonstig_schwer <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Schwerverletzt")
sonstig_toedlich <- filter(unfaelle_muc, sonstig_final == 1 & kategorie == "Tödlich")


colors <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")
labels <- c("Nur PKW", "Fahrrad", "Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Beteiligte", opacity = 0.9){
  
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

m7 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "pkw_leicht", zIndex = 410) %>% 
  addMapPane(name = "pkw_schwer", zIndex = 420) %>%
  addMapPane(name = "pkw_toedlich", zIndex = 430) %>%
  addMapPane(name = "rad_leicht", zIndex = 440) %>%
  addMapPane(name = "rad_schwer", zIndex = 450) %>%
  addMapPane(name = "rad_toedlich", zIndex = 460) %>%
  addMapPane(name = "fussgaenger_leicht", zIndex = 470) %>% 
  addMapPane(name = "fussgaenger_schwer", zIndex = 480) %>%
  addMapPane(name = "fussgaenger_toedlich", zIndex = 490) %>%
  addMapPane(name = "sonstig_leicht", zIndex = 500) %>%
  addMapPane(name = "sonstig_schwer", zIndex = 510) %>%
  addMapPane(name = "sonstig_toedlich", zIndex = 520) %>%
  addMapPane(name = "labels", zIndex = 530) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_leicht$lon, pkw_leicht$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "pkw_leicht"),
                   group = "PKW leicht") %>%
  addCircleMarkers(pkw_schwer$lon, pkw_schwer$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_schwer"),
                   group = "PKW schwer") %>%
  addCircleMarkers(pkw_toedlich$lon, pkw_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_toedlich"),
                   group = "PKW toedlich") %>%
  addCircleMarkers(rad_leicht$lon, rad_leicht$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "rad_leicht"),
                   group = "rad leicht") %>%
  addCircleMarkers(rad_schwer$lon, rad_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_schwer"),
                   group = "rad schwer") %>%
  addCircleMarkers(rad_toedlich$lon, rad_toedlich$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_toedlich"),
                   group = "rad toedlich") %>%
  addCircleMarkers(fussgaenger_leicht$lon, fussgaenger_leicht$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity =0.5,
                   options = leafletOptions(pane = "fussgaenger_leicht"),
                   group = "fussgaenger leicht") %>%
  addCircleMarkers(fussgaenger_schwer$lon, fussgaenger_schwer$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_schwer"),
                   group = "fussgaenger schwer") %>%
  addCircleMarkers(fussgaenger_toedlich$lon, fussgaenger_toedlich$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_toedlich"),
                   group = "fussgaenger toedlich") %>%
  addCircleMarkers(sonstig_leicht$lon, sonstig_leicht$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "sonstig_leicht"),
                   group = "sonstig leicht") %>%
  addCircleMarkers(sonstig_schwer$lon, sonstig_schwer$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_schwer"),
                   group = "sonstig schwer") %>%
  addCircleMarkers(sonstig_toedlich$lon, sonstig_toedlich$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_toedlich"),
                   group = "sonstig toedlich") %>%
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
  addLayersControl(overlayGroups = c("PKW leicht", "PKW schwer", "PKW toedlich", "rad leicht", "rad schwer", "rad toedlich", 
                                     "fussgaenger leicht", "fussgaenger schwer", "fussgaenger toedlich", "sonstig leicht", "sonstig schwer", "sonstig toedlich")) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

saveWidget(m7, "unfaelle_muc_7.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)

#Additional Map schwer
#Additional map
#Create values for types of accidents
unfaelle <- unfaelle %>% 
  mutate(pkw_final = ifelse(pkw == 1 & fahrrad == 0 & fussgaenger ==0 & kraftrad == 0 & sonstig == 0, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(rad_final = ifelse(fahrrad == 1 & fussgaenger ==0 , 1, 0))
unfaelle <- unfaelle %>% 
  mutate(fussgaenger_final = ifelse(fussgaenger == 1, 1, 0))
unfaelle <- unfaelle %>% 
  mutate(sonstig_final = ifelse(pkw_final == 0 & rad_final == 0 & fussgaenger_final == 0 , 1, 0))

pkw_leicht <- filter(unfaelle, pkw_final == 1 & kategorie == "Leichtverletzt")
pkw_schwer <- filter(unfaelle, pkw_final == 1 & kategorie == "Schwerverletzt")
pkw_toedlich <- filter(unfaelle, pkw_final == 1 & kategorie == "Tödlich")
rad_leicht <- filter(unfaelle, rad_final == 1 & kategorie == "Leichtverletzt")
rad_schwer <- filter(unfaelle, rad_final == 1 & kategorie == "Schwerverletzt")
rad_toedlich <- filter(unfaelle, rad_final == 1 & kategorie == "Tödlich")
fussgaenger_leicht <- filter(unfaelle, fussgaenger_final == 1 & kategorie == "Leichtverletzt")
fussgaenger_schwer <- filter(unfaelle, fussgaenger_final == 1 & kategorie == "Schwerverletzt")
fussgaenger_toedlich <- filter(unfaelle, fussgaenger_final == 1 & kategorie == "Tödlich")
sonstig_leicht <- filter(unfaelle, sonstig_final == 1 & kategorie == "Leichtverletzt")
sonstig_schwer <- filter(unfaelle, sonstig_final == 1 & kategorie == "Schwerverletzt")
sonstig_toedlich <- filter(unfaelle, sonstig_final == 1 & kategorie == "Tödlich")


colors <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")
labels <- c("Nur PKW", "Fahrrad", "Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Beteiligte", opacity = 0.9){
  
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

m8 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "pkw_leicht", zIndex = 410) %>% 
  addMapPane(name = "pkw_schwer", zIndex = 420) %>%
  addMapPane(name = "pkw_toedlich", zIndex = 430) %>%
  addMapPane(name = "rad_leicht", zIndex = 440) %>%
  addMapPane(name = "rad_schwer", zIndex = 450) %>%
  addMapPane(name = "rad_toedlich", zIndex = 460) %>%
  addMapPane(name = "fussgaenger_leicht", zIndex = 470) %>% 
  addMapPane(name = "fussgaenger_schwer", zIndex = 480) %>%
  addMapPane(name = "fussgaenger_toedlich", zIndex = 490) %>%
  addMapPane(name = "sonstig_leicht", zIndex = 500) %>%
  addMapPane(name = "sonstig_schwer", zIndex = 510) %>%
  addMapPane(name = "sonstig_toedlich", zIndex = 520) %>%
  addMapPane(name = "labels", zIndex = 530) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_leicht$lon, pkw_leicht$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "pkw_leicht"),
                   group = "PKW leicht") %>%
  addCircleMarkers(pkw_schwer$lon, pkw_schwer$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_schwer"),
                   group = "PKW schwer") %>%
  addCircleMarkers(pkw_toedlich$lon, pkw_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_toedlich"),
                   group = "PKW toedlich") %>%
  addCircleMarkers(rad_leicht$lon, rad_leicht$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "rad_leicht"),
                   group = "rad leicht") %>%
  addCircleMarkers(rad_schwer$lon, rad_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_schwer"),
                   group = "rad schwer") %>%
  addCircleMarkers(rad_toedlich$lon, rad_toedlich$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_toedlich"),
                   group = "rad toedlich") %>%
  addCircleMarkers(fussgaenger_leicht$lon, fussgaenger_leicht$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "fussgaenger_leicht"),
                   group = "fussgaenger leicht") %>%
  addCircleMarkers(fussgaenger_schwer$lon, fussgaenger_schwer$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_schwer"),
                   group = "fussgaenger schwer") %>%
  addCircleMarkers(fussgaenger_toedlich$lon, fussgaenger_toedlich$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_toedlich"),
                   group = "fussgaenger toedlich") %>%
  addCircleMarkers(sonstig_leicht$lon, sonstig_leicht$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "sonstig_leicht"),
                   group = "sonstig leicht") %>%
  addCircleMarkers(sonstig_schwer$lon, sonstig_schwer$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_schwer"),
                   group = "sonstig schwer") %>%
  addCircleMarkers(sonstig_toedlich$lon, sonstig_toedlich$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_toedlich"),
                   group = "sonstig toedlich") %>%
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
  addLayersControl(overlayGroups = c("PKW leicht", "PKW schwer", "PKW toedlich", "rad leicht", "rad schwer", "rad toedlich", 
                                     "fussgaenger leicht", "fussgaenger schwer", "fussgaenger toedlich", "sonstig leicht", "sonstig schwer", "sonstig toedlich")) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

saveWidget(m8, "unfaelle_8.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)

#Additional map with filters by daytime
#Additional map with layer filters by type and category


colors <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")
labels <- c("Nur PKW", "Fahrrad", "Fußgänger", "Sonstige")
sizes <- c(2, 2, 2, 2)
shapes <- c("circle", "circle", "circle", "circle")
borders <- c("#d62828", "#fcbf49", "#00008b", "#c5c3c6")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, position = "bottomright", title = "Beteiligte", opacity = 0.9){
  
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

m7 <- leaflet() %>%
  setView(11.55, 48.15, zoom = 11) %>% 
  addMapPane(name = "pkw_leicht", zIndex = 410) %>% 
  addMapPane(name = "pkw_schwer", zIndex = 420) %>%
  addMapPane(name = "pkw_toedlich", zIndex = 430) %>%
  addMapPane(name = "rad_leicht", zIndex = 440) %>%
  addMapPane(name = "rad_schwer", zIndex = 450) %>%
  addMapPane(name = "rad_toedlich", zIndex = 460) %>%
  addMapPane(name = "fussgaenger_leicht", zIndex = 470) %>% 
  addMapPane(name = "fussgaenger_schwer", zIndex = 480) %>%
  addMapPane(name = "fussgaenger_toedlich", zIndex = 490) %>%
  addMapPane(name = "sonstig_leicht", zIndex = 500) %>%
  addMapPane(name = "sonstig_schwer", zIndex = 510) %>%
  addMapPane(name = "sonstig_toedlich", zIndex = 520) %>%
  addMapPane(name = "labels", zIndex = 530) %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(opacity = 0.8)) %>%
  addCircleMarkers(pkw_leicht$lon, pkw_leicht$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "pkw_leicht"),
                   group = "PKW leicht") %>%
  addCircleMarkers(pkw_schwer$lon, pkw_schwer$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_schwer"),
                   group = "PKW schwer") %>%
  addCircleMarkers(pkw_toedlich$lon, pkw_toedlich$lat, 
                   radius= 2, 
                   color= "#d62828", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "pkw_toedlich"),
                   group = "PKW toedlich") %>%
  addCircleMarkers(rad_leicht$lon, rad_leicht$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "rad_leicht"),
                   group = "rad leicht") %>%
  addCircleMarkers(rad_schwer$lon, rad_schwer$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_schwer"),
                   group = "rad schwer") %>%
  addCircleMarkers(rad_toedlich$lon, rad_toedlich$lat, 
                   radius= 2, 
                   color= "#fcbf49", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "rad_toedlich"),
                   group = "rad toedlich") %>%
  addCircleMarkers(fussgaenger_leicht$lon, fussgaenger_leicht$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity =0.5,
                   options = leafletOptions(pane = "fussgaenger_leicht"),
                   group = "fussgaenger leicht") %>%
  addCircleMarkers(fussgaenger_schwer$lon, fussgaenger_schwer$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_schwer"),
                   group = "fussgaenger schwer") %>%
  addCircleMarkers(fussgaenger_toedlich$lon, fussgaenger_toedlich$lat, 
                   radius= 2, 
                   color= "#00008b", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "fussgaenger_toedlich"),
                   group = "fussgaenger toedlich") %>%
  addCircleMarkers(sonstig_leicht$lon, sonstig_leicht$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   options = leafletOptions(pane = "sonstig_leicht"),
                   group = "sonstig leicht") %>%
  addCircleMarkers(sonstig_schwer$lon, sonstig_schwer$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_schwer"),
                   group = "sonstig schwer") %>%
  addCircleMarkers(sonstig_toedlich$lon, sonstig_toedlich$lat, 
                   radius= 2, 
                   color= "#c5c3c6", 
                   stroke = FALSE, 
                   fillOpacity = 1,
                   options = leafletOptions(pane = "sonstig_toedlich"),
                   group = "sonstig toedlich") %>%
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
  addLayersControl(overlayGroups = c("PKW leicht", "PKW schwer", "PKW toedlich", "rad leicht", "rad schwer", "rad toedlich", 
                                     "fussgaenger leicht", "fussgaenger schwer", "fussgaenger toedlich", "sonstig leicht", "sonstig schwer", "sonstig toedlich")) %>%
  addLegendCustom(colors, labels, sizes, shapes, borders)

saveWidget(m7, "unfaelle_muc_7.html")

rm(pkw_df, rad_df, pkw_rad_df, pkw_fussgaenger_df, rad_fussgaenger_df, sonstig_df)





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

#Data Codebook: https://opendata-esri-de.opendata.arcgis.com/datasets/esri-de-content::verkehrsunfälle-2019

muc <- subset(lkr, county == "SK München")
muc <- as_Spatial(muc[, "geometry"])
rm(lkr)

# 3. Filter and transform vars --------------------------------------------
#Vars filtern
unfaelle_19 <- unfaelle_19a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_18 <- unfaelle_18a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_17 <- unfaelle_17a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]
unfaelle_16 <- unfaelle_16a[c("UKATEGORIE", "ULAND", "UREGBEZ", "UKREIS", "UJAHR", "UMONAT", "USTUNDE", "UWOCHENTAG", "UART", "IstRad", "IstPKW", "IstFuss", "IstKrad", "IstSonstig", "LINREFX", "LINREFY")]

#Für 2019 und 2018 gkfz mit sonstigem vermengen und gkfz-Variable löschen
unfaelle_19 <- unfaelle_19 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig)) 
unfaelle_19 <- unfaelle_19[, !names(unfaelle_19) %in% c("IstGkfz")]
unfaelle_18 <- unfaelle_18 %>%
  mutate(IstSonstig = ifelse(IstSonstig == 0, IstGkfz, IstSonstig))
unfaelle_18 <- unfaelle_18[, !names(unfaelle_18) %in% c("IstGkfz")]

#Join Dataframes
unfaelle <- rbind(unfaelle_19, unfaelle_18, unfaelle_17, unfaelle_16)
rm(unfaelle_16, unfaelle_16a, unfaelle_17, unfaelle_17a, unfaelle_18, unfaelle_18a, unfaelle_19, unfaelle_19a)

#Vars umbenennen
unfaelle <- setNames(unfaelle, c("kategorie", "bundesland", "regierungsbezirk", "kreis", "jahr", "monat", "stunde", "wochentag", "verletzt", "fahrrad", "pkw", "fussgaenger", "kraftrad", "sonstig", "lon", "lat"))

#Nach München und Schwerverletzten/Toten filtern
unfaelle <- unfaelle %>% 
  filter(bundesland == 9 & regierungsbezirk == 1 & kreis == 62) %>%
  filter(kategorie == 1 | kategorie == 2)

#Werte verändern
unfaelle$kategorie <- mapvalues(unfaelle$kategorie, from = c("1", "2"), to = c("Tödlich", "Schwerverletzt"))

#Vars für Fußgänger, Radfahrer, Autofahrer und sonstige erstellen
unfaelle$sum <- rowSums(unfaelle[,c("pkw", "fahrrad", "fussgaenger", "kraftrad", "sonstig")])
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

#CRS ändern
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

#Raster erstellen
r <- raster(muc, res=0.0015)
crs(r) <- proj4string(muc)
pos <- data.frame(unfaelle$lon, unfaelle$lat)
r <- rasterize(pos, r, fun="count")
r[r < 4] <- NA

#Farbskalen erstellen
raster_pal <- colorNumeric("Reds", c(3, 7), na.color = "#00000000")
raster_pal2 <- palette(c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"))
points_pal <- colorFactor(palette = c("#fcbf49", "#d62828"),
  levels = c("Schwerverletzt", "Tödlich"))

#Legende
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


#Maps erstellen
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


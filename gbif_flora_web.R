### content: GBIF Übung - Flora Web Daten laden und Karten erstellen
###

# benötigte Pakete
library(rgbif)
library(data.table)
library(sf)
library(mapview)

# wir laden FloraWeb-Daten aus GBIF.
# dafür müssen wir uns den Institutional Code aus GBIF heraussuchen: e6fab7b3-c733-40b9-8df3-2a03e49532c1 oder wir nutzen den institutionCode="BfN"



# Bewimperte Alpenrose

speclist <- "Rhododendron hirsutum"


#  diese Funktion soll dabei helfen die richtige Art zu finden
name_suggest(speclist)
name_suggest(speclist)$data$key[1] # wir wählen den ersten aus

keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 
# vergleich: https://www.gbif.org/species/4174017


# auslesen der Daten aus GBIF. 
gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 1000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")


Rhododendron_hirsutum <- as.data.frame(gbif_species$data) # umwandeln in Data Frame

dim(Rhododendron_hirsutum) # kommt in 180 Kartenblättern vor

gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 1000, institutionCode="BfN")

Rhododendron_hirsutum <- as.data.frame(gbif_species$data)
dim(Rhododendron_hirsutum)


#### plotten auf Karte
# Umwandeln in SimpleFeature Geodaten
Rhododendron_hirsutum <- st_as_sf(Rhododendron_hirsutum, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))


mapview(Rhododendron_hirsutum, map.types="OpenStreetMap", cex = 2 )


## zweiter Versuch mit der Hirschzunge
speclist <- "Asplenium scolopendrium"


#  diese Funktion soll dabei helfen die richtige Art zu finden
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 
# vergleich: https://www.gbif.org/species/2650669


# auslesen der Daten aus GBIF. Für Borstgrasrasen dauert das sehr lange. 
gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 10000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")

Asplenium_scolopendrium <- as.data.frame(gbif_species$data)

dim(Asplenium_scolopendrium)

#### plotten auf Karte

# Umwandeln in SimpleFeature Geodaten

Asplenium_scolopendrium <- st_as_sf(Asplenium_scolopendrium, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))


mapview(Asplenium_scolopendrium, map.types="OpenStreetMap", cex = 2 )



###################### Geht es auch für Pflanzengesellschaften


# Beispiel Xerobrometum (Trespen-Trockenrasen): Typische Arten
speclist <- c("Globularia bisnagarica", "Linum tenuifolium", "Fumana procumbens", "Trinia glauca","Teucrium montanum") 


#  diese Funktion soll dabei helfen die richtige Art zu finden
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 

# ersetzen von 8128422 mit 6027378, denn es gibt zwei Arten mit dem Namen Trinia glauca (unterschiedliche Autoren)

keys[4] <- 6027378

gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 10000, institutionCode="BfN")



# Aus GBIF wird eine verschachtelte Liste exportiert, mit dieser Funktion macht man einen Data-Frame draus. 
gbif_species_df<- rbindlist(lapply(gbif_species, function(x) x$data), fill = TRUE, use.names = TRUE)

# auswählen von Koordinaten und Artname
gbif_species_df <- gbif_species_df[,c("species","decimalLatitude","decimalLongitude")]

# anzeigen, wie viele Vorkommen gefunden wurden
table(gbif_species_df$species)

# Doppelungen löschen
gbif_species_df <- gbif_species_df[!duplicated(gbif_species_df),]


#### Zählen der Kombination und plotten auf Karte #########

# Umwandeln in SimpleFeature Geodaten

gbif_species_df_geo <- st_as_sf(gbif_species_df, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))

# schreiben der Koordinaten in einen Data Frame
df <- st_coordinates(gbif_species_df_geo)
df <- as.data.frame(df)

# zählen die Anzahl von Punkten, die übereinander liegen (~Artenzahl)
df2 <-  setDT(df)[,list(value=.N),names(df)]
df2 <- as.data.frame(df2)

table(df2$value)

# alle löschen, wo nur eine Art vorkommt
df2 <- df2[df2$value>1,]


# plotten der Anzahl
df_count <- st_as_sf(df2, coords = c("X", "Y"), crs = st_crs(4326))

mapview(df_count, map.types="OpenStreetMap", cex = 3 )

###############################
#### Steppenrasen?
##############################
#"Pulsatilla pratensis subsp. nigricans", "Adonis vernalis", "Astragalus exscapus", "Oxytropis pilosa", "Potentilla incana", "Seseli hippomarathrum", "Silene otites"





library(remotes)
library(tidyverse)
library(usethis)
library(spotifyr)
library(ggplot2)

spotifyr::get_spotify_access_token()
options(max.print=1000)

# Energy, valence, tempo?

# rumours %>% ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1)


fleetwoodMac <- get_playlist_audio_features("", "1NdovVKsJylONwiE6WXwB5")

fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac"] <- "Fleetwood Mac 1"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Then Play On (2013 Remaster; Expanded Edition)"] <- "Then Play On"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac (2017 Remaster)"] <- "Fleetwood Mac 2"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tusk (2015 Remaster)"] <- "Tusk"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Mirage (2016 Remaster)"] <- "Mirage"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tango in the Night (2017 Remaster)"] <- "Tango in the Night"

fleetwoodMac 

fleetwoodMac_byAlbum <- fleetwoodMac %>% 
  group_by(track.album.name, track.album.release_date) %>%
  summarize(averageEnergy = mean(energy), averageValence = mean(valence))

ggplot(fleetwoodMac_byAlbum, aes(averageEnergy, averageValence, label = track.album.name)) + geom_point() + geom_text(hjust=0, vjust=0)

ggplot(fleetwoodMac, aes(track.album.release_date, tempo)) + geom_boxplot() + theme(axis.text.x = element_text(angle=-90, hjust=0))

ggplot(fleetwoodMac, aes(energy, valence, size = track.popularity, colour = mode_name)) + 
  geom_point() + 
  facet_wrap(~track.album.release_date + track.album.name)  +
  labs(x = "Valence", y = "Energy", colour = "Mode", size = "Track Popularity")
                                                                             
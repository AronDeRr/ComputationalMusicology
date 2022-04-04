library(flexdashboard)
library(remotes)
library(tidyverse)
library(usethis)
library(spotifyr)
library(ggplot2)
library(knitr)
library(compmus)
library(plotly)
library(ggdendro)
library(heatmaply)
library(tidymodels)
spotifyr::get_spotify_access_token()

fleetwoodMac <- get_playlist_audio_features("", "1NdovVKsJylONwiE6WXwB5")

fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac"] <- "Fleetwood Mac 1"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Then Play On (2013 Remaster; Expanded Edition)"] <- "Then Play On"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac (2017 Remaster)"] <- "Fleetwood Mac 2"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tusk (2015 Remaster)"] <- "Tusk"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Mirage (2016 Remaster)"] <- "Mirage"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tango in the Night (2017 Remaster)"] <- "Tango in the Night"
fleetwoodMac$track.name[fleetwoodMac$track.name == "Coming Home" & fleetwoodMac$track.album.name == "Heroes Are Hard to Find"] <- "Coming Home 2"

fleetwoodMac <- fleetwoodMac %>%
  mutate(era = case_when(track.album.name == "Fleetwood Mac 1" ~ "Peter Green",
                         track.album.name == "Mr. Wonderful" ~ "Peter Green",
                         track.album.name == "Then Play On" ~ "Peter Green",
                         track.album.name == "Kiln House" ~ "Peter Green",
                         track.album.name == "Future Games" ~ "Bob Welch",
                         track.album.name == "Bare Trees" ~ "Bob Welch",
                         track.album.name == "Penguin" ~ "Bob Welch",
                         track.album.name == "Mystery to Me" ~ "Bob Welch",
                         track.album.name == "Heroes Are Hard to Find" ~ "Bob Welch",
                         track.album.name == "Fleetwood Mac 2" ~ "Buckingham Nicks",
                         track.album.name == "Rumours" ~ "Buckingham Nicks",
                         track.album.name == "Tusk" ~ "Buckingham Nicks",
                         track.album.name == "Mirage" ~ "Buckingham Nicks",
                         track.album.name == "Tango in the Night" ~ "Buckingham Nicks",
                         track.album.name == "Say You Will" ~ "Buckingham Nicks",
                         track.album.name == "Behind the Mask" ~ "Billy Burnette",
                         track.album.name == "Time" ~ "Billy Burnette"))

fleetwoodMac$era <- factor(fleetwoodMac$era, levels = c("Peter Green", "Bob Welch", "Buckingham Nicks", "Billy Burnette"))
fleetwoodMac$track.album.name <- factor(fleetwoodMac$track.album.name, levels = c("Fleetwood Mac 1", "Mr. Wonderful", "Then Play On", "Kiln House", "Future Games", "Bare Trees", "Penguin", "Mystery to Me", "Heroes Are Hard to Find", "Fleetwood Mac 2", "Rumours", "Tusk", "Mirage", "Tango in the Night", "Behind the Mask", "Time", "Say You Will"))

fleetwoodMacAlbum <- fleetwoodMac %>%
  group_by(track.album.name, era) %>%
  summarize(avg_danceability = mean(danceability),
            avg_energy = mean(energy),
            avg_loudness = mean(loudness),
            avg_speechiness = mean(speechiness),
            avg_acousticness = mean(acousticness),
            avg_instrumentalness = mean(instrumentalness),
            avg_liveness = mean(liveness),
            avg_tempo = mean(tempo),
            avg_valence = mean(valence))

fleetwoodMac_juice <-
  recipe(
    track.album.name ~
      avg_danceability +
      avg_energy +
      avg_loudness +
      # avg_speechiness +
      avg_acousticness +
      # avg_instrumentalness +
      # avg_liveness +
      avg_valence +
      avg_tempo,
      # C + `C#|Db` + D + `D#|Eb` +
      # E + `F` + `F#|Gb` + G +
      # `G#|Ab` + A + `A#|Bb` + B +
      # c01 + c02 + c03 + c04 + c05 + c06 +
      # c07 + c08 + c09 + c10 + c11 + c12,
    data = fleetwoodMacAlbum
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(fleetwoodMacAlbum %>% mutate(track.album.name = str_trunc(track.album.name, 30))) %>%
  juice() %>%
  column_to_rownames("track.album.name")

fleetwoodMac_distance <- dist(fleetwoodMac_juice, method = "euclidean")

fleetwoodMac_distance %>% 
  hclust(method = "average") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram() + 
  labs(title = "Clustering the albums")


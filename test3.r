library(flexdashboard)
library(remotes)
library(tidyverse)
library(usethis)
library(spotifyr)
library(ggplot2)
library(knitr)
library(compmus)
library(plotly)
spotifyr::get_spotify_access_token()

fleetwoodMac <- get_playlist_audio_features("", "1NdovVKsJylONwiE6WXwB5")

fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac"] <- "Fleetwood Mac 1"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Then Play On (2013 Remaster; Expanded Edition)"] <- "Then Play On"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Fleetwood Mac (2017 Remaster)"] <- "Fleetwood Mac 2"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tusk (2015 Remaster)"] <- "Tusk"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Mirage (2016 Remaster)"] <- "Mirage"
fleetwoodMac$track.album.name[fleetwoodMac$track.album.name == "Tango in the Night (2017 Remaster)"] <- "Tango in the Night"

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
 
tempoPlot <- ggplot(fleetwoodMac) +
    facet_grid(~era, scales = "free", space = "free") +
    geom_boxplot(aes(track.album.name, tempo)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "", y = "Tempo", title = "Album tempo in a boxplot")

tempoPlotly <- ggplotly(tempoPlot)
hoverinfo <- with(fleetwoodMac, paste0("Song: ", track.name, "</br></br", 
                                     "Tempo: ", tempo))
                  
tempoPlotly$x$data[[1]]$text <- hoverinfo
tempoPlotly$x$data[[1]]$hoverinfo <- c("text", "boxes")

tempoPlotly


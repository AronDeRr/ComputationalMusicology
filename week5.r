library(tidyverse)
library(spotifyr)
library(compmus)

pata_pata <-
  get_tidy_audio_analysis("6CDzDgIUqeDY5g8ujExx2f") %>%
  select(segments) %>%
  unnest(segments)

pata_pata %>%
  mutate(loudness_max_time = start + loudness_max_time) %>%
  arrange(loudness_max_time) %>%
  mutate(delta_loudness = loudness_max - lag(loudness_max)) %>%
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

pata_pata %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  arrange(start) %>%
  mutate(pitches = map2(pitches, lag(pitches), `-`)) %>%
  slice(-1) %>% 
  compmus_gather_chroma() %>% 
  group_by(start, duration) %>% 
  summarise(novelty = sum(log1p(pmax(value, 0)))) %>% 
  ggplot(aes(x = start + duration / 2, y = novelty)) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

pata_pata %>%
  arrange(start) %>%
  mutate(timbre = map2(timbre, lag(timbre), `-`)) %>%
  slice(-1) %>%
  compmus_gather_timbre() %>%
  group_by(start, duration) %>% 
  summarise(novelty = sum(log1p(pmax(value, 0)))) %>% 
  ggplot(aes(x = start + duration / 2, y = novelty)) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

graveola <- get_tidy_audio_analysis("6PJasPKAzNLSOzxeAH33j2")
graveola %>%
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

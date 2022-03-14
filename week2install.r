# remotes::install_github('jaburgoyne/compmus')
# install.packages("flexdashboard")
# gitcreds::gitcreds_set()
library(tidyverse)
library(dplyr)
library(spotifyr)
library(compmus)

landslide_live_1997 <-
  get_tidy_audio_analysis("06h9kk12VjJ2bqcBc6IScR") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

landslide_studio <-
  get_tidy_audio_analysis("5ihS6UUlyQAfmp48eSkxuQ") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

landslide_live_1975 <-
  get_tidy_audio_analysis("53IkukOzY8ZnDfr4AdVuql") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

landslide_studio %>%
  mutate(pitches = map(pitches, compmus_normalise, "manhattan")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title="Landslide (studio version)") +
  theme_minimal() +
  scale_fill_viridis_c()

landslide_live_1975 %>%
  mutate(pitches = map(pitches, compmus_normalise, "manhattan")) %>%
  compmus_gather_chroma() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Landslide (live version 1975") +
  theme_minimal() +
  scale_fill_viridis_c()

landslide_live_1997 %>%
  mutate(pitches = map(pitches, compmus_normalise, "manhattan")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Landslide (live version 1997") +
  theme_minimal() +
  scale_fill_viridis_c()



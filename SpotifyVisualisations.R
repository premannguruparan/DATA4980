library(tidyverse)
library(ggridges)
library(readxl)
spotify_data <- read_excel("spotify-2023.xlsx")
# Convert Year Released to numeric if not already
spotify_data <- spotify_data %>%
  mutate(year_released = as.numeric(`Year Released`))

# Scatterplot with smoothing
ggplot(spotify_data, aes(x = year_released, y = Streams)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", size = 1.2) +
  labs(
    title = "Do Older or Newer Songs Get More Streams in 2023?",
    x = "Year Released",
    y = "Streams (2023)"
  ) +
  theme_minimal(base_size = 14)

# Create age variable
spotify_data <- spotify_data %>%
  mutate(
    year_released = as.numeric(`Year Released`),
    song_age = 2023 - year_released,
    age_group = cut(song_age, breaks = c(0, 2, 5, 10, 20, 30, Inf),
                    labels = c("0–2 yrs", "3–5 yrs", "6–10 yrs", "11–20 yrs", "21–30 yrs", "30+ yrs"),
                    right = FALSE)
  )

# Boxplot
ggplot(spotify_data, aes(x = age_group, y = Streams)) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.3) +
  labs(
    title = "How Does Song Age Relate to Streams in 2023?",
    x = "Song Age Group",
    y = "Streams (2023)"
  ) +
  theme_minimal(base_size = 14)

# Calculate top 10% threshold
stream_threshold <- quantile(spotify_data$Streams, 0.90, na.rm = TRUE)

# Filter for top streamed songs and count per release year
top_years <- spotify_data %>%
  filter(Streams >= stream_threshold) %>%
  group_by(year_released) %>%
  summarise(top_songs_count = n()) %>%
  arrange(desc(top_songs_count)) %>%
  slice_max(top_songs_count, n = 15)

# Create lollipop chart
ggplot(top_years, aes(x = reorder(as.factor(year_released), top_songs_count), y = top_songs_count)) +
  geom_segment(aes(x = reorder(as.factor(year_released), top_songs_count), 
                   xend = reorder(as.factor(year_released), top_songs_count), 
                   y = 0, yend = top_songs_count), color = "gray") +
  geom_point(size = 4, color = "darkorange") +
  coord_flip() +
  labs(
    title = "Top Years Among the Top 10% Streamed Songs in 2023",
    x = "Year Released",
    y = "Count of Top-Streamed Songs"
  ) +
  theme_minimal(base_size = 14)

# Define top-streamed threshold
top_threshold <- quantile(spotify_data$Streams, 0.90, na.rm = TRUE)

# Categorize tracks
spotify_bpm <- spotify_data %>%
  mutate(
    popularity_group = ifelse(Streams >= top_threshold, "Top 10%", "Other 90%"),
    BPM = as.numeric(BPM)
  ) %>%
  filter(!is.na(BPM), BPM >= 60, BPM <= 200)  # Filter plausible BPMs

# Ridge plot
ggplot(spotify_bpm, aes(x = BPM, y = popularity_group, fill = popularity_group)) +
  geom_density_ridges(scale = 1.2, alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("Top 10%" = "tomato", "Other 90%" = "steelblue")) +
  labs(
    title = "Do Top-Streamed Songs Cluster Around Certain Tempos?",
    x = "BPM (Tempo)",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggplot(spotify_bpm, aes(x = popularity_group, y = BPM, fill = popularity_group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  scale_fill_manual(values = c("Top 10%" = "tomato", "Other 90%" = "steelblue")) +
  labs(
    title = "BPM Distribution of Songs by Popularity Group",
    x = "",
    y = "BPM"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Top 25% streamed songs
top_25 <- spotify_data %>%
  filter(Streams >= quantile(Streams, 0.75, na.rm = TRUE)) %>%
  mutate(BPM = as.numeric(BPM)) %>%
  filter(!is.na(Key), !is.na(BPM))

# Calculate BPM std dev per Key
bpm_diversity <- top_25 %>%
  group_by(Key) %>%
  summarise(
    bpm_sd = sd(BPM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(bpm_sd))

# Bar plot
ggplot(bpm_diversity, aes(x = reorder(Key, bpm_sd), y = bpm_sd)) +
  geom_col(fill = "orchid4") +
  coord_flip() +
  labs(
    title = "BPM Diversity by Musical Key (Top 25% Streamed Songs)",
    x = "Key",
    y = "Standard Deviation of BPM"
  ) +
  theme_minimal(base_size = 14)

# Clean and bin BPM
bpm_bins <- spotify_data %>%
  mutate(
    BPM = as.numeric(BPM),
    bpm_bin = cut(BPM, breaks = seq(60, 200, by = 10), right = FALSE)
  ) %>%
  filter(!is.na(bpm_bin), !is.na(Streams))

# Average streams per bin
bpm_streams <- bpm_bins %>%
  group_by(bpm_bin) %>%
  summarise(
    avg_streams = mean(Streams, na.rm = TRUE),
    .groups = "drop"
  )

# Bar chart
ggplot(bpm_streams, aes(x = bpm_bin, y = avg_streams)) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Average Streams by BPM Range (2023)",
    x = "BPM Range",
    y = "Average Streams"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sum total streams per artist
top_artists <- spotify_data %>%
  filter(!is.na(Artist)) %>%
  group_by(Artist) %>%
  summarise(
    total_streams = sum(Streams, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_streams)) %>%
  slice_max(total_streams, n = 15)

# Bar chart
ggplot(top_artists, aes(x = reorder(Artist, total_streams), y = total_streams)) +
  geom_col(fill = "mediumpurple") +
  coord_flip() +
  labs(
    title = "Top 15 Artists by Total Streams (2023)",
    x = "Artist",
    y = "Total Streams"
  ) +
  theme_minimal(base_size = 14)


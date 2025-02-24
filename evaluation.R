library(tidyverse)
library(jsonlite)
library(scales)
library(ggtext)
library(readr)


# Load the data
forecast <- fromJSON(file = "data/forecast.json") %>% bind_rows %>% 
  rename(party = "_row") 

# Add row party == "oth" which is 100% minus sum of value, name Sonstige
forecast <- bind_rows(forecast, data.frame(party = "oth", value = 100 - sum(forecast$value), name = "Andere")) 

result <- data.frame(
  party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "fw", "bsw", "oth"),
  result = c(28.5, 16.4, 11.6, 4.3, 8.8, 20.8, 1.5, 4.97, 3.0)
)



# Zweitstimmen 

## MAE

compare <- merge(forecast, result, by = "party") %>% filter(party != "oth")
mean(abs(compare$value - compare$result)) # 0.99625

## Plot of forecasted vs. actual votes

# Create the data frame
party_colors <- data.frame(
  alt_name = c("SPD", "CDU/CSU", "Grüne", "Linke", "FDP", "AfD", "BSW"),
  correct_color = c("#E3000F", "#000000", "#1AA037", "#C13197", "#FFEF00", "#0489DB", "#8037DE"),
  stringsAsFactors = FALSE
)

compare <- merge(compare, party_colors, by.x = "name", by.y = "alt_name")

compare <- compare %>% mutate(value_label = round(value, 1),
       value_label = ifelse(value_label %% 1 == 0, str_c(value_label, ".0"), value_label),
       value_label = str_replace_all(value_label, "\\.", ","))

compare %>% 
  ggplot(aes(x = reorder(name, -value), y = value, fill = name)) +
  # Add background bars for value_l1 and nudge them to the left
  geom_col(aes(y = result), alpha = 0.3) +
  geom_hline(yintercept = 5, linetype = "dotted", linewidth = .5, color = "black") +
  geom_linerange(aes(ymin = low, ymax = high, color = name), linewidth = 10, alpha = 0.7,
                 position=position_dodge(width=.5)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +  # Convert y-axis to percentages
  geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
  geom_point(size = 2, fill = "white", shape = 21) +
  geom_text(aes(label = value_label, y = high), vjust = -.5, size = 3, color = "black", fontface = "bold") +
  scale_color_manual(values = compare$correct_color, breaks = compare$name) +
  scale_fill_manual(values = compare$correct_color, breaks = compare$name) +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 8, face = "bold", color = "black"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    )
  ) +
  labs(
    x = NULL,
    y = "Zweitstimmenanteil",
    title = "Zweitstimme: Vorhersage und Ergebnis",
    # subtitle = paste0("Stand: ", format(Sys.Date(), '%d.%m.%Y')),
    caption = "<i>Erläuterung:</i> Zweitstimmenergebnisse bei der Bundestagswahl 2025 im Hintergrund. <br> Balken entsprechen 83%-Unsicherheitsintervallen (Ergebnis in 5/6 der Fälle innerhalb des Intervalls).<br>Quelle: <b>zweitstimme.org</b>, Lizenz: CC BY-SA 4.0")
ggsave("output/zweitstimmen.png", width = 7, height = 5, dpi = 300)


## Erststimmen

# Load district forecast
forecast_districts <- fromJSON(file = "data/forecast_districts.json") %>% bind_rows

# Load district results

kerg2 <- read_delim("data/kerg2.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE, 
                    skip = 9)


es_results <- kerg2 %>% filter(Gebietsart == "Wahlkreis" & Gruppenart != "System-Gruppe" & Stimme == 1) %>% 
  mutate(Prozent = Prozent %>% str_replace("\\,", ".") %>% as.numeric) %>% 
  group_by(Gebietsnummer) %>% 
  mutate(winner = Prozent == max(Prozent, na.rm = T))

es_results$party <- es_results$Gruppenname %>% str_replace_all(c("CDU" = "cdu", "CSU" = "cdu", "GRÜNE" = "gru", "SPD" = "spd", "AfD" = "afd", "Die Linke" = "lin", "FDP" = "fdp", "BSW" = "bsw"))

es_results$wkr <- es_results$Gebietsnummer %>% as.numeric()

## Share of correctly predicted districts

es_compare <- merge(forecast_districts %>% filter(winner) %>% select(value, low, high, wkr, party), es_results %>% filter(winner) %>%  select(Prozent, wkr, party), by = "wkr") %>% 
  mutate(correct = party.x == party.y)

es_compare %>% ungroup %>% 
  summarise(correct_share = mean(correct, na.rm = T)) # 90.3

es_compare %>% group_by(party.y) %>% 
  summarise(correct_share = mean(correct, na.rm = T))

# party.y correct_share
# <chr>           <dbl>
#   1 afd             1    
# 2 cdu             0.968
# 3 gru             0.917
# 4 lin             0.5  
# 5 spd             0.578

es_compare %>% ungroup %>% 
  summarise(correct_sum = sum(correct, na.rm = T)) # 270

## MAE
es_compare <- merge(forecast_districts %>% select(value, low, high, wkr, party), es_results %>% select(Prozent, wkr, party), by = c("wkr", "party"))

es_compare$error <- abs(es_compare$value - es_compare$Prozent)

mean(abs(es_compare$value - es_compare$Prozent)) # 2.21

es_compare %>% group_by(party) %>% 
  summarise(mae = mean(error)) 
# party   mae
# <chr> <dbl>
#   1 afd   2.76 
# 2 bsw   1.46 
# 3 cdu   2.26 
# 4 fdp   0.846
# 5 gru   3.15 
# 6 lin   2.45 
# 7 spd   1.90 

## Coverage
es_compare$covered <- es_compare$Prozent > es_compare$low & es_compare$Prozent < es_compare$high
es_compare$covered %>% mean # 83%

es_compare %>% group_by(party) %>% 
  summarise(coverage = mean(covered))

# party coverage
# <chr>    <dbl>
#   1 afd      0.963
# 2 bsw      0.886
# 3 cdu      0.970
# 4 fdp      0.896
# 5 gru      0.717
# 6 lin      0.519
# 7 spd      0.957


## Table of wrongly predicted districts: wkr, forecasted winner, actual winner, probability for actual winner

es_compare <- merge(forecast_districts %>% filter(winner) %>% select(wkr, wkr_name, party), es_results %>% filter(winner) %>%  select(Prozent, wkr, party), by = "wkr") %>% 
  mutate(correct = party.x == party.y) %>% filter(!correct)


es_compare <- merge(es_compare, select(forecast_districts, c(wkr, party, probability)), by.x = c("wkr", "party.y"), by.y = c("wkr", "party"))


es_compare %>% select(Wahlkreis = wkr, Name = wkr_name, Vorhergesagt = party.x, Gewonnen = party.y, "Wahrscheinlichkeit für Ereignis" = probability) %>% View
  # Make html table
  kable("html", escape = F, align = "c") 

## Maps for forecast and result

### 2. Define Color Scheme ---------------------------------

# Custom color scheme for political parties
primary_color <- list(
  "afd" = "#0489DB",  # AfD Blue
  "spd" = "#E3000F",  # SPD Red
  "cdu" = "#000000",  # CDU Black
  "fdp" = "#FFEF00",  # FDP Yellow
  "lin" = "#C13197",  # Left Purple
  "gru" = "#1AA037",  # Green
  "bsw" = "#8037DE",  # BSW Purple
  "oth" = "grey"      # Others Grey
)

### 3. Process Geographic Data -----------------------------

# Set random seed for reproducibility
set.seed(123)

# Filter for winning districts
gdf <- forecast_districts %>% 
  filter(winner == 1)

gdf$party %>% table
# afd cdu gru lin spd 
# 48 201  19   3  28

# Load and process shapefile data
wahlkreise_sf <- st_read("data/btw25_geometrie_wahlkreise_shp/btw25_geometrie_wahlkreise_shp.shp")

gdf_sf <- merge(wahlkreise_sf, gdf, by.x = "WKR_NR", by.y = "wkr")

### 4. Create Party Labels --------------------------------

# Compute districts won per party and create labels
party_labels <- gdf %>%
  group_by(party) %>%
  dplyr::summarize(won_districts = sum(winner, na.rm = TRUE)) %>%
  mutate(
    label = paste0(
      party %>% str_replace_all(c(
        "afd" = "AfD",
        "cdu" = "CDU/CSU",
        "bsw" = "BSW",
        "gru" = "Grüne",
        "spd" = "SPD",
        "lin" = "Linke"
      )),
      " (",
      won_districts,
      ")"
    )
  ) %>%
  pull(label, name = party)

### 5. Create District Map Plot ---------------------------

district_map <- ggplot(gdf_sf) +
  # Add district polygons
  geom_sf(
    aes(
      fill = factor(party),
      alpha = probability
    )
  ) +
  # Set color scheme
  scale_fill_manual(
    values = primary_color,
    name = "",
    labels = party_labels
  ) +
  # Customize theme
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Remove alpha legend
  guides(alpha = "none") +
  # Prevent extra space around plot
  coord_sf(expand = FALSE)

### 6. Save Plot -----------------------------------------

# Save as PNG
ggsave(
  filename = "output/figure_forecast_districts.png",
  plot = district_map,
  device = "png",
  dpi = 150,
  height = 10,
  width = 10,
  bg = "white"
)


gdf_sf <- merge(wahlkreise_sf, es_results %>% filter(winner), by.x = "WKR_NR", by.y = "wkr")
es_results$winner <- 1
### 4. Create Party Labels --------------------------------

es_results$party %>% table
# afd cdu gru lin spd 
# 46 190  12   6  45 


# Compute districts won per party and create labels
party_labels <- es_results %>%
  group_by(party) %>%
  dplyr::summarize(won_districts = sum(winner, na.rm = TRUE)) %>%
  mutate(
    label = paste0(
      party %>% str_replace_all(c(
        "afd" = "AfD",
        "cdu" = "CDU/CSU",
        "bsw" = "BSW",
        "gru" = "Grüne",
        "spd" = "SPD",
        "lin" = "Linke"
      )),
      " (",
      won_districts,
      ")"
    )
  ) %>%
  pull(label, name = party)

### 5. Create District Map Plot ---------------------------

district_map <- ggplot(gdf_sf) +
  # Add district polygons
  geom_sf(
    aes(
      fill = factor(party),
      alpha = Prozent
    )
  ) +
  # Set color scheme
  scale_fill_manual(
    values = primary_color,
    name = "",
    labels = party_labels
  ) +
  # Customize theme
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Remove alpha legend
  guides(alpha = "none") +
  # Prevent extra space around plot
  coord_sf(expand = FALSE)

### 6. Save Plot -----------------------------------------

# Save as PNG
ggsave(
  filename = "output/figure_result_districts.png",
  plot = district_map,
  device = "png",
  dpi = 150,
  height = 10,
  width = 10,
  bg = "white"
)



# Compare with others
es_results
forecast_districts


# Yougov

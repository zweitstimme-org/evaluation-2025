library(tidyverse)
library(jsonlite)
library(scales)
library(ggtext)
library(readr)

###########################################
# 1. Data Loading and Preparation
###########################################

# Load and prepare forecast data
forecast <- fromJSON(file = "data/forecast.json") %>% 
  bind_rows() %>% 
  rename(party = "_row")

# Add "others" category
forecast <- bind_rows(
  forecast, 
  data.frame(
    party = "oth", 
    value = 100 - sum(forecast$value), 
    name = "Andere"
  )
)

# Load actual election results
election_results <- data.frame(
  party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw", "oth"),
  result = c(28.5, 16.4, 11.6, 4.3, 8.8, 20.8, 4.97, 4.5)
)

###########################################
# 2. Second Vote Analysis (Zweitstimmen)
###########################################

# Calculate Mean Absolute Error (MAE)
vote_comparison <- merge(forecast, election_results, by = "party") %>% 
  filter(party != "oth")
mae_second_votes <- mean(abs(vote_comparison$value - vote_comparison$result))

# Define party colors for visualization
party_colors <- data.frame(
  alt_name = c("SPD", "CDU/CSU", "Grüne", "Linke", "FDP", "AfD", "BSW"),
  correct_color = c("#E3000F", "#000000", "#1AA037", "#C13197", "#FFEF00", "#0489DB", "#8037DE"),
  stringsAsFactors = FALSE
)

# Prepare data for plotting
plot_data <- merge(vote_comparison, party_colors, by.x = "name", by.y = "alt_name") %>%
  mutate(
    value_label = round(value, 1),
    value_label = ifelse(value_label %% 1 == 0, str_c(value_label, ".0"), value_label),
    value_label = str_replace_all(value_label, "\\.", ",")
  )

# Create visualization of forecasted vs actual votes
second_vote_plot <- plot_data %>% 
  ggplot(aes(x = reorder(name, -value), y = value, fill = name)) +
  geom_col(aes(y = result), alpha = 0.3) +
  geom_hline(yintercept = 5, linetype = "dotted", linewidth = .5, color = "black") +
  geom_linerange(
    aes(ymin = low, ymax = high, color = name), 
    linewidth = 10, 
    alpha = 0.7,
    position = position_dodge(width = .5)
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
  geom_point(size = 2, fill = "white", shape = 21) +
  geom_text(
    aes(label = value_label, y = high), 
    vjust = -.5, 
    size = 3, 
    color = "black", 
    fontface = "bold"
  ) +
  scale_color_manual(values = plot_data$correct_color, breaks = plot_data$name) +
  scale_fill_manual(values = plot_data$correct_color, breaks = plot_data$name) +
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
      size = 8,
      margin = margin(t = 5)
    )
  ) +
  labs(
    x = NULL,
    y = "Zweitstimmenanteil",
    title = "Zweitstimme: Vorhersage und Ergebnis",
    caption = "<i>Erläuterung:</i> Zweitstimmenergebnisse bei der Bundestagswahl 2025 im Hintergrund. <br> Balken entsprechen 83%-Unsicherheitsintervallen (Ergebnis in 5/6 der Fälle innerhalb des Intervalls).<br>Quelle: <b>zweitstimme.org</b>, Lizenz: CC BY-SA 4.0"
  )

ggsave("output/zweitstimmen.png", plot = second_vote_plot, width = 7, height = 5, dpi = 300)

###########################################
# 3. First Vote Analysis (Erststimmen)
###########################################

# Load district-level data
district_forecast <- fromJSON(file = "data/forecast_districts.json") %>% bind_rows()

# Load and prepare actual district results
district_results_raw <- read_delim(
  "data/kerg2.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE, 
  skip = 9
)

# Process district results
district_results <- district_results_raw %>% 
  filter(Gebietsart == "Wahlkreis" & Gruppenart != "System-Gruppe" & Stimme == 1) %>% 
  mutate(
    Prozent = Prozent %>% str_replace("\\,", ".") %>% as.numeric,
    party = Gruppenname %>% str_replace_all(c(
      "CDU" = "cdu", 
      "CSU" = "cdu", 
      "GRÜNE" = "gru", 
      "SPD" = "spd", 
      "AfD" = "afd", 
      "Die Linke" = "lin", 
      "FDP" = "fdp", 
      "BSW" = "bsw"
    )),
    wkr = Gebietsnummer %>% as.numeric()
  ) %>%
  group_by(Gebietsnummer) %>%
  mutate(winner = Prozent == max(Prozent, na.rm = T))

# Analysis of correctly predicted districts
district_prediction_accuracy <- merge(
  district_forecast %>% 
    filter(winner) %>% 
    select(value, low, high, wkr, party),
  district_results %>% 
    filter(winner) %>% 
    select(Prozent, wkr, party),
  by = "wkr"
) %>%
  mutate(correct = party.x == party.y)

# Calculate overall prediction accuracy
prediction_accuracy <- district_prediction_accuracy %>%
  ungroup() %>%
  summarise(
    correct_share = mean(correct, na.rm = T),
    correct_count = sum(correct, na.rm = T)
  )

# Calculate party-wise prediction accuracy
party_prediction_accuracy <- district_prediction_accuracy %>%
  group_by(party.y) %>%
  summarise(correct_share = mean(correct, na.rm = T))

# Calculate Mean Absolute Error for districts
district_mae <- merge(
  district_forecast %>% 
    select(value, low, high, wkr, party),
  district_results %>% 
    select(Prozent, wkr, party),
  by = c("wkr", "party")
) %>%
  mutate(error = abs(value - Prozent))

# Overall MAE
mean_district_mae <- mean(district_mae$error)

# Party-wise MAE
party_district_mae <- district_mae %>%
  group_by(party) %>%
  summarise(mae = mean(error))

# Calculate coverage (whether actual result falls within predicted interval)
district_coverage <- district_mae %>%
  mutate(covered = Prozent > low & Prozent < high)

# Overall coverage
mean_coverage <- mean(district_coverage$covered)

# Party-wise coverage
party_coverage <- district_coverage %>%
  group_by(party) %>%
  summarise(coverage = mean(covered))

## Table of wrongly predicted districts: wkr, forecasted winner, actual winner, probability for actual winner

es_compare <- merge(district_forecast %>% filter(winner) %>% select(wkr, wkr_name, party), 
                    district_results %>% filter(winner) %>%  select(Prozent, wkr, party), by = "wkr") %>% 
  mutate(correct = party.x == party.y) %>% filter(!correct)


es_compare <- merge(es_compare, select(district_forecast, c(wkr, party, probability)), by.x = c("wkr", "party.y"), by.y = c("wkr", "party"))


es_compare %>% select(Wahlkreis = wkr, Name = wkr_name, Vorhergesagt = party.x, Gewonnen = party.y, "Wahrscheinlichkeit für Ereignis" = probability) %>%  # %>% View
# Make html table
kable("html", escape = F, align = "c") 



###########################################
# 4. District Map Visualization
###########################################

# Helper function to create district maps
create_district_map <- function(data, color_scheme) {
  ggplot(data) +
    geom_sf(aes(fill = factor(party), alpha = Prozent)) +
    scale_fill_manual(values = color_scheme, name = "") +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(alpha = "none") +
    coord_sf(expand = FALSE)
}

# Define color scheme for parties
party_colors_map <- list(
  "afd" = "#0489DB",
  "spd" = "#E3000F",
  "cdu" = "#000000",
  "fdp" = "#FFEF00",
  "lin" = "#C13197",
  "gru" = "#1AA037",
  "bsw" = "#8037DE",
  "oth" = "grey"
)

# Load geographic data
wahlkreise_sf <- st_read("data/btw25_geometrie_wahlkreise_shp/btw25_geometrie_wahlkreise_shp.shp")

# Create forecast map
forecast_map_data <- merge(
  wahlkreise_sf, 
  district_forecast %>% filter(winner), 
  by.x = "WKR_NR", 
  by.y = "wkr"
) %>% 
  mutate(Prozent = value)

# Create and save forecast map
forecast_map <- create_district_map(forecast_map_data, party_colors_map)
ggsave(
  "output/figure_forecast_districts.png",
  plot = forecast_map,
  device = "png",
  dpi = 150,
  height = 10,
  width = 10,
  bg = "white"
)

# Create results map
results_map_data <- merge(
  wahlkreise_sf, 
  district_results %>% filter(winner), 
  by.x = "WKR_NR", 
  by.y = "wkr"
)

# Create and save results map
results_map <- create_district_map(results_map_data, party_colors_map)
ggsave(
  "output/figure_result_districts.png",
  plot = results_map,
  device = "png",
  dpi = 150,
  height = 10,
  width = 10,
  bg = "white"
)




# Compare with others

district_winner <- district_results %>% filter(winner) %>% ungroup %>% 
  select(wkr, true_winner = party)
district_winner


our_winner <- district_forecast %>% filter(winner) %>% select(wkr, wkr_name, our_winner = party) #  winner, value, probability
our_winner

## Yougov
yougov_forecast <- read_delim("data/YouGov_WK_20_02_2025.csv", 
                                   delim = "\t", escape_double = FALSE, 
                                   trim_ws = TRUE)

# yougov_winner <- yougov_forecast %>% mutate(
  # wkr = as.numeric(WK_ID),
#   cdu = UnionShare,
#   spd = SPDShare,
#   gru = GreenShare,
#   lin = LinkeShare,
#   afd = AfDShare,
#   fdp = FDPShare,
#   bsw = SonstigeShare
#   
# ) %>% select(wkr, cdu, spd, gru, lin, afd, fdp, bsw) %>% 
#   pivot_longer(cols = cdu:bsw, names_to = "yougov_winner", values_to = "value") %>% 
#   group_by(wkr) %>% 
#   filter(value == max(value)) %>% select(-value)
yougov_forecast$Winner25 %>% table

yougov_winner <- yougov_forecast %>% mutate(
  wkr = as.numeric(WK_ID),
  yougov_winner = case_when(Winner25 == "AfD" ~ "afd",
                            Winner25 == "Bündnis 90/Die Grünen" ~ "gru",
                            Winner25 == "CDU/CSU" ~ "cdu",
                            Winner25 == "Die Linke" ~ "lin",
                            Winner25 == "SPD" ~ "spd"),
  winner_2021 = case_when(Winner21 == "AfD" ~ "afd",
                          Winner21 == "Bündnis 90/Die Grünen" ~ "gru",
                          Winner21 == "CDU/CSU" ~ "cdu",
                          Winner21 == "Die Linke" ~ "lin",
                          Winner21 == "SPD" ~ "spd")
                          
  ) %>% select(wkr, yougov_winner, winner_2021)

yougov_winner

## Citizen forecast

load("data/erststimme_winner_cf.RData")
citizen_forecast_winner <- erststimme_winner %>% 
  mutate(wkr = as.numeric(wkr),
         citizen_winner = mode_winner) %>% 
  select(wkr, citizen_winner) %>% 
  mutate(
    citizen_winner = str_replace_all(citizen_winner, "CDU/CSU, SPD", "oth"),
    citizen_winner = str_replace_all(citizen_winner, "CDU/CSU, AfD", "oth"),
    citizen_winner = str_replace_all(citizen_winner, "AfD", "afd"),
    citizen_winner = str_replace_all(citizen_winner, "SPD", "spd"),
    citizen_winner = str_replace_all(citizen_winner, "CDU/CSU", "cdu"),
    citizen_winner = str_replace_all(citizen_winner, "DIE LINKE", "lin"),
    citizen_winner = str_replace_all(citizen_winner, "GRÜNE", "gru")
  )
citizen_forecast_winner$citizen_winner %>% table         
citizen_forecast_winner


## Wahlkreisprognose
wk_prognose <- read_csv("data/wk_prognose_calls_21_02_2025.csv")

wk_prognose <- wk_prognose %>% mutate(wkr = WK,
                                      party = str_extract(call, "(?<= für ).*") %>% str_replace_all(c("AFD" = "afd", "CDU/CSU" = "cdu", "GRÜNE" = "gru", "LINKE" = "lin", "SPD" = "spd"))) %>% 
  select(wkr, wk_prognose_winner = party) 

wk_prognose$wk_prognose_winner %>% table
wk_prognose


## Election.de
load("data/2025-02-21-election-de.RData")

election_de_results_df_2 %>% View

election_de_winnner <- election_de_results_df_2 %>% 
  rename(wkr = wkr_nr) %>% 
  group_by(wkr) %>% 
  filter(prob == max(prob)) %>% 
  select(wkr, election_de_winner = party) %>% 
  mutate(election_de_winner = str_replace_all(election_de_winner, c("AfD" = "afd", "CDU" = "cdu", "CSU" = "cdu", "DIE LINKE" = "lin", "GRÜNE" = "gru", "SPD" = "spd")))
  
election_de_winnner$election_de_winner %>% table



# Merge them all
all_winners <- 
  district_winner %>% 
  left_join(our_winner, by = "wkr") %>% 
  left_join(yougov_winner, by = "wkr") %>% 
  left_join(citizen_forecast_winner, by = "wkr") %>% 
  left_join(wk_prognose, by = "wkr") %>% 
  left_join(election_de_winnner, by = "wkr")

# Calculate the share of correctly predicted for each forecast
all_winners %>% 
  mutate(
    our_correct = true_winner == our_winner,
    yougov_correct = true_winner == yougov_winner,
    citizen_correct = true_winner == citizen_winner,
    wk_prognose_correct = true_winner == wk_prognose_winner,
    election_de_correct = true_winner == election_de_winner,
    winner_2021_correct = true_winner == winner_2021
  ) %>% 
  summarise(
    our_correct = mean(our_correct),
    yougov_correct = mean(yougov_correct),
    citizen_correct = mean(citizen_correct),
    wk_prognose_correct = mean(wk_prognose_correct),
    election_de_correct = mean(election_de_correct),
    winner_2021 = mean(winner_2021_correct)
  )
  
  all_winners %>% 
    mutate(
      our_correct = true_winner == our_winner,
      yougov_correct = true_winner == yougov_winner,
      citizen_correct = true_winner == citizen_winner,
      wk_prognose_correct = true_winner == wk_prognose_winner,
      election_de_correct = true_winner == election_de_winner,
      winner_2021 = true_winner == winner_2021
    ) %>% 
  summarise(
    "zweitstimme.org" = mean(our_correct),
    "Yougov" = mean(yougov_correct),
    "Bürger:innen-Vorhersage" = mean(citizen_correct),
    "Wahlkreisprognose" = mean(wk_prognose_correct),
    "election.de" = mean(election_de_correct),
    "Gewinner 2021" = mean(winner_2021)
  ) %>% 
  # Make ggplot, showing the share for each forecast as one bar each
  pivot_longer(cols = c("zweitstimme.org", "Yougov", "Bürger:innen-Vorhersage", "Wahlkreisprognose", "election.de", "Gewinner 2021"), names_to = "forecast", values_to = "share") %>%
  ggplot(aes(x = fct_reorder(forecast, share), y = share)) +
  geom_col() +
    # Add percentages as text at end of bars
  geom_text(aes(label = str_c(share*299, " von 299"), y = share - .05), color = "white") +
  labs(
    x = "",
    y = "Anteil korrekt vorhergesagter Wahlkreise",
    title = "Korrekt vorhergesagte Wahlkreise nach Prognosequelle",
  ) + 
    coord_flip() +
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
        size = 8,
        margin = margin(t = 5)
      )
    )
  
ggsave("output/forecast_comparison.png", width = 10, height = 6, dpi = 300)  












district_value <- district_results %>% ungroup %>% 
  select(wkr, party, true_value = Prozent)
district_value

district_value <- district_value %>% filter(party %in% c("cdu", "spd", "gru", "lin", "fdp", "afd", "bsw"))
district_value <- filter(district_value, !is.na(true_value))
district_value$party %>% table


district_value %>% group_by(wkr) %>% 
  summarise(true_value = 100 - sum(true_value, na.rm = TRUE)) %>% 
  mutate(party = "oth") %>% bind_rows(district_value) %>% arrange(wkr) -> district_value

saveRDS(district_value, "data/district_value.rds")

our_value <- district_forecast %>% select(wkr, wkr_name, party, our_value = value) #  winner, value, probability
our_value

## Yougov
yougov_forecast <- read_delim("data/YouGov_WK_20_02_2025.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

yougov_value <- yougov_forecast %>% mutate(
wkr = as.numeric(WK_ID),
  cdu = UnionShare,
  spd = SPDShare,
  gru = GreenShare,
  lin = LinkeShare,
  afd = AfDShare,
  fdp = FDPShare,
  bsw = SonstigeShare

) %>% select(wkr, cdu, spd, gru, lin, afd, fdp, bsw) %>%
  pivot_longer(cols = cdu:bsw, names_to = "party", values_to = "yougov_value")
yougov_value


# Merge them all
all_value <- 
  district_value %>% 
  left_join(our_value, by = c("wkr", "party")) %>% 
  left_join(yougov_value, by = c("wkr", "party"))

# Get MAE for our and for yougov
all_value %>% 
  mutate(
    our_mae = abs(true_value - our_value),
    yougov_mae = abs(true_value - yougov_value)
  )  %>%
  summarise(
    our_mae = mean(our_mae, na.rm = T),
    yougov_mae = mean(yougov_mae, na.rm = T)
  )






# Load and prepare forecast data
pred_vacant <- fromJSON(file = "data/pred_vacant.json") %>% 
  bind_rows() 

pred_vacant %>% head


(pred_vacant %>% filter(wkr %in% vacant_districts))$abandon_p %>% mean
(pred_vacant %>% filter(!(wkr %in% vacant_districts)))$abandon_p %>% mean


pred_vacant <- filter(pred_vacant, abandon_p > .5)
vacant_districts <- c(1, 14, 54, 58, 71, 151, 169, 181, 182, 183, 185, 202, 204, 206, 218, 243, 251, 259, 274, 275, 277, 282, 290)


pred_vacant$true_vacant <- pred_vacant$wkr %in% vacant_districts
mean(pred_vacant$true_vacant)

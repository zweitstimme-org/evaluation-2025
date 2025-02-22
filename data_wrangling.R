pacman::p_load("tidyverse")

# Erststimmen Data --------------------------------------------------------------

# YouGov - Erststimmenanteil WK - 20.02.2025
YG_WK_E <- read.csv("data/YouGov_WK_20_02_2025.csv", sep = "\t")

# election.de - numerische Gewinnwahrscheinlichkeiten WK - 21.02.2025
load("data/2025-02-21-election-de.RData")

# Wahlkreisprognose.de - Wahlkreisgewinner, mit Wortwahrscheinlichkeiten - 21.02.2025
wk_prognose_calls <- read.csv("data/wk_prognose_calls_21_02_2025.csv")


# Assemble ----------------------------------------------------------------

all_calls <- wk_prognose_calls
rename(all_calls, WK_No = WK)
# extract wk_prognose_calls
all_calls <- all_calls |> 
  mutate(WK_Prognose_Call = case_when(
    str_detect(call, "CDU/CSU") ~ "CDU/CSU",
    str_detect(call, "AFD") ~ "AfD",
    str_detect(call, "SPD") ~ "SPD",
    str_detect(call, "GRÜNE") ~ "Grüne",
    str_detect(call, "LINKE") ~ "Linke",
    str_detect(call, "BSW") ~ "BSW",
    str_detect(call, "FDP") ~ "FDP"
  ))
all_calls <- all_calls |> 
  mutate(WK_Prognose_Too_Close_to_Call = case_when(
    str_detect(call, "call") ~ TRUE,
    .default = FALSE
  ))
all_calls <- select(all_calls, -call)
all_calls <- election_de_results_df_2 |>
  group_by(wkr_nr) |>
  filter(prob==max(prob)) |>
  select(-candname, -prob) |>
  rename(election_de_call = party,
         WK_Name = wkr_name) |>
  ungroup() |>
  left_join(all_calls, by = join_by(wkr_nr == WK))

all_calls <- YG_WK_E |> 
  select(WK_ID, Winner25, YouGovCall, Winner21) |>
  mutate(YG_Too_Close_to_Call = case_when(
    str_detect(YouGovCall, "Unentschieden") ~ TRUE,
    .default = FALSE
  )) |>
  select(-YouGovCall) |>
  rename(YouGov_call = Winner25) |>
  left_join(all_calls, by = join_by(WK_ID == wkr_nr))

# Unify Party Name --------------------------------------------------------

all_calls <- all_calls %>%
  mutate(across(
    .cols = c(YouGov_call, Winner21, election_de_call, WK_Prognose_Call),    ~ . %>%
      str_replace("Bündnis 90/Die Grünen", "Grüne") %>%
      str_replace("^CDU$", "CDU/CSU") %>%
      str_replace("^CSU$", "CDU/CSU") %>%
      str_replace("DIE LINKE", "Linke") %>%
      str_replace("AFD", "AfD") %>%
      str_replace("GRÜNE", "Grüne")
  ))

write.csv(all_calls, "data/ES_Calls.csv", row.names = F)
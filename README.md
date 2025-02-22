# evaluation-2025

## Available Data

Where does this data come from? Raw data scraped from the original sources, dates should be in here for everything. More questions - @linusha hopefully knowns.
Numerical Wahlkreisprognose.de data by courtesy of @Paul-Elvis-Otto, election.de scraper by @simonmunzert.

### Ready to use Data

## 2nd vote
- `data/YouGov_ZS_Laender_Bund.csv` projected vote-share (2nd vote) on federal level and bund. High/Low only available at federal level. Last updated: 20.02.2025

## 1st vote

- `data/ES_Calls.csv` - final calls made by election.de, WK.prognose, YouGov, including information whether "Too close to call" where applicable. Winner21 is also included.

### Other tools to use

- `btw_extractor.py` - Generates a table of all first- and second-vote shares per party in all districs, given the official table from the Bundeswahlleitung. In the case that the table hasn't changed its format, this can be used to extract the result as soon as the table becomes available. See `BTW_21_Result_per_WK.csv` for example of last year. **Important: Uses "Tabelle: Ergebnisse nach Wahlkreisen (flacher Aufbau)"**.
- `benchmark_swings.R` - Calculates simple proportional/uniform swing based on national trend versus the 21 election result on WK level. Gives resulting calls for each WK. Trend can easily be exchanged for 25 result if needed. BSW is currently ignored.
    - output of this script with 21 results and latest DAWUM trend is in `benchmark_calls_national_trend_on_21.csv`
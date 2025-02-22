import pandas as pd

# Update Filename here!
d = pd.read_csv("data/w-btw21_kerg2.csv")

d = d[["Gebietsart","Gebietsnummer","Gebietsname", "Gruppenart", "Gruppenname", "Stimme", "Anzahl"]]

d = d[d.Gebietsart == "Wahlkreis"]
d = d[(d.Gruppenart == "Partei") | (d.Gruppenname == "Gültige")]

# List of Gruppenname values to exclude from aggregation
exclude_list = ['CDU', 'CSU', "SPD", "DIE LINKE", "AfD", "FDP", "Freie Wähler", "GRÜNE", "Gültige"] # We need Gültige to calculate % later
# Separate rows to be aggregated and those that should remain unchanged (Sonstige)
df_to_aggregate = d[~d['Gruppenname'].isin(exclude_list)]
df_to_keep = d[d['Gruppenname'].isin(exclude_list)]
df_aggregated = df_to_aggregate.groupby(['Gebietsname', 'Stimme', "Gebietsnummer"], as_index=False).agg({
    'Anzahl': 'sum',
})
df_aggregated['Gruppenname'] = 'Sonstige'

df = pd.concat([df_to_keep, df_aggregated], ignore_index=True)

df_gueltige = df[df['Gruppenname'] == 'Gültige'][['Gebietsname', 'Anzahl', "Stimme"]].rename(columns={'Anzahl': 'Anzahl_gueltig'})
df = df.merge(df_gueltige, on=['Gebietsname', "Stimme"], how='left')
df = df[df['Gruppenname'] != "Gültige"]

df["Prozent"] = ((df["Anzahl"]/df["Anzahl_gueltig"])*100).round(decimals = 1)

df = df.drop(["Gebietsart", "Gruppenart", "Anzahl", "Anzahl_gueltig"], axis=1)

df_pivot = df.pivot_table(index=["Gebietsnummer", "Gebietsname"], 
                           columns=["Gruppenname", "Stimme"], 
                           values="Prozent")

df_pivot.columns = [f"{col[0]}_Stimme{int(col[1])}" for col in df_pivot.columns]

df_pivot.reset_index(inplace=True)

df_pivot.to_csv("BTW_21_Result_per_WK.csv", index=False)

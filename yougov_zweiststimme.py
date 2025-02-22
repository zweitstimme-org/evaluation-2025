import json
import csv

with open("data/YouGov_ZS_Laender_20_02_2025.json") as json_data:
    d = json.load(json_data)
    states = ["SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH"]
    parties = ["union", "afd", "spd", "green", "linke", "other", "fdp", "bsw", "fw"]
    countries = []
    for key, value in d.items():
        country = {}
        country["name"] = states[int(key) - 1]
        
        for party in parties:
            print(party)
            print(value)
            el = [x for x in value["data"] if x["id"] == party][0]
            print(party)
            country[f"{party}_share"] = el["share"]
            country[f"{party}_high"] = el["high"]
            country[f"{party}_low"] = el["low"]
        countries.append(country)

    with open('data/YouGov_ZS_Laender_20_02_2025.csv', 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=None)
        for i, row in enumerate(countries):
            if i == 0:
                writer.fieldnames = row.keys()
                writer.writeheader()

            writer.writerow(row)
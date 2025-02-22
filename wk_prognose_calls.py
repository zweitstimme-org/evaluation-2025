import json
import csv

with open("data/wk_prognose_calls_21_02.json") as json_data:
    d = json.load(json_data)

    def remove_geo (wk):
        del wk["geojson"]
        return wk
    d = list(map(lambda wk: remove_geo(wk), d))
    
    def extract_calls (wk):
        return {
            "WK": wk["name"],
            "call": wk["value"][0]
        }

    calls = list(map(lambda wk: extract_calls(wk), d))

    with open('data/wk_prognose_calls_21_02_2025.csv', 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=["None"])
        for i, row in enumerate(calls):
            if i == 0:
                writer.fieldnames = row.keys()
                writer.writeheader()

            writer.writerow(row)
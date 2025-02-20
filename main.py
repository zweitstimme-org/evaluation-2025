import requests
import re
import json
import csv

def fetch_data(url):
    """
    Fetch the content from the URL and filter for lines containing '"metadata":'.
    Returns the filtered text as a single string.
    """
    response = requests.get(url)
    response.raise_for_status()
    lines = response.text.splitlines()
    # Only keep lines that include '"metadata":'
    filtered_lines = [line for line in lines if '"metadata":' in line]
    return "\n".join(filtered_lines)

def remove_geometry(text):
    """
    Remove geometry entries from the input text using a regex.
    
    The regex matches an optional preceding comma/whitespace, the key "geometry": 
    and its quoted value (allowing for escaped quotes).
    """
    pattern = r',?\s*"geometry":\s*"(?:\\.|[^"\\])*"'
    return re.sub(pattern, '', text)

def repair_json(text):
    """
    Repair a broken JSON snippet by:
      1. Removing the 'var' keyword.
      2. Converting variable assignments to JSON key–value pairs.
      3. Wrapping the entire content in curly braces.
      4. Removing extra commas that break the JSON structure.
    
    Returns the parsed JSON data if successful, or None if JSON remains invalid.
    """
    # 1. Remove "var" keyword.
    text = re.sub(r'\bvar\s+', '', text)
    # 2. Convert assignments (e.g. _Flourish_data = {...},) into key-value pairs.
    text = re.sub(r'(\S+)\s*=\s*', r'"\1": ', text)
    # 3. Wrap the content in curly braces so it becomes a single JSON object.
    text = "{" + text + "}"
    # 4. Remove extraneous commas:
    #    a. Comma immediately after an opening brace.
    text = re.sub(r'\{\s*,\s*', '{', text)
    #    b. Comma immediately after an opening bracket.
    text = re.sub(r'\[\s*,\s*', '[', text)
    #    c. Trailing commas before closing braces/brackets.
    text = re.sub(r',\s*([\}\]])', r'\1', text)
    text = text.strip()
    
    try:
        data = json.loads(text)
        return data
    except json.JSONDecodeError as e:
        print("Error: Fixed JSON is still invalid.")
        print(e)
        # Optionally, write out the tentative JSON text for inspection.
        with open("tentative_output.txt", "w", encoding="utf-8") as f:
            f.write(text)
        return None

def convert_json_to_csv_with_election_district(json_file, csv_file):
    """
    Convert the repaired JSON file into a CSV.
    
    The CSV is built from:
      - A filtered metadata header (excluding "WK-NR" and "WK-NAME")
      - The "Wahlkreis" column (from the "name" field)
      - An "ID" column (the numeric part from "Wahlkreis")
      - An "Election District" column (the remaining text from "Wahlkreis")
      - The "Tendenz aktuell" column (from the first element in "value")
    """
    # Load JSON data from file.
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    # Extract header information.
    col_meta = data["_Flourish_data_column_names"]["choropleth"]["metadata"]
    # Filter out unwanted columns "WK-NR" and "WK-NAME"
    col_meta_filtered = [col for col in col_meta if col not in ("WK-NR", "WK-NAME")]
    col_name = data["_Flourish_data_column_names"]["choropleth"]["name"]
    col_value = data["_Flourish_data_column_names"]["choropleth"]["value"][0]
    header = col_meta_filtered + [col_name, "ID", "Election District", col_value]
    
    # Extract each row from the choropleth data.
    rows = data["_Flourish_data"]["choropleth"]
    
    with open(csv_file, 'w', encoding='utf-8', newline='') as csv_out:
        writer = csv.writer(csv_out)
        writer.writerow(header)  # Write header row
        
        for row in rows:
            # Filter out unwanted metadata columns using the original metadata list order.
            row_meta = row["metadata"]
            row_meta_filtered = [value for value, col in zip(row_meta, col_meta) if col not in ("WK-NR", "WK-NAME")]
            
            # Extract the "Wahlkreis" value.
            wahlkreis_value = row["name"]  # e.g., "1 Flensburg – Schleswig"
            
            # Extract numeric ID and remaining text.
            match = re.match(r'(\d+)\s+(.*)', wahlkreis_value)
            if match:
                id_val = match.group(1)
                election_district = match.group(2)
            else:
                id_val = ""
                election_district = wahlkreis_value
            
            # Extract the value from "Tendenz aktuell" (first element of the value list).
            value_value = row["value"][0] if row["value"] else ""
            
            # Combine into a complete row.
            row_data = row_meta_filtered + [wahlkreis_value, id_val, election_district, value_value]
            writer.writerow(row_data)
    
    print(f"CSV file written to {csv_file}")

def main():
    # Step 1: Fetch data from URL and filter the lines with "metadata":
    url = "https://flo.uri.sh/visualisation/5986443/embed"
    print("Fetching data...")
    filtered_text = fetch_data(url)
    
    # Optionally, write the raw filtered data to a file for debugging.
    with open("output.txt", "w", encoding="utf-8") as f:
        f.write(filtered_text)
    print("Data fetched and written to output.txt")
    
    # Step 2: Remove geometry entries from the filtered text.
    print("Removing geometry entries...")
    cleaned_text = remove_geometry(filtered_text)
    # Optionally, write this intermediate output.
    with open("cleaned.txt", "w", encoding="utf-8") as f:
        f.write(cleaned_text)
    print("Geometry removed; cleaned data written to cleaned.txt")
    
    # Step 3: Repair the JSON format.
    print("Repairing JSON format...")
    repaired_data = repair_json(cleaned_text)
    if repaired_data is not None:
        # Write the final repaired JSON to output.json.
        with open("output.json", "w", encoding="utf-8") as f:
            json.dump(repaired_data, f, ensure_ascii=False, indent=2)
        print("Repaired JSON written to output.json")
    else:
        print("Failed to repair JSON; check tentative_output.txt for details.")
        return

    # Step 4: Convert JSON to CSV with Election District.
    convert_json_to_csv_with_election_district("output.json", "output.csv")

if __name__ == "__main__":
    main()

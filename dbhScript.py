import math
import pandas as pd
import numpy as np

## Constants
EQUATIONS = "equationSourcesDBH.csv"
TREES_AGE = "inputTreesDBH.csv" ## Input CSV with at least ID, Species, and Age
OUTPUT_CSV = "outputTreesDBH.csv" ## (TODO) ensure there is an empty CSV with this name to store output
MAPPED = "mappedTreesDBH.csv"

## read from CSVs
input_file = pd.read_csv(TREES_AGE)
input_file['New_DBH'] = ""
eq = pd.read_csv(EQUATIONS)
map = pd.read_csv(MAPPED)

## Turn input dataset columns into lists
age = input_file['AGE'].tolist()
species = input_file['Common Name'].tolist()

def main():
    results = [] ## refactored 
    for index in range(len(input_file)):
        ## get row from equations where input species == equations species
        mapped_row = map.loc[map["OriginalName"] == species[index]]
        new_species = mapped_row.iloc[0]['MappedName']
        row = eq.loc[eq['Species'] == new_species]
        if len(row.index) == 0:
            print("species not found in equations table", index)
            continue
        if index % 10 == 0:
            print('index', index)
        eq_name = row.iloc[0]["EqName"]
        if eq_name == 'cub':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            d = row.iloc[0]['d']
            result = a + (b * age[index]) + (c * age[index] ** 2) + (d * age[index] ** 3)
            # input_file.at[index, "New_DBH"] = result

        elif eq_name == 'loglogw1':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = math.exp(a + (b * np.log(np.log(age[index] + 1) + (c / 2)))) ## double check that c should be used
            # input_file.at[index, "New_DBH"] = result

        elif eq_name == 'quad':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = a + (b * age[index]) + (c * age[index] ** 2)
            # input_file.at[index, "New_DBH"] = result 

        elif eq_name == 'lin':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            result = a + (b * age[index])
            # input_file.at[index, "New_DBH"] = result 
            
        else: 
            print("Species has no correlating equation")
            continue

        results.append(result) ## refactored

    input_file['New_DBH'] = results ## refactored
    input_file.to_csv(OUTPUT_CSV, mode='w', header=True, index=False) ## refactored
        
        

if __name__ == "__main__":
    main()
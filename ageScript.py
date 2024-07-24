import math
import pandas as pd
import numpy as np

## Constants
EQUATIONS = "equationSources.csv"
TREES_DBH = "inputTrees.csv" ## Input CSV with at least ID, Species, and DBH
OUTPUT_CSV = "outputTreesAge.csv" ## (TODO) ensure there is an empty CSV with this name to store output
MAPPED = "mappedTrees.csv"

## read from CSVs
input_file = pd.read_csv(TREES_DBH)
input_file['AGE'] = ""
eq = pd.read_csv(EQUATIONS)
map = pd.read_csv(MAPPED)

## Prep output dataset
OUT_ID = 'ID'
OUT_SPECIES = 'Species'
OUT_DBH = 'DBH'
OUT_AGE = 'Age'

## Turn input dataset columns into lists
dbh = input_file['DBH'].tolist()
species = input_file['SPECIES_NA'].tolist()
age = input_file['AGE'].tolist()

def main():
    for index in range(len(input_file)):
        ## get row from equations where input species == equations species
        mapped_row = map.loc[map["OriginalName"] == species[index]]
        new_species = mapped_row.iloc[0]['MappedName']
        print(new_species)
        row = eq.loc[eq['Species'] == new_species]
        if len(row.index) == 0:
            print("species not found in equations table", index)
            continue
        if index % 10 == 0:
            print('index', index)
        if row.iloc[0]["EqName"] == 'cub':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            d = row.iloc[0]['d']
            result = a + (b * dbh[index]) + (c * dbh[index] ** 2) + (d * dbh[index] ** 3)
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result)

        elif row.iloc[0]["EqName"] == 'loglogw1':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = math.exp(a + (b * np.log(np.log(dbh[index] + 1) + (c / 2)))) ## double check that c should be used
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result)

        elif row.iloc[0]["EqName"] == 'loglogw4':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = math.exp(a + (b * np.log(np.log(dbh[index] + 1) + (dbh[index]**2) * (c / 2)))) ## double check that c should be used
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result)

        elif row.iloc[0]["EqName"] == 'quad':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = a + (b * dbh[index]) + (c * dbh[index] ** 2)
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result) 

        elif row.iloc[0]["EqName"] == 'lin':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            result = a + (b * dbh[index])
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result) 
        
        elif row.iloc[0]["EqName"] == 'expow1':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = math.exp(a + (b * dbh[index]) + (c / 2)) ## double check that c should be used
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result)
        
        elif row.iloc[0]["EqName"] == 'expow4':
            a = row.iloc[0]['a']
            b = row.iloc[0]['b']
            c = row.iloc[0]['c']
            result = math.exp(a + (b * dbh[index]) + ((dbh[index] ** 2) * (c / 2))) ## double check that c should be used
            # new_row = {OUT_ID: id[index], OUT_SPECIES: species[index], OUT_DBH: dbh[index], OUT_AGE: result}
            input_file.at[index, "AGE"] = round(result)
            
        else: 
            print("Species has no correlating equation")
            continue

        input_file.to_csv(OUTPUT_CSV, mode='w', header=True, index=False)
        
        

if __name__ == "__main__":
    main()
#==============================================
#   This script was used to combine excel sheets
#        from CBM-CFS3 forest stands
#           Robert Ilyes - Geog 481
#==============================================

# Install Packages if necessary
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("writexl")  # For writing the final table to an Excel file

# Import Libraries
library(readxl)
library(dplyr)
library(writexl)

# Define the directory containing the Excel files
folder_path <- "C://Users/robik/Downloads/CBM_Outputs/"

# List all Excel files in the directory
file_list <- list.files(path = folder_path, pattern = "*.xls", full.names = TRUE)

# Initialize an empty data frame to store the results
combined_data <- data.frame()

# Loop through each file
for (file in file_list) {
  # Here, we assume the ID is the number part of the file name before the extension
  id <- gsub(".*?(\\d+).*", "\\1", basename(file))  
  # Read the Excel file
  data <- read_excel(file)
  # Extract the first row of data (excluding the column names)
  first_row <- data %>%
    slice(1) %>%
    mutate(ID = id)  # Add the ID field
  # Append the first row to the combined data frame
  combined_data <- bind_rows(combined_data, first_row)
}

# View the combined data
print(combined_data)

# Write the combined data to a new Excel file
write_xlsx(combined_data, "C://Users/robik/Downloads/CBM_Outputs/combined_data.xlsx")

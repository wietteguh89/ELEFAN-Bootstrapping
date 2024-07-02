######################                  Simple Bootstrapping in R                     ##################
###                                       Wiwiet Teguh Taufani                                       ###
###                   Graduate School of Global Food Resources, Hokkaido University                  ###
### Department of Aquatic Resources, Faculty of Fisheries and Marine Science, Universitas Diponegoro ###
### last updated January 2023

# Install necessary packages
install.packages("infer")
install.packages("dplyr")
install.packages("boot")
install.packages("writexl")
install.packages("readxl")

# Load the libraries
library(infer)      # For statistical inference
library(dplyr)      # For data manipulation
library(boot)       # For bootstrapping functions
library(writexl)    # For writing Excel files
library(readxl)     # For reading Excel files

##### DATA PREPARATION #####

# Set the working directory to the specified path
setwd("your directory")

# Read data from an Excel file
data <- read_xlsx("your excell file.xlsx")

# Perform bootstrapping on the data
boot_data <- data %>% 
  bootstraps(times = 1000, apparent = FALSE) %>%        # Create 1000 bootstrap samples of the data
  mutate(splits = purrr::map(splits, analysis)) %>%     # Apply the analysis function to each bootstrap sample
  unnest(splits) %>%                                    # Unnest the list-column containing the bootstrap samples
  group_by(id) %>%                                      # Group the data by the bootstrap sample ID
  group_split() %>%                                     # Split the grouped data into a list of data frames, one for each bootstrap sample
  bind_cols()                                          # Combine the list of data frames into a single data frame

# Write the bootstrapped data to an Excel file
write_xlsx(boot_data, path = "Output_boot_data.xlsx")



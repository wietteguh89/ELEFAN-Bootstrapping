######################                      Kimura Plot in R                          ##################
###                                       Wiwiet Teguh Taufani                                       ###
###                   Graduate School of Global Food Resources, Hokkaido University                  ###
### Department of Aquatic Resources, Faculty of Fisheries and Marine Science, Universitas Diponegoro ###
### last updated June 2024


###################### FIX #############################

# Install necessary packages 
# install.packages("dplyr")
# install.packages("readr")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("ggplot2")
# install.packages("minpack.lm")
# install.packages("ggExtra")

# Load the required libraries
library(dplyr)      # For data manipulation
library(readr)      # For reading CSV files
library(gridExtra)  # For arranging multiple grid-based plots
library(grid)       # For low-level grid graphics
library(ggplot2)    # For creating plots
library(minpack.lm) # For non-linear least squares fitting
library(ggExtra)    # For adding marginal histograms/boxplots/density plots

# Set the working directory to the specified path
setwd("your directory")

# Read data from a CSV file
data <- read_csv("your csv file.csv")  # Replace with the path to your CSV file

# Display the first few rows of the data to ensure it has been read correctly
head(data)
unique(data$No)  # Display unique values in the 'No' column

# Separate scenarios into two subsets
data_subset1 <- data %>% filter(No %in% c("3", "6", "12", "18", "24"))  # Filter rows with specific scenarios
data_subset2 <- data %>% filter(!No %in% c("3", "6", "12", "18", "24")) # Filter rows without those scenarios

# Create exponential models using nlsLM
exp_mod <- nlsLM(K ~ a * exp(b * L_inf), data = data, start = list(a = 1, b = 0.01))            # Model for all data
exp_mod_a <- nlsLM(K ~ a * exp(b * L_inf), data = data_subset1, start = list(a = 1, b = 0.01))  # Model for subset1
exp_mod_b <- nlsLM(K ~ a * exp(b * L_inf), data = data_subset2, start = list(a = 1, b = 0.01))  # Model for subset2

# Display summaries of the models
summary(exp_mod)
summary(exp_mod_a)
summary(exp_mod_b)

# Get predicted values from the models
data$predicted_K <- predict(exp_mod)               # Predictions for all data
data_subset1$predicted_K <- predict(exp_mod_a)     # Predictions for subset1
data_subset2$predicted_K <- predict(exp_mod_b)     # Predictions for subset2

# Convert 'No' column to a factor with ordered levels
data$No <- factor(data$No, levels = c("24", "18", "12", "6", "3", "Ev 2", "Ev 3", "Ev 4", "Ev 5", "Ev 6"))
data_subset1$No <- factor(data_subset1$No, levels = c("24", "18", "12", "6", "3"))
data_subset2$No <- factor(data_subset2$No, levels = c("Ev 2", "Ev 3", "Ev 4", "Ev 5", "Ev 6"))

# Define custom colors for each level of 'No'
custom_colors <- c("24" = "blue", "18" = "green", "12" = "purple", "6" = "red", "3" = "black", 
                   "Ev 2" = "yellow", "Ev 3" = "orange", "Ev 4" = "lightblue", "Ev 5" = "lightgreen", "Ev 6" = "pink")
custom_colors_a <- c("24" = "blue", "18" = "green", "12" = "purple", "6" = "red", "3" = "black")
custom_colors_b <- c("Ev 2" = "yellow", "Ev 3" = "orange", "Ev 4" = "lightblue", "Ev 5" = "lightgreen", "Ev 6" = "pink")

# Create the Kimura plot
kimura_plot <- ggplot(data, aes(x = L_inf, y = K, color = No)) +
  geom_point() +  # Add data points
  geom_line(aes(y = predicted_K), color = "red") +  # Add the predicted line from the exponential model
  scale_color_manual(name = "Scenario", values = custom_colors) +  # Manually set colors for the 'No' levels
  labs(title = expression("L"[infinity]*" vs K All scenarios"),  # Add plot title with LaTeX-like formatting
       x = expression("L"[infinity]*" (cmFL)"),                  # Add x-axis label with LaTeX-like formatting
       y = expression("K (y"^{-1}*")")) +                       # Add y-axis label with superscript
  theme_minimal() +  # Use a minimal theme for the plot
  theme(axis.title = element_text(size = 14),   # Set axis title text size
        axis.text = element_text(size = 12),    # Set axis label text size
        axis.line = element_line(color = "black"),  # Add axis lines
        panel.grid = element_blank(),           # Remove panel grid
        panel.background = element_blank(),     # Remove panel background
        plot.background = element_blank(),      # Remove plot background
        legend.position = c(0.9, 0.75),         # Set legend position
        legend.background = element_blank(),    # Remove legend background
        legend.box.background = element_blank(),# Remove legend box background
        legend.key = element_blank()) +         # Remove legend key background
  scale_x_continuous(limits = c(125, 250), breaks = seq(125, 250, 25)) +  # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0.0, 1.5), breaks = seq(0.0, 1.5, 0.5)) + # Set y-axis limits and breaks
  guides(color = guide_legend(ncol = 2))  # Display legend in two columns

# Add histograms to the x-axis and y-axis using ggExtra
kimura_plot_with_histograms <- ggMarginal(kimura_plot, type = "histogram", bins = 30, groupFill = TRUE)

# Display the plot
print(kimura_plot_with_histograms)


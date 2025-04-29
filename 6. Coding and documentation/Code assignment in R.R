# Data Cleaning and Analysis Script for Occupational Task Intensity
# This script processes O*NET task data and Eurostat employment data
# to analyze Non-Routine Cognitive Analytical (NRCA) task intensity across countries

# Set working directory and load required packages
setwd("Z:\\File folders\\Teaching\\Reproducible Research\\2023\\Repository\\RRcourse2023\\6. Coding and documentation")
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualization
library(Hmisc)       # For weighted mean and variance calculations

# 1. Define Helper Functions -------------------------------------------------

#' Standardize variables by country using weighted mean and variance
#' 
#' @param data The dataframe containing the data
#' @param var The variable to standardize
#' @param weight The weighting variable
#' @param prefix Prefix for the new standardized variable
#' @return A dataframe with the new standardized variable
standardize_var <- function(data, var, weight, prefix = "std") {
  var_name <- sym(var)
  weight_name <- sym(weight)
  
  data %>%
    mutate(!!paste(prefix, weight, var, sep = "_") := 
             (!!var_name - wtd.mean(!!var_name, !!weight_name)) / 
             sqrt(wtd.var(!!var_name, !!weight_name)))
}

# 2. Data Import and Processing ---------------------------------------------

# 2.1 Import O*NET task data
task_data <- read.csv("Data/onet_tasks.csv") %>%
  mutate(
    # Extract first digit of ISCO-08 occupation code
    isco08_1dig = as.numeric(str_sub(isco08, 1, 1))
  ) 

# Aggregate task data by 1-digit ISCO occupation level
aggdata <- task_data %>%
  group_by(isco08_1dig) %>%
  summarise(across(starts_with("t_"), mean, na.rm = TRUE))

# 2.2 Import Eurostat employment data
# Define countries to analyze (easily extensible)
countries <- c("Belgium", "Spain", "Poland")

# Read and combine all ISCO worksheets
all_data <- map_dfr(1:9, ~{
  read_excel("Data/Eurostat_employment_isco.xlsx", 
             sheet = paste0("ISCO", .x)) %>%
    mutate(ISCO = .x) %>%  # Add ISCO category
    select(TIME, ISCO, all_of(countries))  # Select relevant columns
})

# Calculate total employment and shares for each country
all_data <- all_data %>%
  group_by(TIME) %>%
  mutate(
    across(
      all_of(countries),
      list(
        total = ~sum(.x),        # Total employment per country-period
        share = ~.x/sum(.x)      # Occupation share per country-period
      ),
      .names = "{.fn}_{.col}"
    )
  ) %>%
  ungroup()

# 3. Data Merging and Transformation ----------------------------------------

# Merge employment data with task data
combined <- all_data %>%
  left_join(aggdata, by = c("ISCO" = "isco08_1dig"))

# 4. Standardization Process -----------------------------------------------

# Define cognitive task variables of interest
cognitive_tasks <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")  # Analyzing data, creative thinking, interpreting information

# Standardize tasks and compute NRCA index for each country
for(country in countries) {
  # Standardize each cognitive task variable
  for(task in cognitive_tasks) {
    combined <- standardize_var(
      combined, 
      task, 
      paste0("share_", country), 
      "std"
    )
  }
  
  # Compute NRCA index (sum of standardized cognitive tasks)
  std_cols <- paste0("std_share_", country, "_", cognitive_tasks)
  combined <- combined %>%
    mutate(!!paste0("NRCA_", country) := rowSums(select(., all_of(std_cols))))
  
  # Standardize the NRCA index
  combined <- standardize_var(
    combined, 
    paste0("NRCA_", country), 
    paste0("share_", country), 
    "std"
  )
}

# 5. Compute Country-Level Weighted NRCA Index ------------------------------

country_nrca <- map_dfr(countries, ~{
  combined %>%
    group_by(TIME) %>%
    summarise(
      country = .x,
      nrca_index = wtd.mean(
        get(paste0("std_share_", .x, "_NRCA_", .x)), 
        get(paste0("share_", .x))
      ),
      .groups = "drop"
    )
})

# 6. Data Visualization ----------------------------------------------------

# Create time series plot of NRCA index for all countries
nrca_plot <- ggplot(country_nrca, 
                    aes(x = TIME, y = nrca_index, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Non-Routine Cognitive Analytical (NRCA) Task Intensity",
    subtitle = "Standardized Index Over Time",
    x = "Time Period",
    y = "NRCA Index",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(nrca_plot)

# 7. Data Quality Checks ---------------------------------------------------

# 7.1 Check for missing values
missing_values <- combined %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count")

# 7.2 Examine data distributions
data_distribution <- combined %>%
  select(ISCO, TIME, starts_with("share_"), starts_with("NRCA_")) %>%
  summary()

# Print quality check results
cat("Missing values check:\n")
print(missing_values)
cat("\nData distribution summary:\n")
print(data_distribution)

# 8. Output Results --------------------------------------------------------

# Save processed data
write.csv(combined, "Data/processed_task_data.csv", row.names = FALSE)
write.csv(country_nrca, "Data/country_nrca_index.csv", row.names = FALSE)

# Save the visualization
ggsave("Output/nrca_trends.png", plot = nrca_plot, 
       width = 10, height = 6, dpi = 300)

# Print completion message
cat("\nScript completed successfully. Results saved to Data/ and Output/ folders.\n")


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment


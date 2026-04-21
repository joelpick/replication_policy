# Load packages
library(dplyr)
library(stringr)

# Load the CSV file
journal_data <- read.csv("journal_data.csv")

# Check structure
str(journal_data)
head(journal_data)
nrow(journal_data)

# Convert data to numeric
journal_data <- journal_data %>%
  mutate(
    # Remove commas and convert to numeric
    Total.Citations = as.numeric(str_replace_all(Total.Citations, ",", "")),

    # Convert JIF
    X2023.JIF = as.numeric(X2023.JIF),

    # Remove % and convert OA
    X..of.Citable.OA = as.numeric(str_remove(X..of.Citable.OA, "%"))
  )

# Check quartiles
table(journal_data$JIF.Quartile)
prop.table(table(journal_data$JIF.Quartile))

# Basic stats for Total citations, JIF, and % OA

stats <- journal_data %>%
  summarise(
    # Total Citations
    citations_mean   = mean(Total.Citations, na.rm = TRUE),
    citations_median = median(Total.Citations, na.rm = TRUE),
    citations_sd     = sd(Total.Citations, na.rm = TRUE),
    citations_q1     = quantile(Total.Citations, 0.25, na.rm = TRUE),
    citations_q3     = quantile(Total.Citations, 0.75, na.rm = TRUE),
    citations_iqr    = IQR(Total.Citations, na.rm = TRUE),

    # JIF
    jif_mean   = mean(X2023.JIF, na.rm = TRUE),
    jif_median = median(X2023.JIF, na.rm = TRUE),
    jif_sd     = sd(X2023.JIF, na.rm = TRUE),
    jif_q1     = quantile(X2023.JIF, 0.25, na.rm = TRUE),
    jif_q3     = quantile(X2023.JIF, 0.75, na.rm = TRUE),
    jif_iqr    = IQR(X2023.JIF, na.rm = TRUE),

    # % OA
    oa_mean   = mean(X..of.Citable.OA, na.rm = TRUE),
    oa_median = median(X..of.Citable.OA, na.rm = TRUE),
    oa_sd     = sd(X..of.Citable.OA, na.rm = TRUE),
    oa_q1     = quantile(X..of.Citable.OA, 0.25, na.rm = TRUE),
    oa_q3     = quantile(X..of.Citable.OA, 0.75, na.rm = TRUE),
    oa_iqr    = IQR(X..of.Citable.OA, na.rm = TRUE)
  )

stats
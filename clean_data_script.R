# Load package #
library(tidyverse)

# Load data #
nsdca <- read_csv(here::here("data", "raw", "nsdca.csv"))

# Clean column names #
nsdca <- nsdca %>%
	rename(
		region = `California Region`,
		disease_type = `Disease Type`,
		cases = `Case\nCount`,
		screened = `Number\nScreened`,
		percent_region = `Percent\nof All\nDisorders\nin Region`
	) %>%
	# Convert percent column to numeric & create factor for categorical variable: region #
	mutate(
		percent_region = as.numeric(percent_region),
		region = factor(region, labels = c("Los Angeles", "Central Coast", "Northern Central Valley",
																			 "Northern, Mountain, and Desert Areas", "San Francisco Bay Area",
																			 "South Coast", "Southern Inland Empire")
		))

# Remove rows with missing values #
nsdca_cleaned <- nsdca %>%
	filter(!is.na(percent_region))

# Display the structure of the cleaned data frame #
str(nsdca_cleaned)

# Save the cleaned data to a new CSV file #
write_csv(nsdca_cleaned, "cleaned_nsdca.csv")

# Create data/clean folder #
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

# Save the complete-case data #
write_rds(nsdca_cleaned, here::here("data", "clean", "nsdca_cleaned_complete.rds"))

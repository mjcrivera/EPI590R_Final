install.packages("gtsummary", dependencies = TRUE)
library(tidyverse)
library(gtsummary)

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
	# Convert percent column to numeric & create factors for categorical variables #
	mutate(
		percent_region = as.numeric(percent_region),
		region = factor(region, labels = c("Los Angeles", "Central Coast", "Northern Central Valley",
																			 "Northern, Mountain, and Desert Areas", "San Francisco Bay Area",
																			 "South Coast", "Southern Inland Empire")
		))

# Remove rows with missing values #
nsdca_cleaned <- nsdca %>%
	filter(!is.na(percent_region))

# 1. Create a {gtsummary} table of descriptive statistics #
# The table will show descriptive statistics for 'Case Count', 'Number Screened', #
# and 'Percent of All Disorders in Region' including an overall column. #
tbl_summary(
	nsdca_cleaned,
	by = region,
	include = c(region, cases, screened, percent_region),
	label = list(
		cases ~ "Case Count",
		screened ~ "Number Screened",
		percent_region ~ "Percent of All Disorders in Region"
	)
) %>%
	add_overall() %>%
	modify_table_styling(
		columns = label,
		rows = label == "Percent of All Disorders in Region",
		footnote = "see https://catalog.data.gov/dataset/newborn-screening-disorders-426e4"
	)

# 2. Fit a regression and present well-formatted results #
# Fit a linear regression model predicting 'Case Count' from 'Number Screened' #
linear_model <- lm(cases ~ screened, data = nsdca_cleaned)
# Present the regression results using {gtsummary} #
tbl_regression(linear_model,
							 intercept = TRUE,
							 label = list(screened ~ "Number Screened"))

# 3. Create a figure #
# Create a histogram of the 'Case Count' variable to visualize its distribution.
histogram_figure <- ggplot(nsdca_cleaned, aes(x = cases)) +
	geom_histogram(binwidth = 25, fill = "lightblue", color = "black") +
	labs(
		title = "Histogram of Case Counts",
		x = "Case Count",
		y = "Frequency"
	)

# 4. Write and use a function #
# This function calculates the mean and standard deviation for a given numeric variable.
calculate_stats <- function(data, variable) {
	data %>%
		summarise(
			mean = mean({{ variable }}, na.rm = TRUE),
			sd = sd({{ variable }}, na.rm = TRUE)
		)
}

# Use the function to calculate stats for 'Case Count' and store the results in a list
case_count_stats <- calculate_stats(nsdca_cleaned, cases)

case_count_stats

screened_stats <- calculate_stats(nsdca_cleaned, screened)

screened_stats

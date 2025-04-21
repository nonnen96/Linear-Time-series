# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Load the dataset
df <- read.csv("valeurs_mensuelles.csv", sep=";", dec=",", stringsAsFactors=FALSE)

# Preview data
head(df)

# Parse date and value
df$Date <- as.Date(df$DATE, format="%Y-%m-%d")
df$Value <- as.numeric(gsub(",", ".", df$VALEUR))

# Create a time series object
start_year <- as.numeric(format(min(df$Date), "%Y"))
start_month <- as.numeric(format(min(df$Date), "%m"))
ice_ts <- ts(df$Value, start=c(start_year, start_month), frequency=12)

# Plot the raw time series
autoplot(ice_ts) + ggtitle("Fabrication de glaces et sorbets (raw)") +
  ylab("Indice de Production (base 100 en 2015)") + xlab("Année")

# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Load the dataset - the file has a complex structure with multiple columns
df <- read.csv("valeurs_mensuelles.csv", sep=";", dec=".", stringsAsFactors=FALSE)

# Preview data
head(df)

# Convert the second column to numeric and create a time series object
df[, 2] <- as.numeric(df[, 2])
time_series <- ts(df[, 2])

# Plot the time series
plot(time_series, type = "l", col = "blue", lwd = 2, 
  xlab = "Time", ylab = "Values", 
  main = "Evolution of Values in the Second Column")

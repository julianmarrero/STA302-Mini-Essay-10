# Load necessary libraries
library(readxl)
library(dplyr)
library(stats)
library(MASS)  # For Negative Binomial Regression

# Read the dataset
file_path <- "../Data/2022_Toronto_Poll_By_Poll_All_Offices.xlsx"
df <- read_excel(file_path)

# Preliminary data cleaning and preparation
# Focusing on a single candidate across subdivisions

# Select data for a single candidate as an example
candidate_data <- df %>% 
  filter(Candidate == "Tory John") %>%
  select(Subdivision, VoteCount)

# Convert Subdivision to a factor if it's not already
candidate_data$Subdivision <- as.factor(candidate_data$Subdivision)

# Fit a Poisson Regression Model
poisson_model <- glm(VoteCount ~ Subdivision, data=candidate_data, family="poisson")
summary(poisson_model)

# Check for overdispersion
# Compute dispersion parameter
dispersion_param <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(paste("Dispersion Parameter: ", dispersion_param))

# If dispersion parameter significantly greater than 1, consider Negative Binomial Regression
if(dispersion_param > 1.5) {  # Adjust threshold based on your criteria
  print("Fitting Negative Binomial Model due to overdispersion")
  negbin_model <- glm.nb(VoteCount ~ Subdivision, data=candidate_data)
  summary(negbin_model)
} else {
  print("Poisson Model is sufficient")
}

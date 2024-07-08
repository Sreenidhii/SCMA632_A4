# Load necessary libraries
library(readr)
library(broom)
library(ggplot2)

# Load the pizza data
df <- read_csv("D:\\CHRIST\\Boot camp\\DATA\\pizza_data.csv")

# Fit the linear model
model_fit <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = df)

# Print the summary of the model
print(tidy(model_fit))

# Conjoint attributes
conjoint_attributes <- c("brand", "price", "weight", "crust", "cheese", "size", "toppings", "spicy")

# Initialize lists to store results
level_name <- list()
part_worth <- list()
part_worth_range <- c()
important_levels <- list()

# Loop through each conjoint attribute
for (item in conjoint_attributes) {
  # Get the unique levels of the attribute
  levels <- unique(df[[item]])
  
  # Store the levels
  level_name <- c(level_name, list(levels))
  
  # Get the coefficients of the attribute
  coeffs <- coef(model_fit)[grep(item, names(coef(model_fit)))]
  
  # Calculate the part worths
  part_worths <- coeffs
  part_worths <- c(part_worths, -sum(part_worths))
  
  # Store the part worths
  part_worth <- c(part_worth, list(part_worths))
  
  # Calculate the importance of the attribute
  part_worth_range <- c(part_worth_range, max(part_worths) - min(part_worths))
  
  # Store the important level
  important_levels <- c(important_levels, list(which.max(part_worths)))
}

# Calculate the relative importance of each attribute
attribute_importance <- round(100 * part_worth_range / sum(part_worth_range), 2)

# Print the results
print("Level names:")
print(level_name)

print("Part worths:")
print(part_worth)

print("Important levels:")
print(important_levels)

print("Part worth range:")
print(part_worth_range)

print("Attribute importance:")
print(attribute_importance)

# Create a dictionary to store part worths
part_worth_dict <- list()
for (i in 1:length(conjoint_attributes)) {
  for (j in 1:length(level_name[[i]])) {
    temp_list <- list(part_worth[[i]][j])
    names(temp_list) <- level_name[[i]][j]
    part_worth_dict <- c(part_worth_dict, temp_list)
  }
}

print("Part worth dictionary:")
print(part_worth_dict)

# Create a dictionary to store attribute levels
attrib_level <- list()
for (i in 1:length(conjoint_attributes)) {
  temp_list <- list(level_name[[i]])
  names(temp_list) <- conjoint_attributes[i]
  attrib_level <- c(attrib_level, temp_list)
}

# Plot the attribute importance
ggplot(data.frame(Attribute = conjoint_attributes, Importance = attribute_importance), 
       aes(x = Attribute, y = Importance)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Relative importance of attributes", x = "Attributes", y = "Importance")

# Calculate the utility for each pizza
utility <- rowSums(do.call(cbind, lapply(conjoint_attributes, function(attr) {
  sapply(df[[attr]], function(level) part_worth_dict[[level]])
})))

print("Utility values:")
print(utility)

# Paths to the input files
tif <- rast(system.file("extdata", "rgb_cir_stack_2020.tif", package = "classifieR")) # replace with your file path
training_sites <- st_read("C:/Users/konst/Documents/Master/classifieR/inst/extdata/training.gpkg") # replace with your file path

# Select indices to calculate
selected_indices <- c("NDVI", "SAVI")

# Calculate indices
indices_stack <- calculate_indices(tif$rgb_2020_1, tif$rgb_2020_2,
                                   tif$rgb_2020_3, tif$cir, selected_indices)

# Extract training data
training_data <- extract(indices_stack, training_sites)
training_data <- na.omit(training_data)

# Train Random Forest model
rf_model <- randomForest(training_data[,-1], as.factor(training_data[,1]))
print(rf_model)

# Perform validation (e.g., using cross-validation or test data)
# This is a placeholder for actual validation code
validation_results <- "Validation results here"
print(validation_results)

# Calculate Area of Applicability (AoA)
# This is a placeholder for actual AoA calculation code
aoa_results <- "Area of Applicability results here"
print(aoa_results)

houses <- read.csv("melb_data_cleaned.csv")

run_linear_regression <- function(data, target, predictors, test_size = 0.2, seed = NULL) {
  # Split data into training and testing sets
  if (!is.null(seed)) {
    set.seed(seed)
  }
  train_index <- sample(1:nrow(data), (1 - test_size) * nrow(data))  # (1 - test_size)% for training
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Fit linear regression model
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Summary of the regression model
  print(summary(model))
  
  # Make predictions on the testing set
  predictions <- predict(model, newdata = test_data)
  
  # Evaluate model performance
  # Calculate RMSE
  rmse <- sqrt(mean((test_data[[target]] - predictions)^2))
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
  # Calculate MAE
  mae <- mean(abs(test_data[[target]] - predictions))
  cat("Mean Absolute Error (MAE):", mae, "\n")
  
  return(list(model = model, rmse = rmse, mae = mae))
}


# ------------------------------------------------------------------------------
# - Price Regression (using numerical attributes only)
# ------------------------------------------------------------------------------

result <- run_linear_regression(
  data = houses, 
  target = "Price", 
  predictors = c("Rooms", "Bedroom2", "Bathroom", "Car", "Landsize", "Distance"), 
  test_size = 0.2, 
  seed = 123
)

# ------------------------------------------------------------------------------
# - Price Regression (with house type)
# ------------------------------------------------------------------------------

one_hot_encoded <- model.matrix(~ Type - 1, data = houses)
one_hot_encoded_df <- as.data.frame(one_hot_encoded)
colnames(one_hot_encoded_df) <- gsub("Type", "", colnames(one_hot_encoded_df))
houses <- cbind(houses, one_hot_encoded_df)
houses <- subset(houses, select = -Type)

result <- run_linear_regression(
  data = houses, 
  target = "Price", 
  predictors = c("Rooms", "Bedroom2", "Bathroom", "Car", "Landsize", "Distance", "h", "t", "u"), 
  test_size = 0.2, 
  seed = 123
)

# ------------------------------------------------------------------------------
# - Price Regression (with house type and region name)
# ------------------------------------------------------------------------------

one_hot_encoded <- model.matrix(~ Regionname - 1, data = houses)
one_hot_encoded_df <- as.data.frame(one_hot_encoded)
colnames(one_hot_encoded_df) <- gsub("Regionname", "", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("South-Eastern Metropolitan", "SEMetropolitan", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Eastern Metropolitan", "EMetropolitan", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Eastern Victoria", "EVictoria", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Northern Metropolitan", "NMetropolitan", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Northern Victoria", "NVictoria", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Southern Metropolitan", "SMetropolitan", colnames(one_hot_encoded_df))
colnames(one_hot_encoded_df) <- gsub("Western Metropolitan", "WMetropolitan", colnames(one_hot_encoded_df))

houses <- cbind(houses, one_hot_encoded_df)
houses <- subset(houses, select = -Regionname)

result <- run_linear_regression(
  data = houses, 
  target = "Price", 
  predictors = c("Rooms", "Bedroom2", "Bathroom", "Car", "Landsize", 
                 "Distance", "h", "t", "u", "EMetropolitan", 
                 "EVictoria", "NMetropolitan", "NVictoria", "SEMetropolitan", 
                 "SMetropolitan", "WMetropolitan"), 
  test_size = 0.2, 
  seed = 123
)

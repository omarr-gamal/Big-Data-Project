# ------------------------------------------------------------------------------
# - Data Loading and Inspection
# ------------------------------------------------------------------------------

houses <- read.csv("melb_data.csv")

# Display the structure of the dataset
str(houses)

# Display summary statistics of the dataset
summary(houses)
head(houses)

# Check for missing values in each column
colSums(is.na(houses))

# ------------------------------------------------------------------------------
# - Data Cleaning
# ------------------------------------------------------------------------------

# Dropping rows with missing 'Car' values
houses <- houses[complete.cases(houses$Car), ]

# Dropping unnecessary columns
houses <- subset(houses, select = -c(Address, SellerG, Date, Postcode, CouncilArea, YearBuilt))

# ------------------------------------------------------------------------------
# - Outlier Removal
# ------------------------------------------------------------------------------

numeric_columns <- c("Rooms", "Bedroom2", "Bathroom", "Car", "Landsize", "Distance", "Price")

# Function to remove outliers using the IQR technique.
remove_outliers <- function(df, cols) {
  for (col in cols) {
    q1 <- quantile(df[[col]], 0.25)
    q3 <- quantile(df[[col]], 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 2 * iqr
    upper_bound <- q3 + 2 * iqr
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  return(df)
}

# Remove outliers from numeric columns
houses_clean <- remove_outliers(houses, numeric_columns)

# ------------------------------------------------------------------------------
# - Missing Value Imputation
# ------------------------------------------------------------------------------

# Impute missing Building Area values based on the median for each number of rooms
unique_rooms <- unique(houses_clean$Rooms)
for (room_count in unique_rooms) {
  median_area <- median(houses_clean$BuildingArea[houses_clean$Rooms == room_count], na.rm = TRUE)
  houses_clean$BuildingArea[houses_clean$Rooms == room_count & is.na(houses_clean$BuildingArea)] <- median_area
}



# ------------------------------------------------------------------------------
# - Writing Cleaned Data
# ------------------------------------------------------------------------------

write.csv(houses_clean, file = "melb_data_cleaned.csv", row.names = FALSE)

houses <- read.csv("melb_data.csv")

str(houses)

summary(houses)

colSums(is.na(houses))

houses <- houses[complete.cases(houses$Car), ]

houses <- subset(houses, select = -c(Address, SellerG, Date, Postcode, CouncilArea))

houses <- houses[, !names(houses) %in% "YearBuilt"]

numeric_columns <- c("Rooms", "Bedroom2", "Bathroom", "Car", "Landsize", "Distance", "Price")

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

houses_clean <- remove_outliers(houses, numeric_columns)


unique_rooms <- unique(houses_clean$Rooms)
for (room_count in unique_rooms) {
  # Calculate the median Building Area for houses with the current number of rooms
  median_area <- median(houses_clean$BuildingArea[houses_clean$Rooms == room_count], na.rm = TRUE)

  # Replace missing Building Area values with the median for the corresponding number of rooms
  houses_clean$BuildingArea[houses_clean$Rooms == room_count & is.na(houses_clean$BuildingArea)] <- median_area
}




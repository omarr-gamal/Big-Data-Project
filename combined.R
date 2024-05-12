# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# - EDA
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

install.packages("gplots")
library(gplots)

houses <- read.csv("melb_data.csv")

plot(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])


summary(houses)


hist(houses$Price, main = "Distribution of Price", 
     xlab = "Price ($)", ylab = "Frequency", breaks = 100)

plot(houses$Rooms, houses$Price, main = "Price vs Number of Rooms", xlab = "Number of Rooms", ylab = "Price ($)")

correlation_matrix <- cor(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])
print(correlation_matrix)

# Compute correlation matrix
correlation_matrix <- cor(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])

# Round correlation matrix to two decimal places
correlation_matrix_rounded <- round(correlation_matrix, 2)

# Print correlation matrix
print(correlation_matrix_rounded)

barplot(table(houses$Type), main = "Frequency of Property Types", xlab = "Property Type", ylab = "Frequency")


## Get Overview of the data
head(houses)  # View the first few rows of the dataset
str(houses)   # View the structure of the dataset

summary(houses)
### From the summary function, we find summary statistics of the dataset, for example :

par(mar=c(5, 4, 4, 2) + 0.1)  # Increase if necessary


#  suburb
# Calculate counts
suburb_counts <- table(houses$Suburb)
# Convert table to a data frame for better display
suburb_counts_df <- as.data.frame(suburb_counts)
names(suburb_counts_df) <- c("Suburb", "Count")

# Order by count, descending
suburb_counts_df <- suburb_counts_df[order(-suburb_counts_df$Count),]

# Print the data frame
print(suburb_counts_df)
# Count unique suburbs
num_unique_suburbs <- length(unique(houses$Suburb))

# Print the number of unique suburbs
print(num_unique_suburbs)

#rooms
# Calculate room frequency
room_counts <- table(houses$Rooms)
# Plot the frequency of rooms
barplot(room_counts, 
        main = "Frequency of Rooms in Houses", 
        xlab = "Number of Rooms", 
        ylab = "Frequency",
        col = "skyblue", 
        las = 1,           # Makes axis labels horizontal
        ylim = c(0, max(room_counts) + 500))  # Adjust ylim to give some space above bars for clarity

#type
# Calculate type frequency
type_counts <- table(houses$Type)
# Define descriptive labels
type_labels_full <- c(
  br = "Bedroom(s)",
  h = "House",
  u = "Unit, Duplex",
  t = "Townhouse",
  `dev site` = "Development Site",
  `o res` = "Other Residential"
)

# Define colors for each type
type_colors <- c(
  br = "gray",
  h = "skyblue",
  u = "salmon",
  t = "lightgreen",
  `dev site` = "orange",
  `o res` = "purple"
)
# Calculate type frequency
type_counts <- table(houses$Type)

# Check what types are present in the data
actual_types <- names(table(houses$Type))

# Subset the full labels and colors to match the data
labels_for_pie <- type_labels_full[actual_types]
colors_for_pie <- type_colors[actual_types]

# Calculate percentage labels for the pie chart
percentage_labels <- paste(labels_for_pie, 
                           ": ", 
                           round(100 * type_counts / sum(type_counts), 1), 
                           "%", sep="")

# Create the pie chart
pie(type_counts, 
    labels = percentage_labels, 
    col = colors_for_pie,
    main = "Distribution of House Types",
    init.angle = 90)  # Optional: Adjust the starting angle if desired


# Histogram of Price
hist(houses$Price, 
     breaks = 50,  # Adjust number of breaks to improve visualization
     main = "Histogram of House Prices", 
     xlab = "Price", 
     col = "lightblue", 
     border = "white")

max_price <- max(houses$Price)
end_point <- max_price * 1.1
hist(houses$Price, main = "Distribution of Price", xlab = "Price", xlim = c(0, end_point))

# Boxplot of Price by Type
boxplot(Price ~ Type, data = houses, horizontal = TRUE,  main = "Price by Type", xlab = "Price", ylab = "Type")

# Scatterplot of Price vs. Rooms
plot(houses$Rooms, houses$Price, main = "Price vs. Rooms", xlab = "Rooms", ylab = "Price")

# Pie Chart of Housing Type
type <- houses$Type
type_counts <- table(type)
type_pct <- round((type_counts / sum(type_counts)) * 100)
pie(type_counts, labels = paste(names(type_counts), type_pct, "%"), main = "Pie chart of House Type")

# Density Plot for Price without "e" notation
plot(density(houses$Price), main = "Density Plot of Property Prices", xlab = "Price (in dollars)", ylab = "Density")

# Density Plot for Distance
plot(density(houses$Distance), main = "Density Plot of Distance from CBD", xlab = "Distance (from CBD)", ylab = "Density")

# Median Price by Number of Rooms - Boxplot
boxplot(houses$Price ~ houses$Rooms, main = "Median Price by Number of Rooms", xlab = "Number of Rooms", ylab = "Price")

# method
# Count the frequencies of each sale method
method_counts <- table(houses$Method)

# Define specific methods and calculate 'Others'
specific_methods <- c("S", "SP", "PI", "VB")
other_methods <- setdiff(names(method_counts), specific_methods)
others_count <- sum(method_counts[other_methods])
specific_counts <- method_counts[specific_methods]

# Combine into new table
new_counts <- c(specific_counts, Others = others_count)
# Labels for each category
method_labels <- c(
  S = "Sold",
  SP = "Sold Prior",
  PI = "Passed In",
  VB = "Vendor Bid",
  Others = "Others"
)

# Consistent colors for pie chart (you might adjust these to match previous charts)
method_colors <- c(
  S = "lightblue",    # Sold
  SP = "lightgreen",  # Sold Prior
  PI = "orange",      # Passed In
  VB = "pink",        # Vendor Bid
  Others = "grey"     # Others
)

# Ensure that the labels and colors are correctly ordered
final_labels <- method_labels[names(new_counts)]
final_colors <- method_colors[names(new_counts)]
# Create the pie chart
pie(new_counts,
    labels = final_labels,
    col = final_colors,
    main = "Distribution of Sale Methods",
    init.angle = 90)

# Add a legend for clarity
legend("topright",
       legend = names(final_labels),
       fill = final_colors,
       title = "Key",
       cex = 0.8)  # Adjust text size as necessary

#seller g
# Calculate frequency of each seller
seller_counts <- table(houses$SellerG)

# Number of unique sellers
num_unique_sellers <- length(seller_counts)
print(paste("Number of unique real estate agents: ", num_unique_sellers))
# Maximum sales by a single agent
max_sales <- max(seller_counts)
max_seller <- names(which(seller_counts == max_sales))
print(paste("The maximum sales by a single agent were made by:", max_seller, "with", max_sales, "sales."))

#date
# Assuming the date is in a format like 'DD/MM/YYYY'
houses$Date <- as.Date(houses$Date, format = "%d/%m/%Y")
# Get the range of dates
date_range <- range(houses$Date)
print(paste("Sales are recorded from", date_range[1], "to", date_range[2]))
# Count sales per month
sales_per_month <- table(format(houses$Date, "%Y-%m"))

# Plot the number of sales per month
barplot(sales_per_month, main="Monthly Sales", xlab="Month", ylab="Number of Sales",
        las=2, col="lightblue")
#distance
# Histogram of distance from CBD
hist(houses$Distance, breaks = 20, main = "Histogram of Distance from CBD", 
     xlab = "Distance (km)", col = "cornflowerblue")

#postcode
# Count of unique postcodes
unique_postcodes <- unique(houses$Postcode)
num_unique_postcodes <- length(unique_postcodes)
cat("Number of unique postcodes:", num_unique_postcodes, "\n")

# Display a few unique postcodes
head(unique_postcodes)
# Frequency table of postcodes
postcode_frequency <- table(houses$Postcode)
# Display the frequency of the first few postcodes
head(postcode_frequency)
# Bar plot of the top 20 most common postcodes
top_postcodes <- sort(postcode_frequency, decreasing = TRUE)[1:20]
barplot(top_postcodes, main = "Top 20 Postcodes by Number of Properties", 
        xlab = "Postcode", ylab = "Frequency", las = 2, col = "steelblue")

#bedroom2
# Create a table of frequencies for Bedroom2
bedroom2_counts <- table(houses$Bedroom2)

# Create a bar plot for Bedroom2
barplot(bedroom2_counts, main = "Bar Plot of Bedroom2", xlab = "Number of Bedrooms", ylab = "Frequency", col = "lightblue", las = 2)

# Bar Plot of Rooms
barplot(table(houses$Rooms), main = "Bar Plot of Rooms", xlab = "Rooms", ylab = "Frequency")
# Bar Plot of Bathrooms
barplot(table(houses$Bathroom), main = "Bar Plot of Bathrooms", xlab = "Bathrooms", ylab = "Frequency")
# Bar Plot of Car Spots
barplot(table(houses$Car), main = "Bar Plot of Car Spots", xlab = "Car Spots", ylab = "Frequency")

#####
# Barplot of Property Count by Region Name
barplot(table(houses$Regionname), main = "Property Count by Region Name", xlab = "Region Name", ylab = "Count", col = "skyblue")

# Boxplot of Price by Type of Property
boxplot(Price ~ Type, data = houses, horizontal = TRUE, main = "Price by Type of Property", xlab = "Price", ylab = "Type", col = "lightblue")

# Histogram of Building Area
#hist(houses$BuildingArea, main = "Distribution of Building Area", xlab = "Building Area", col = "skyblue")

# Pie Chart of Method of Sale
pie(table(houses$Method), main = "Pie Chart of Method of Sale", col = rainbow(length(unique(houses$Method))), labels = paste(names(table(houses$Method)), "(", round(prop.table(table(houses$Method)) * 100), "%)", sep = ""))

# Scatterplot of Price vs. Distance from CBD
plot(houses$Distance, houses$Price, main = "Price vs. Distance from CBD", xlab = "Distance from CBD", ylab = "Price", col = "blue")

# Barplot of Property Count by Number of Rooms
barplot(table(houses$Rooms), main = "Property Count by Number of Rooms", xlab = "Number of Rooms", ylab = "Count", col = "skyblue")

# Boxplot of Price by Number of Bedrooms
boxplot(Price ~ Bedroom2, data = houses, main = "Price by Number of Bedrooms", xlab = "Number of Bedrooms", ylab = "Price", col = "lightblue")

# Histogram of Land Size
# Histogram with log transformation
hist(log(houses$Landsize + 1), main = "Histogram of Log-transformed Landsize", xlab = "Log-transformed Landsize (log(square meters))", col = "blue", breaks = 50)


#building area
# Histogram with log transformation
hist(log(houses$BuildingArea + 1), main = "Histogram of Log-transformed BuildingArea", xlab = "Log-transformed Building Area (log(square meters))", col = "blue", breaks = 50)

#yearbuilt
# Calculate the frequency of each year
# Binning years by decade
houses$DecadeBuilt <- cut(houses$YearBuilt, breaks = seq(1850, 2020, by = 10), labels = seq(1850, 2010, by = 10), include.lowest = TRUE)

# Calculate the frequency of each decade
decade_freq <- table(houses$DecadeBuilt)

# Bar plot for DecadeBuilt
barplot(decade_freq, main = "Bar Plot of Decade Built", xlab = "Decade Built", ylab = "Frequency", col = "salmon", las = 2, cex.names = 0.8)


#council
# Calculate the frequency of each council area
council_freq <- table(houses$CouncilArea)

# Remove NA values if necessary
council_freq <- council_freq[!is.na(names(council_freq))]
# Bar plot for CouncilArea
barplot(council_freq, 
        main = "Bar Plot of Council Areas", 
        ylab = "Frequency", 
        col = "lightgreen", 
        las = 2, 
        cex.names = 0.8)

#region
# Count the occurrences of each region
region_counts <- table(houses$Regionname)
# Save the current graphical parameters
old_par <- par()

# Set the margin size (bottom margin specifically)
# The numbers represent the size of the margins in lines:
# c(bottom, left, top, right)
par(mar = c(7, 4, 4, 2) +2)  # Increase bottom margin

# Bar plot with adjusted margins
barplot(table(houses$Regionname), 
        main = "Bar Plot of Regions", 
        ylab = "Count of Properties", 
        col = rainbow(length(table(houses$Regionname))), 
        las = 2,
        cex.names = 0.7
)  # Rotate x-axis labels


# Histogram of property counts
hist(houses$Propertycount,
     breaks = 50,  # Adjust the number of breaks for better granularity
     main = "Histogram of Property Counts",
     xlab = "Property Counts",
     col = "skyblue",
     border = "white")


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
# ------------------------------------------------------------------------------
# - Data Cleaning
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# - Price Regression
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# - Visualizing The Model
# ------------------------------------------------------------------------------

# Make predictions on the entire dataset
houses$Predicted_Price <- predict(result$model, newdata = houses)


# Scatterplot of Predicted vs. Actual Prices with perfect fit line
plot(
  houses$Price, houses$Predicted_Price, 
  xlab = "Actual Price", ylab = "Predicted Price",
  pch = 16, lwd = 2, cex = 0.5
)
abline(0, 1, col = "red", lwd = 2)

# --------------------------------------------------------------------

# Residual Plot
residuals <- houses$Price - houses$Predicted_Price
plot(
  houses$Predicted_Price, residuals, 
  xlab = "Predicted Price", ylab = "Residuals",
  pch = 16, lwd = 2, cex = 0.5
)
# Horizontal line at y = 0 resembling a perfect model.
abline(h = 0, col = "red", lwd = 2)

# --------------------------------------------------------------------

# distribution of Price and Predicted Price
hist(
  houses$Price, 
  col = rgb(1, 1, 1, alpha = 0.5), breaks = 100,
  xlab = "Price", main = "Histogram of Price and Predicted Price",
  xlim = c(-1000000, 3000000), ylim = c(0, 400)
)
hist(houses$Predicted_Price, col = rgb(0, 0, 0, alpha = 0.5), breaks = 100, add = TRUE)
hist(residuals, col = rgb(1, 0, 0, alpha = 0.2), breaks = 200, add = TRUE)

legend("topright", legend = c("Price", "Predicted Price", "Error"), fill = c("white", "black", "pink"))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# - Number of Rooms Classification
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

install.packages("party")
install.packages("e1071")
library(party)
library(e1071)

houses <- read.csv("melb_data_cleaned.csv")
str(houses)

#Categorical to factor
houses <- houses[, c("Rooms", "Type", "Price", "Distance","Bathroom", "Car", "Landsize", "BuildingArea", "Method","Propertycount")]
houses$Type <- as.factor(houses$Type)
houses$Rooms <- as.factor(houses$Rooms)
houses$Method <- as.factor(houses$Method)
str(houses)

#Data splitting
ind <- sample(2,nrow(houses),prob=c(0.8,0.2),replace=TRUE)
train_data <- houses[ind==1, ]
test_data <- houses[ind==2, ]

#Decision Tree
tree_model <- ctree(Rooms ~ ., data = train_data)
plot(tree_model,type="simple")
tree_pred <- predict(tree_model, newdata = test_data)
table(Predicted = tree_pred,Actual = test_data$Rooms)

#SVM
svm_model <- svm(Rooms ~ ., data = train_data, kernel="radial")
svm_pred <- predict(svm_model, newdata = test_data)
table(Predicted = svm_pred,Actual = test_data$Rooms)

#Naive bayes
nb_model <- naiveBayes(Rooms ~ ., data = train_data)
nb_pred <- predict(nb_model, newdata = test_data)
table(Predicted = nb_pred,Actual = test_data$Rooms)

tree_accuracy <- sum(tree_pred == test_data$Rooms) / length(tree_pred)
svm_accuracy <- sum(svm_pred == test_data$Rooms) / length(svm_pred)
nb_accuracy <- sum(nb_pred == test_data$Rooms) / length(nb_pred)
tree_accuracy
svm_accuracy
nb_accuracy

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# - Hypothesis Testing
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(lattice)
houses <- read.csv("melb_data_cleaned.csv")

# Using the number of unique values to decide what hypothesis to make
unique(houses$Method)
unique(houses$Rooms)
unique(houses$Type)
unique(houses$Bathroom)

#Hypothesis 1 : The number of rooms 
# Null hypothesis : The mean price is the same across all categories of room numbers. The number of rooms has no effect on the price.
# Alternate hypothesis : The mean price varies between the room categories of room numbers. The number of rooms has an effect on the price.
houses$Rooms <- as.factor(houses$Rooms)
result <- aov(Price ~ Rooms, data = houses)
summary(result)
#The p value is <2e-16 which is way lower than 0.05, that means that we should reject the null hypothesis in all cases
TukeyHSD(result)

#In conclusion the alternate hypothesis is correct in all cases
densityplot(~ Price, groups = Rooms, data = houses, auto.key = TRUE,
            main = "Density Plot of Prices by number of rooms",
            xlab = "Price", plot.points = FALSE)

# Based on Tukey
#on average
#2-1: Houses with 2 rooms are $335,512.7 more expensive than houses with 1 room.
#3-1: Houses with 3 rooms are $627,921.6 more expensive than those with 1 room.
#4-1: Houses with 4 rooms are $897,083.1 more expensive than those with 1 room.
#5-1: Houses with 5 rooms are $1,093,865.5 more expensive than those with 1 room.
#3-2: Houses with 3 rooms are $292,408.9 more expensive than those with 2 rooms.
#4-2: Houses with 4 rooms are $561,570.3 more expensive than those with 2 rooms.
#5-2: Houses with 5 rooms are $758,352.8 more expensive than those with 2 rooms.
#4-3: Houses with 4 rooms are $269,161.4 more expensive than those with 3 rooms.
#5-3: Houses with 5 rooms are $465,943.9 more expensive than those with 3 rooms.
#5-4: Houses with 5 rooms are $196,782.5 more expensive than those with 4 rooms.


#t - townhouse
#h - house,cottage,villa, semi,terrace
#u - unit, duplex

# Hypothesis 2: The effect of house type on price
# Null hypothesis: The mean price is the same across all categories of house types. The type of house has no effect on the price.
# Alternate hypothesis: The mean price varies between the categories of house types. The type of house has an effect on the price.
result_type <- aov(Price ~ Type, data = houses)
summary(result_type)
TukeyHSD(result_type)
densityplot(~ Price, groups = Type, data = houses, auto.key = TRUE,
            main = "Density Plot of Prices by House Type",
            xlab = "Price", plot.points = FALSE)
#We will reject the null hypothesis in all cases making the alternate hypothesis always correct
# Based on Tukey
#on average
#t-h: Houses with type t are $242,035.2 less expensive than houses with type h.
#u-h: Houses with type u are $564,274.9 less expensive than houses with type h.
#u-t: Houses with type u are $322,239.6 less expensive than houses with type t.

#Hypothesis 3 : The Effect of Distance from CBD on Number of Rooms
# Null hypothesis: The mean price is the same across all categories of Sale Method. The Sale Method of house has no effect on the price.
# Alternate hypothesis: The mean price varies between at least two of the categories of sale methods. The sale method of house has an effect on the price.
result_method <- aov(Price ~ Method, data = houses)
summary(result_method)
TukeyHSD(result_method)
densityplot(~ Price, groups = Method, data = houses, auto.key = TRUE,
            main = "Density Plot of Prices by Method",
            xlab = "Price", plot.points = FALSE)
# Tukey's Honest Significant Difference Test Conclusions:

# The TukeyHSD test provides pairwise comparisons among different property sales conditions such as:
# S (sold), PI (property passed in), SP (sold prior), SA (sold after auction), and VB (vendor bid).

# Key Conclusions:
# 1. Significant Differences:
#    - SP-PI (p adj = 0.0000000): Reject the null hypothesis. Significant difference in mean values between properties sold prior and properties passed in.
#    - SP-S (p adj = 0.0000000): Reject the null hypothesis. Significant difference in mean values between properties sold and sold prior.
#    - VB-SP (p adj = 0.0000000): Reject the null hypothesis. Significant difference in mean values between vendor bids and properties sold prior.
#    These results suggest that the timing and nature of the sale (prior, at, or post-auction) have significant impacts on the outcome metrics (likely price).

# 2. Non-significant Differences:
#    - S-PI (p adj = 0.9813904): Accept the null hypothesis. No significant difference between sold properties and properties passed in.
#    - SA-PI (p adj = 0.9527848): Accept the null hypothesis. No significant difference between properties sold after auction and properties passed in.
#    - VB-PI (p adj = 0.9999995): Accept the null hypothesis. No significant difference between vendor bids and properties passed in.
#    - SA-S (p adj = 0.9767688): Accept the null hypothesis. No significant difference between properties sold and sold after auction.
#    - VB-S (p adj = 0.9922909): Accept the null hypothesis. No significant difference between vendor bids and sold properties.
#    - VB-SA (p adj = 0.9574013): Accept the null hypothesis. No significant difference between vendor bids and properties sold after auction.
#    These outcomes suggest that, in some scenarios, the conditions under which sales occur do not significantly alter the outcome metrics.




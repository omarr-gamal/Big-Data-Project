install.packages("gplots")
library(gplots)

houses <- read.csv("melb_data_cleaned.csv")

plot(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])


summary(houses)


hist(houses_clean$Price, main = "Distribution of Price", 
     xlab = "Price ($)", ylab = "Frequency", breaks = 100)

plot(houses_clean$Rooms, houses_clean$Price, main = "Price vs Number of Rooms", xlab = "Number of Rooms", ylab = "Price ($)")

correlation_matrix <- cor(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])
print(correlation_matrix)
# Draw Correlation Matrix as Heatmap
# heatmap(correlation_matrix, 
#         symm = TRUE,             # Treat the correlation matrix as symmetric
#         main = "Correlation Matrix", 
#         xlab = "Variables", 
#         ylab = "Variables", 
#         col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
#         margins = c(10, 10))    # Adjust margins for better visualization

# Compute correlation matrix
correlation_matrix <- cor(houses[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])

# Round correlation matrix to two decimal places
correlation_matrix_rounded <- round(correlation_matrix, 2)

# Print correlation matrix
print(correlation_matrix_rounded)

# Draw Correlation Matrix as Heatmap with text labels
heatmap.2(correlation_matrix_rounded, 
          trace = "none",         # Do not display trace lines
          main = "Correlation Matrix", 
          xlab = "Variables", 
          ylab = "Variables", 
          col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
          dendrogram = "none",    # Do not show dendrogram
          cellnote = correlation_matrix_rounded,  # Add correlation values as text labels
          notecol = "black",      # Set text color
          notecex = 0.7,          # Set text size
          margins = c(10, 10))    # Adjust margins for better visualization



barplot(table(houses_clean$Type), main = "Frequency of Property Types", xlab = "Property Type", ylab = "Frequency")

colSums(is.na(houses_clean))

table(houses$BuildingArea)


################################### Martina




## Load the Data
houses <- read.csv("melb_data_cleaned.csv")

## Get Overview of the data
head(houses)  # View the first few rows of the dataset
str(houses)   # View the structure of the dataset

### From running this, we observe that the dataset contains 12,389 observations(rows) and 15 variables(columns). 
### The variables include information such as suburb name, number of rooms, type of property, price, method of sale, distance from CBD, bedroom count, bathroom count, car park count, land size, building area, latitude, longitude, region name, and property count.
### Numeric variables include: Rooms, Price, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, Lattitude, Longtitude, and Propertycount.
### Character variables include: Suburb, Type, Method, and Regionname.
### Categorical Variables: suburb name, type of property, and method of sale
### Continuous Numerical Variables: Price, distance, land size, building area, latitude, longitude, and property count
### Discrete Numerical Variables: Bedroom count, bathroom count, and car park count

summary(houses)
### From the summary function, we find summary statistics of the dataset, for example :

### The number of rooms ranges from 1 to 5, with a median of 3.
### Bedroom2, Bathroom, and Car variables also have varying counts, with median values of 3, 1, and 1, respectively.

### The prices of the properties range from $85,000 to $2,695,000.
### The median price is $900,000, with a mean price of approximately $1,018,070.

# Histogram of Price
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

# Bar Plot of Rooms
barplot(table(houses$Rooms), main = "Bar Plot of Rooms", xlab = "Rooms", ylab = "Frequency")
# Bar Plot of Bathrooms
barplot(table(houses$Bathroom), main = "Bar Plot of Bathrooms", xlab = "Bathrooms", ylab = "Frequency")
# Bar Plot of Car Spots
barplot(table(houses$Rooms), main = "Bar Plot of Car Spots", xlab = "Car Spots", ylab = "Frequency")

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
hist(houses$Landsize, main = "Distribution of Land Size", xlab = "Land Size", col = "skyblue")

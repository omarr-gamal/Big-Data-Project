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
# Draw Correlation Matrix as Heatmap
# heatmap(correlation_matrix, 
#         symm = TRUE,             # Treat the correlation matrix as symmetric
#         main = "Correlation Matrix", 
#         xlab = "Variables", 
#         ylab = "Variables", 
#         col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
#         margins = c(10, 10))    # Adjust margins for better visualization



# Now you can use heatmap.2
heatmap.2(correlation_matrix_rounded, trace = "none", main = "Correlation Matrix")

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



barplot(table(houses$Type), main = "Frequency of Property Types", xlab = "Property Type", ylab = "Frequency")

colSums(is.na(houses))

table(houses$BuildingArea)


################################### Martina




## Load the Data
houses <- read.csv("melb_data.csv")

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

# Check what types are present in the data
actual_types <- names(table(houses$Type))

# Subset the full labels and colors to match the data
labels_for_pie <- type_labels_full[actual_types]
colors_for_pie <- type_colors[actual_types]


# Histogram of Price
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

#longitude
# Convert Longitude to numeric if it is not
if (!is.numeric(houses$Longitude)) {
  houses$Longitude <- as.numeric(as.character(houses$Longitude))
}
# Plot histogram
hist(houses$Longitude, 
     main = "Distribution of Longitude", 
     xlab = "Longitude", 
     col = "skyblue", 
     border = "black", 
     breaks = 30,
     xlim = range(houses$Longitude, na.rm = TRUE))  # Ensure NA values are removed for plot limits

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

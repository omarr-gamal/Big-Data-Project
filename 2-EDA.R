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
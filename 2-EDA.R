
# Plot 1


hist(houses_clean$BuildingArea, main = "Distribution of Price", 
     xlab = "Price ($)", ylab = "Frequency", xlim = c(1, 500), breaks = 5000)

plot(houses_clean$Rooms, houses_clean$Price, main = "Price vs Number of Rooms", xlab = "Number of Rooms", ylab = "Price ($)")

correlation_matrix <- cor(data[, c("Price", "Rooms", "Bedroom2", "Bathroom", "Landsize")])
print(correlation_matrix)
# Draw Correlation Matrix as Heatmap
heatmap(correlation_matrix, 
        symm = TRUE,             # Treat the correlation matrix as symmetric
        main = "Correlation Matrix", 
        xlab = "Variables", 
        ylab = "Variables", 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
        margins = c(10, 10))    # Adjust margins for better visualization

barplot(table(houses_clean$Type), main = "Frequency of Property Types", xlab = "Property Type", ylab = "Frequency")

colSums(is.na(houses_clean))

table(houses$BuildingArea)
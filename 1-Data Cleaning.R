data <- read.csv("melb_data.csv")

str(data)

summary(data)

hist(data$Price, main = "Distribution of Price", xlab = "Price ($)", ylab = "Frequency")

plot(data$Rooms, data$Price, main = "Price vs Number of Rooms", xlab = "Number of Rooms", ylab = "Price ($)")

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

barplot(table(data$Type), main = "Frequency of Property Types", xlab = "Property Type", ylab = "Frequency")

colSums(is.na(data))


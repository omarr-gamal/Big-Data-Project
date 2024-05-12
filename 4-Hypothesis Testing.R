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

#Hypothesis 3 : The Effect of the sell method and the price.
# Null hypothesis: The mean price is the same across all categories of Sale Method. The Sale Method of house has no effect on the price.
# Alternate hypothesis: The mean price varies between the categories of sale methods. The sale method of house has an effect on the price.
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


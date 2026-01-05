# EDA

# 1. installing and importing libraries

install.packages('tidyverse')
install.packages("ggplot2")
install.packages("psych")
install.packages('patchwork')
install.packages('reshape2')
install.packages("lmtest")
library(lmtest)
library(tidyverse)
library(ggplot2)
library(psych)
library(patchwork)
library(reshape2)


# 2.Importing and preparing the Data set

setwd('C:\\Users\\Dan\\Documents\\Class\\y III sem I\\assignment\\reg')

df <- RegressionProject_Daniel.Mekuriaw_UGR.4804.16

# rounding the bedroom numbers in each house
df$bedrooms <- as.integer(round(df$bedrooms))

# converting the year_built into age of the house
df$house_age <- 2025 - df$year_built


# 3. see the variables

glimpse(df) 


# 4. summarize the data numerically
# 4.1. the over all summary

summary(df)
summary_table <- describe(df)


# 4.2. for categorical variables

table(df$condition)
table(df$location_quality)
table(df$renovated)

# 4.3. missing valued variable(renovation of the house)

mean(is.na(df$year_renovated))*100 # this will tell what percent of the data is missing

# 5. presentation of each variable
# 5.1. response variable(price)

price_histo <- ggplot(df) + geom_histogram(aes(price), bins = 900) + labs(title = 'Histogram')
price_dens <- ggplot(df) + geom_density(aes(price), linewidth = 1) + labs( title = 'Densityplot')
price_box <- ggplot(df) + geom_boxplot(aes(price)) + labs(title = 'Boxplot')
price_histo|price_dens|price_box

# 5.2. continues variables

# make a function to plot for continuous variables
for_conti <- function(value, title) {
  
  plot1 <- ggplot(df) + geom_histogram(aes(value, y = after_stat(density))) + geom_density(aes(value)) + labs(title = paste('Histogram: ', title))
  
  plot2 <- ggplot(df) + geom_boxplot(aes(value)) + labs(title = paste('Boxplot: ', title))
  
  plot1|plot2

}
# do the function for each continuous variable
for_conti(df$size_sqft, "Interior Size (sqft)")
for_conti(df$lot_sqft, "Lot Size (sqft)")
for_conti(df$dist_center_km, "Distance from Center (km)")
for_conti(df$crime_rate, "Crime Rate")
for_conti(df$house_age, "The age of the house")

# 6. presentation of the relation of each variable with price

# 6.1 price Vs continuous

P1 <- ggplot(df) + geom_point(aes(price, size_sqft)) + labs(title = 'Scatter plot: price Vs size_sqft')
P2 <- ggplot(df) + geom_point(aes(price, lot_sqft)) + labs(title = 'Scatter plot: price Vs lot_sqft')
P3 <- ggplot(df) + geom_point(aes(price, house_age)) + labs(title = 'Scatter plot: price Vs house_age')
P4 <- ggplot(df) + geom_point(aes(price, dist_center_km)) + labs(title = 'Scatter plot: price Vs dist_center_km')
P5 <- ggplot(df) + geom_point(aes(price, crime_rate)) + labs(title = 'Scatter plot: price Vs crime_rate')

(P1|P2|P3)/(P4|P5)

# 6.2 price Vs categorical and discrete variables

pp1 <- ggplot(df) + geom_boxplot(aes(price, location_quality)) + labs(title = "1. Price vs Location_Quality") + theme_minimal()
pp2 <- ggplot(df) + geom_boxplot(aes(price, condition)) + labs(title = "2. Price vs Condition") + theme_minimal()
pp3 <- ggplot(df) + geom_boxplot(aes(price, waterfront)) + labs(title = "3. Price vs Waterfront") + theme_minimal()
pp4 <- ggplot(df) + geom_boxplot(aes(price, as.factor(bedrooms))) + labs(title = "4. Price vs Bedrooms", y = 'Bedrooms') + theme_minimal()
pp5 <- ggplot(df) + geom_boxplot(aes(price, as.factor(bathrooms))) + labs(title = "5. Price vs bathrooms", y = 'bathrooms') + theme_minimal()
pp6 <- ggplot(df) + geom_boxplot(aes(price, renovated)) + labs(title = "6. Price vs renovated") + theme_minimal()
pp7 <- ggplot(df) + geom_boxplot(aes(price, has_pool)) + labs(title = "7. Price vs has_pool") + theme_minimal()
pp8 <- ggplot(df) + geom_boxplot(aes(price, as.factor(garage_cars))) + labs(title = "8. Price vs garage_cars", y = 'garage_cars') + theme_minimal()
pp9 <- ggplot(df) + geom_boxplot(aes(price, as.factor(school_rating))) + labs(title = "9. Price vs school_rating", y = 'school_rating') + theme_minimal()

(pp1|pp2|pp3)/(pp4|pp5|pp6)/(pp7|pp8|pp9)

# 7. correlation

# 7.1 correlation matrix 

num_Vars <- df[, c('price', 'size_sqft', 'lot_sqft', 'bedrooms', 'bathrooms', 'dist_center_km', 'crime_rate', 'school_rating', 'garage_cars', 'house_age')]

corr_vars <- cor(num_Vars)

# 7.2 correlation heat map
melted_corr <- melt(corr_vars)

ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", fill = "Correlation")

# 8. heteroscedasticity check
# 8.1. numerical check

naive_model <- lm(price ~ size_sqft + dist_center_km + crime_rate + school_rating, data = df)

bptest(naive_model) # Breusch-Pagan test 

# 8.2. graphical check

ggplot(naive_model, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, color = "red") + # The "Zero Error" reference line
  labs(title = "Residual Plot", x = "Fitted", y = "Residuals")







# View the first few rows of the dataset
head(sales_data)

# Check column names and data types
str(sales_data)



#Data Exploration
# Load the ggplot2 library
#Temp Dis
library(ggplot2)
library(dplyr)

# Create a histogram for temperature
ggplot(sales_data, aes(x = temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Temperature",
    x = "Temperature (°C)",
    y = "Frequency"
  ) +
  theme_minimal()

# Discussion for temperature distribution
# The histogram illustrates the distribution of temperatures in the dataset. 
# The majority of recorded temperatures are concentrated within a specific range.
# Further analysis is needed to explore the relationship between temperature and ice cream sales.

#Humidity Distribution

# Create a histogram for humidity
ggplot(sales_data, aes(x = humidity)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  labs(
    title = "Distribution of Humidity",
    x = "Humidity (%)",
    y = "Frequency"
  ) +
  theme_minimal()

#Monthly Sales

# Calculate the average ice cream sales per month
monthly_sales <- sales_data %>%
  group_by(month_name) %>%
  summarize(avg_icecream_sales = mean(icecream_sales))

# Create a bar plot for monthly ice cream sales
ggplot(monthly_sales, aes(x = month_name, y = avg_icecream_sales)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(
    title = "Average Ice Cream Sales per Month",
    x = "Month",
    y = "Average Ice Cream Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Number 2

#2.1
# Calculate the proportion of days with fewer than 200 ice cream sales
prop_less_than_200_icecream <- mean(sales_data$icecream_sales < 200)

# Calculate the 95% confidence interval for the proportion
conf_interval <- prop.test(sum(sales_data$icecream_sales < 200), nrow(sales_data), conf.level = 0.95)$conf.int

# Display the results
prop_less_than_200_icecream
conf_interval


#2.2

# Calculate the proportion of days with total sales (ice cream + hot drinks) below 200
total_sales <- sales_data$icecream_sales + sales_data$hotdrink_sales
prop_less_than_200_total <- mean(total_sales < 200)

# Calculate the 95% confidence interval for the proportion
conf_interval_total <- prop.test(sum(total_sales < 200), nrow(sales_data), conf.level = 0.95)$conf.int

# Display the results
prop_less_than_200_total
conf_interval_total


#2.3

# Subset the data for January and August
january_data <- subset(sales_data, month_name == "Jan")
august_data <- subset(sales_data, month_name == "Aug")

# Calculate the odds ratio for January
odds_january <- sum(january_data$icecream_sales > 0) / sum(january_data$hotdrink_sales > 0)

# Calculate the odds ratio for August
odds_august <- sum(august_data$icecream_sales > 0) / sum(august_data$hotdrink_sales > 0)

# Calculate 95% confidence intervals for both odds ratios
conf_interval_january <- exp(c(log(odds_january) - 1.96 * sqrt(1 / sum(january_data$icecream_sales > 0) + 1 / sum(january_data$hotdrink_sales > 0)),
                               log(odds_january) + 1.96 * sqrt(1 / sum(january_data$icecream_sales > 0) + 1 / sum(january_data$hotdrink_sales > 0))))

conf_interval_august <- exp(c(log(odds_august) - 1.96 * sqrt(1 / sum(august_data$icecream_sales > 0) + 1 / sum(august_data$hotdrink_sales > 0)),
                              log(odds_august) + 1.96 * sqrt(1 / sum(august_data$icecream_sales > 0) + 1 / sum(august_data$hotdrink_sales > 0))))
                            
 # Display the results
  odds_january
  conf_interval_january
  odds_august
  conf_interval_august
                            

#2.4
  
  # Filter the data for January and August
  january_data <- sales_data %>% filter(month_name == "Jan")
  august_data <- sales_data %>% filter(month_name == "Aug")
  
  # Calculate the odds ratio for January
  odds_january <- sum(january_data$icecream_sales > 0) / sum(january_data$hotdrink_sales > 0)
  
  # Calculate the odds ratio for August
  odds_august <- sum(august_data$icecream_sales > 0) / sum(august_data$hotdrink_sales > 0)
  
  # Calculate the standard errors for the odds ratios in January and August
  se_january <- sqrt(1 / sum(january_data$icecream_sales > 0) + 1 / sum(january_data$hotdrink_sales > 0))
  se_august <- sqrt(1 / sum(august_data$icecream_sales > 0) + 1 / sum(august_data$hotdrink_sales > 0))
  
  # Calculate the test statistic
  test_statistic <- (log(odds_january) - log(odds_august)) / sqrt(se_january^2 + se_august^2)
  
  # Calculate the degrees of freedom
  df <- min(sum(january_data$icecream_sales > 0), sum(january_data$hotdrink_sales > 0),
            sum(august_data$icecream_sales > 0), sum(august_data$hotdrink_sales > 0)) - 1
  
  # Calculate the p-value
  p_value <- 2 * (1 - pt(abs(test_statistic), df))
  
  # Display the results
  cat("Test Statistic:", test_statistic, "\n")
  cat("Degrees of Freedom:", df, "\n")
  cat("P-Value:", p_value, "\n")
  
  
  #3.1
  
  # Filter the data for weekdays (Mon-Fri) and weekends
  weekday_data <- sales_data %>% filter(weekend == 0)
  weekend_data <- sales_data %>% filter(weekend == 1)
  
  # Calculate the mean number of ice cream sales for weekdays and weekends
  mean_sales_weekday <- mean(weekday_data$icecream_sales)
  mean_sales_weekend <- mean(weekend_data$icecream_sales)
  
  # Perform a t-test to compare the means
  t_test_result <- t.test(weekday_data$icecream_sales, weekend_data$icecream_sales)
  
  # Display the results
  cat("Mean Ice Cream Sales on Weekdays:", mean_sales_weekday, "\n")
  cat("Mean Ice Cream Sales on Weekends:", mean_sales_weekend, "\n")
  cat("T-Test P-Value:", t_test_result$p.value, "\n")
  

  #3.2
  # Calculate the observed effect size (use the effect size from question 3.2)
  observed_effect_size_3.2 <- 1.048887
  
  # Set the significance level (alpha)
  alpha <- 0.05
  
  # Perform a power analysis based on the observed effect size
  power_analysis <- pwr.t.test(d = observed_effect_size_3.2, n = n_weekdays, sig.level = alpha, type = "two.sample", alternative = "two.sided")
  
  # Extract the power value from the analysis
  power <- power_analysis$power
  
  # Display the power of the test
  cat("Power of the Test (observed effect size):", power, "\n")
  
  
  
  
  
  
  #3.3
  
  # Required libraries
  library(pwr)
  
  # Set the parameters
  n <- 103
  alpha <- 0.05
  power <- 0.9
  
  # Calculate the effect size
  effect_size <- pwr.t.test(n = n, sig.level = alpha, power = power, type = "two.sample")$d
  
  effect_size
  
  
  
  #3.4
  
  # Given effect size and desired power
  effect_size <- 0.4538
  desired_power <- 0.90
  
  # Calculate the required sample size
  required_sample_size <- pwr.t.test(d = effect_size, power = desired_power, type = "two.sample", alternative = "two.sided")
  
  # Display the required sample size
  required_sample_size$n
  
  
  
  4.0
  
  # Fit a linear regression model
  ice_cream_model <- lm(icecream_sales ~ temperature + humidity + windspeed + weekend + bank_holiday + school_holidays, data = sales_data)
  
  # Summary of the regression model
  summary(ice_cream_model)
  
  
# Create a linear regression model
lm_model <- lm(icecream_sales ~ temperature + humidity + windspeed + weekend + bank_holiday + school_holidays, data = sales_data)

# Define the conditions for the scenario
conditions <- data.frame(
  month_name = "May",
  weekend = 0,  # Weekday
  bank_holiday = 0,
  school_holidays = 0,
  temperature = 18,
  humidity = 6,
  windspeed = 10
)

# Predict ice cream sales for the given conditions
predicted_sales <- predict(lm_model, newdata = conditions, interval = "prediction", level = 0.95)

# Display the results
cat("Expected Ice Cream Sales on a Weekday in May: ", predicted_sales[1], "\n")
cat("95% Prediction Interval: (", predicted_sales[2], ", ", predicted_sales[3], ")\n")



4.2 # Define the conditions for the scenario
conditions_4.2 <- data.frame(
  month_name = "April",
  weekend = 1,  # Weekend
  bank_holiday = 0,
  school_holidays = 1,
  temperature = 28,
  humidity = 35,
  windspeed = 5
)

# Predict ice cream sales for the given conditions using the linear regression model (lm_model)
predicted_sales_4.2 <- predict(lm_model, newdata = conditions_4.2, interval = "prediction", level = 0.95)

# Display the results
cat("Expected Ice Cream Sales on a School Holiday Weekend in April: ", predicted_sales_4.2[1], "\n")
cat("95% Prediction Interval: (", predicted_sales_4.2[2], ", ", predicted_sales_4.2[3], ")\n")


#4.3

# Define the conditions for the scenario
conditions_4.3 <- data.frame(
  month_name = "September",
  weekend = 0,  # Weekday
  bank_holiday = 0,
  school_holidays = 0,
  temperature = 12,
  humidity = 90,
  windspeed = 35
)

# Predict ice cream sales for the given conditions
predicted_sales_4.3 <- predict(lm_model, newdata = conditions_4.3, interval = "prediction", level = 0.95)

# Display the results
cat("Expected Ice Cream Sales on a Weekday in September: ", predicted_sales_4.3[1], "\n")
cat("95% Prediction Interval: (", predicted_sales_4.3[2], ", ", predicted_sales_4.3[3], ")\n")



#4.4

# Define the conditions for Scenario 4.3
conditions_4.4 <- data.frame(
  month_name = "Jan",
  weekend = 1,  # Weekend (not a holiday)
  bank_holiday = 0,
  school_holidays = 0,
  temperature = -2,
  humidity = 75,
  windspeed = 15
)

# Predict ice cream sales for the given conditions
predicted_sales_4.4 <- predict(lm_model, newdata = conditions_4.4, interval = "prediction", level = 0.95)

# Display the results
cat("Expected Ice Cream Sales on a January Weekend (Not a Holiday): ", predicted_sales_4.4[1], "\n")
cat("95% Prediction Interval: (", predicted_sales_4.4[2], ", ", predicted_sales_4.4[3], ")\n")







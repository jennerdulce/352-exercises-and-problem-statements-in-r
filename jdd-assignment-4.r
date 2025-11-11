library(ggplot2)
library(scales)

custdata <- read.table("./custdata.tsv", 
                       header = TRUE, 
                       sep = "\t", 
                       stringsAsFactors = FALSE)

str(custdata)
summary(custdata)

# Question 1
ggplot(custdata, aes(x = income)) + 
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Density Plot of Customer Income",
       x = "Income ($)",
       y = "Density") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.000001)) +
  theme_minimal()

ggsave("./q1-income_density_plot.png", 
       width = 8, height = 6, dpi = 300)

# Question 2

custdata_clean <- subset(custdata, 
                         !is.na(housing.type) & 
                         !is.na(income) & 
                         housing.type != "NA")

housing_income <- aggregate(income ~ housing.type, 
                            data = custdata_clean, 
                            FUN = mean)

ggplot(housing_income, aes(x = reorder(housing.type, income), y = income)) +
  geom_bar(stat = 'identity', fill = "steelblue") +
  labs(title = "Average Income by Housing Type",
       x = "Housing Type",
       y = "Average Income ($)") +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0, max(housing_income$income) * 1.1),
                     expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./q2-housing_income_barplot.png", 
       width = 10, height = 6, dpi = 300)

# Question 3
# 3.1: Extract subset of married customers with income > $50,000
married_high_income <- subset(custdata, 
                              marital.stat == "Married" & 
                              income > 50000)

head(married_high_income)
nrow(married_high_income)

# 3.2: Calculate percentage with health insurance
num_with_insurance <- sum(married_high_income$health.ins == TRUE, na.rm = TRUE)
total_customers <- nrow(married_high_income)
percentage_with_insurance <- (num_with_insurance / total_customers) * 100

cat("Number of married customers with income > $50,000:", total_customers, "\n")
cat("Number with health insurance:", num_with_insurance, "\n")
cat("Percentage with health insurance:", percentage_with_insurance, "%\n")

# 3.3: Compare with whole dataset
num_with_insurance_all <- sum(custdata$health.ins == TRUE, na.rm = TRUE)
total_customers_all <- nrow(custdata)
percentage_with_insurance_all <- (num_with_insurance_all / total_customers_all) * 100

cat("\nWhole dataset:\n")
cat("Total customers:", total_customers_all, "\n")
cat("Number with health insurance:", num_with_insurance_all, "\n")
cat("Percentage with health insurance:", percentage_with_insurance_all, "%\n")

cat("\nDifference:", 
    percentage_with_insurance - percentage_with_insurance_all, 
    "percentage points\n")

# Question 4
# 4.1: Remove age outliers (age > 100) and create scatterplot

custdata_no_outliers <- subset(custdata, age <= 100)

custdata_clean_corr <- subset(custdata_no_outliers, 
                              !is.na(age) & 
                              !is.na(income) & 
                              !is.na(num.vehicles))

ggplot(custdata_clean_corr, aes(x = age, y = income, color = num.vehicles)) +
  geom_point(alpha = 0.5) +
  labs(title = "Income vs Age (colored by Number of Vehicles)",
       x = "Age (years)",
       y = "Income ($)",
       color = "Number of Vehicles") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")

ggsave("./q4-scatterplot_age_income_vehicles.png", 
       width = 10, height = 6, dpi = 300)

# 4.2: Calculate correlation coefficients

cor_income_vehicles <- cor(custdata_clean_corr$income, 
                           custdata_clean_corr$num.vehicles, 
                           use = "complete.obs")

cor_vehicles_age <- cor(custdata_clean_corr$num.vehicles, 
                        custdata_clean_corr$age, 
                        use = "complete.obs")

cat("\n=== CORRELATION COEFFICIENTS ===\n")
cat("Correlation between Income and Number of Vehicles:", cor_income_vehicles, "\n")
cat("Correlation between Number of Vehicles and Age:", cor_vehicles_age, "\n")

if (abs(cor_income_vehicles) > abs(cor_vehicles_age)) {
  cat("\nBest correlation: Income and Number of Vehicles (", 
      cor_income_vehicles, ")\n", sep = "")
} else {
  cat("\nBest correlation: Number of Vehicles and Age (", 
      cor_vehicles_age, ")\n", sep = "")
}
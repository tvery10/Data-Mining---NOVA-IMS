install.packages("posterdown")

# Install and load necessary libraries
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")
install.packages("lmtest")

library(car)
library(lmtest)
library(sandwich)
library(tidyverse)  
library(readxl)
library(writexl)
library(lmtest)


# Load the dataset
billionaires <- read_excel("C:/Users/maria/OneDrive/Documentos/_NOVA/1st sem/Statistics/Project/billionaires.xlsx")

# Explore the structure of the dataset
str(billionaires)

# Summary statistics
summary(billionaires)

# Check for missing values
colSums(is.na(billionaires[,]))

# Distribution of key numerical variables
#hist(billionaires$valuation, main = "Distribution of Valuation", xlab = "Valuation")

# Boxplot to identify outliers
#boxplot(billionaires$valuation, main = "Boxplot of Valuation")

# Explore relationships between variables
#cor(billionaires$valuation, billionaires$askedfor)

# Visualize relationships with scatter plots
#plot(billionaires$valuation, billionaires$askedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Valuation", ylab = "Asked For")

# Visualize symmetry
#boxplot(billionaires$askedfor,
#        billionaires$exchangeforstake,
#       names=c("Asked For", "Stake"), col = c("lightgreen","orange"))



var <- c("gender", "selfMade", "age", "cpi_country", "cpi_change_country", "gdp_country", "total_tax_rate_country", "population_country")

# Converta as colunas para o formato 'numeric'
billionaires$cpi_country <- as.numeric(billionaires$cpi_country)
billionaires$cpi_change_country <- as.numeric(billionaires$cpi_change_country)
billionaires$gdp_country <- as.numeric(gsub("[\\$,]", "", billionaires$gdp_country))
billionaires$total_tax_rate_country <- as.numeric(billionaires$total_tax_rate_country)

summary(billionaires[var])



# Missing Values

# Check missing values for each variable
colSums(is.na(billionaires[,var]))

# missing values: age (65), cpi_country(184), cpi_change_country(184), gdp_country(164), total_tax_rate_country (182), population_country (164)

# List of specific countries to delete
countries_to_delete <- c("Andorra", "Bahamas", "Bermuda", "British Virgin Islands", "Cayman Islands", "Eswatini (Swaziland)", "Guernsey", "Hong Kong", "Ireland", "Liechtenstein", "Monaco", "Taiwan", "Turks and Caicos Islands", "Uzbekistan")

# Create a new dataset without rows containing the specified countries and rows with missing 'country'
billionaires_filtered <- subset(billionaires, !(country %in% countries_to_delete) & complete.cases(country))

colSums(is.na(billionaires_filtered[,var]))

total_rows_original <- nrow(billionaires)
total_rows_filtered <- nrow(billionaires_filtered)
# Calculate the percentage of rows deleted
percentage_rows_deleted <- ((nrow(billionaires) - total_rows_filtered) / total_rows_original)

cat("Number of rows in the original dataset:", total_rows_original, "\n")
cat("Number of rows in the filtered dataset:", total_rows_filtered, "\n")
cat("Percentage of rows deleted:", percentage_rows_deleted, "%\n")

# Replace 'age' missing values with the median
billionaires_filtered$age[is.na(billionaires_filtered$age)] <- median(billionaires_filtered$age, na.rm = TRUE)

colSums(is.na(billionaires_filtered[,var]))

bill <- billionaires_filtered



# Plots

# Explore relationships between variables

# Scatter plot for valuation and askedfor
#plot(billionaires$valuation, billionaires$askedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Valuation", ylab = "Asked For")

#plot(x = lvaluation, y = laskedfor, main = "Scatter Plot of Valuation vs Asked For", 
#     xlab = "Log Valuation", ylab = "Log Asked For")

# Barplot
#hist(valuation, nclass=20)
#hist(lvaluation, nclass=10)
#hist(askedfor, nclass=20)
#hist(laskedfor, nclass=20)
#hist(stake, nclass=20)
#hist(log(stake), nclass=20)

# Boxplot to compare valuation by category
#boxplot(billionaires$valuation ~ billionaires$category, main = "Boxplot of Valuation by Category", 
#        xlab = "Category", ylab = "Valuation")

# Cross-tabulation for multiple_entreprenuers and deal
#table(billionaires$multiple_entreprenuers, billionaires$deal)

# Correlation matrix for numeric variables
#cor_matrix <- cor(billionaires[, c("valuation", "askedfor", "exchangeforstake")])
#print(cor_matrix)

# Heatmap for correlation matrix
#heatmap(cor_matrix, annot = TRUE, main = "Correlation Heatmap")

# Explore the distribution of the dependent variable
#table(billionaires$deal)

# Bar plot for the distribution of categories
#barplot(table(billionaires$category), main = "Distribution of Categories", 
#        xlab = "Category", ylab = "Frequency", col = "skyblue")



# Model

# Variables
worth <- bill$finalWorth
gender <- bill$gender
selfmade <- bill$selfMade
age <- bill$age
cpi <- bill$cpi_country
cpi_change <- bill$cpi_change_country
gdp <- bill$gdp_country
tax_rate <- bill$total_tax_rate_country
pop <- bill$population_country


model <- lm(worth ~ gender + selfmade + age + cpi + cpi_change + gdp + tax_rate + pop, data = bill)
summary(model)


# Hypothesis Testing

# 1. Linearity Test (Visual inspection with residuals vs. fitted values plot)
plot(model, which = 1, main = "Linearity Test: Residuals vs Fitted")

# Interpretation: Se houver um padrão discernível ou não linearidade no gráfico, a suposição de linearidade pode ser violada.


plot(x = age, y = log(worth), main = "Scatter Plot")
plot(x = log(cpi), y = log(worth), main = "Scatter Plot")
plot(x = cpi_change, y = log(worth), main = "Scatter Plot")
plot(x = log(gdp), y = log(worth), main = "Scatter Plot")
plot(x = tax_rate, y = log(worth), main = "Scatter Plot")
plot(x = log(pop), y = log(worth), main = "Scatter Plot")

model4 <- lm(log(worth) ~ gender + selfmade + age + log(cpi) + cpi_change + log(gdp) + tax_rate + log(pop), data = bill)
summary(model4)
plot(model4, which = 1, main = "Linearity Test: Residuals vs Fitted")


# Testing Multiple Linear Restrictions

res.ur <- lm(log(worth) ~ gender + selfmade + age + log(cpi) + cpi_change + log(gdp) + tax_rate + log(pop), data = bill)
(r2.ur <- summary(res.ur)$r.squared)

res.r <- lm(log(worth) ~ selfmade + age + log(gdp) + tax_rate + log(pop), data = bill)
(r2.r <- summary(res.r)$r.squared)

(F <- ( (r2.ur-r2.r)/3 ) / ( (1-r2.ur)/(2456-8-1) ) )
1 - pf(F, 3, 2447)

final_model <- lm(log(worth) ~ selfmade + age + log(gdp) + tax_rate + log(pop), data = bill)
summary(final_model)


# 3. No perfect multicollinearity (Check variance inflation factors - VIF)
vif(final_model)


# 4. Homoskedasticity Test (Visual inspection with scale-location plot)

plot(final_model, which = 3, main = "Homoscedasticity Test: Scale-Location")
# Interpretation: Se houver um padrão discernível ou uma mudança na dispersão com os valores ajustados, a homoscedasticidade pode ser violada.

# Teste de Breusch-Pagan para Homoscedasticidade
bptest(final_model)
# Interpretation: Um valor de p próximo de 1 indica homoscedasticidade. Um valor significativamente diferente pode indicar heteroscedasticidade.



# RESET test

RESETreg <- lm(log(worth) ~ selfmade + age + log(gdp) + tax_rate + log(pop) + 
                 I(fitted(model4)^2) + I(fitted(model4)^3), data = bill)
RESETreg
#ou
linearHypothesis(RESETreg, matchCoefs(RESETreg,"fitted"))
#ou
resettest(model4)



# 6. Normally distributed errors (Check normality of residuals with a quantile-quantile plot)
qqnorm(resid(final_model))
qqline(resid(final_model))

# Additionally, a formal test for normality can be performed
shapiro.test(resid(final_model))






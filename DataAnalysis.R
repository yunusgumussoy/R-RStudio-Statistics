# SPDX-License-Identifier: MIT
# Yunus GUMUSSOY - The First Step of Being a Data Scientist

###Task 1: Selection of a Dataset 

library(AER)

mydata <- data("DoctorVisits")
attach(mydata)


###Task 2: Confidence Interval - Single Population

## For 95% confidence interval

# mean calculation
mean <- mean(DoctorVisits$income)

# size calculation
length(DoctorVisits$income)

## Degrees of Freedom 
#df = n - 1 = 5189

# standard deviation
sd <- sd(DoctorVisits$income)

# standard error
standard_error <- sd / sqrt(5190)

# t-score formula & calculation
t_score = qt(0.025, 5189, lower.tail = FALSE)

# margin error formula & calculation
margin_error <- t_score * standard_error

# lower bound and upper bound
lower_bound <- mean - margin_error
upper_bound <- mean + margin_error

lower_bound
upper_bound

## For 99% confidence interval

t_score = qt(0.005, 5189, lower.tail = FALSE)
margin_error <- t_score * standard_error

# lower bound and upper bound
lower_bound <- mean - margin_error
upper_bound <- mean + margin_error

lower_bound
upper_bound


###Task 3. Hypothesis Testing - Single Population

##long form 

# mean
mean <- mean(DoctorVisits$visits)

# size
length(DoctorVisits$visits)

#df = n - 1 = 5189

# standard deviation
sd <- sd(DoctorVisits$visits)

# standard error
standard_error <- sd / sqrt(5190)

t_score = qt(0.025, 5189, lower.tail = FALSE)

margin_error <- t_score * standard_error

# lower bound and upper bound
lower_bound <- mean - margin_error
upper_bound <- mean + margin_error

lower_bound
upper_bound

##short form 

#Ho: m = 0.3
#Ha: m ??? 0.3

t.test(DoctorVisits$visits, mu=0.3, alternative = "two.sided", conf.level = 0.95) 


###Task 4. 

# Dependent Variable (DV): Visits 
# Independent Variables (IVs): age + income + health 

summary(DoctorVisits$age)
summary(DoctorVisits$income)
summary(DoctorVisits$health)

##age
# Correlation
cor(DoctorVisits$visits, DoctorVisits$age)

# Plot function
plot(DoctorVisits$visits, DoctorVisits$age, main="Scatterplot Visits and Age",
     xlab="Age", ylab="Number of Visits", pch=19)

# Adding a regression line to a graph
abline(lm(visits ~ age, data = DoctorVisits), col = "red")

##income

cor(DoctorVisits$visits, DoctorVisits$income)

plot(DoctorVisits$visits, DoctorVisits$income, main="Scatterplot Visits and income",
     xlab="income", ylab="Number of Visits", pch=19)

abline(lm(visits ~ income, data = DoctorVisits), col = "red") # regression line

##health

cor(DoctorVisits$visits, DoctorVisits$health)

plot(DoctorVisits$visits, DoctorVisits$health, main="Scatterplot Visits and health score",
     xlab="health", ylab="Number of Visits", pch=19)

abline(lm(visits ~ health, data = DoctorVisits), col = "red") # regression line

#Regression Model 
# Fitting linear model
m1 <- lm(visits ~ age + income + health, data = DoctorVisits)

summary(m1)


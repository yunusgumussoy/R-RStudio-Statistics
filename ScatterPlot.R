# SPDX-License-Identifier: MIT
# Yunus GUMUSSOY - The First Step of Being a Data Scientist


#Data Summary

#Read in the survey sample data
survey_sample<-read.csv("Sample_dataset_R.csv")

#Representing Categorical data
survey_sample$wealth<-factor(survey_sample$wealth,levels=c(1,2,3,4,5),labels=c("poorest","poorer","middle","richer","richest"))
survey_sample$education<-factor(survey_sample$education,levels=c(0,1,2,3),labels=c("none","primary","secondary","higher"))
survey_sample$area<-factor(survey_sample$area,levels=c(1,2),labels=c("urban","rural"))

#Summary
summary(survey_sample)

#Table
table(survey_sample$sba, survey_sample$wealth)
table(survey_sample$sba, survey_sample$education)
table(survey_sample$sba, survey_sample$area)

counts <- table(survey_sample$sba)
barplot(counts, main="SBA Distribution",
        xlab="The Presence of SBA")

counts <- table(survey_sample$wealth)
barplot(counts, main="Wealth Distribution",
        xlab=" ")

counts <- table(survey_sample$education)
barplot(counts, main="Wealth Education",
        xlab=" ")

counts <- table(survey_sample$sba, survey_sample$wealth)
barplot(counts, main="Distribution of SBA across levels of Wealth",
        xlab="Level of Wealth", col=c("darkblue","red"),
        legend = rownames(counts))

counts <- table(survey_sample$sba, survey_sample$education)
barplot(counts, main="Distribution of SBA across levels of Education",
        xlab="Level of Education", col=c("darkblue","red"),
        legend = rownames(counts))

counts <- table(survey_sample$sba)
barplot(counts, main="Distribution of SBA based on Area",
        xlab="Area of Residence", col=c("darkblue","red"),
        legend = rownames(counts))

plot(survey_sample$wealth, survey_sample$education, main="Scatterplot",
     xlab="Education", ylab="Wealth", pch=19)

table(survey_sample$wealth, survey_sample$education)

counts <- table(survey_sample$sba, survey_sample$area)
barplot(counts, main="Distribution of SBA based on Area",
        xlab="Area of Residence", col=c("darkblue","red"),
        legend = rownames(counts))

boxplot(wealth~sba,data=survey_sample, main="Boxplot",
        xlab="The Presence of SBA", ylab="Levels of Wealth")


#Regression

data <- read.csv("gpa2021.csv")

summary(data)

#note; extra; condition; location

boxplot(gpa~note,data=data,names=c("always", "most of the time", "sometimes", "rarely", "never"), main="GPA-Taking Note",
        xlab="Take Notes", ylab="GPA") 

boxplot(gpa~extra,data=data,names=c("yes", "no"), main="GPA-Extra Resource",
        xlab="Extra Resource", ylab="GPA") 

boxplot(gpa~condition,data=data,names=c("yes", "no"), main="GPA-Condition",
        xlab="psychological/physical condition", ylab="GPA") 

boxplot(gpa~location,data=data,names=c("home", "library", "other"), main="GPA-Location",
        xlab="location", ylab="GPA")

#Scatter plot for assignment 

plot(data$assignment, data$gpa, main="GPA vs. Assignment",
     xlab="GPA", ylab="% of Done Assignment", pch=19)

# regression line (y~x)
abline(lm(data$gpa~data$study), col="red")  


##Scatter plot for attend  

plot(data$attend, data$gpa, main="GPA vs. Attendance",
     xlab="GPA", ylab="# of Attended Weeks", pch=19)
abline(lm(data$gpa~data$attend), col="red") 


##Scatter plot for study  

plot(data$study, data$gpa, main="GPA vs. Study",
     xlab="GPA", ylab="# of Study hours", pch=19)
abline(lm(data$gpa~data$study), col="red") 

##Scatter plot for sleep   

plot(data$sleep, data$gpa, main="GPA vs. sleep",
     xlab="GPA", ylab="# of Sleeping hours", pch=19)
abline(lm(data$gpa~data$sleep), col="red") 

#Histogram
hist(data$sleep)

##Scatter plot for leisure   

plot(data$leisure, data$gpa, main="GPA vs. leisure",
     xlab="GPA", ylab="# of leisure hours", pch=19)
abline(lm(data$gpa~data$leisure), col="red")

#Histogram
hist(data$leisure)

##Scatter plot for active   

plot(data$active, data$gpa, main="GPA vs. Active participation",
     xlab="GPA", ylab="active participation", pch=19)
abline(lm(data$gpa~data$active), col="red")

##Scatter plot for motivation   

plot(data$motivated, data$gpa, main="GPA vs. Motivation",
     xlab="GPA", ylab="active participation", pch=19)
abline(lm(data$gpa~data$motivated), col="red")



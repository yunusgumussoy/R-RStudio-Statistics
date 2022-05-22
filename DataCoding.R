# SPDX-License-Identifier: MIT
# Yunus GUMUSSOY - The First Step of Being a Data Scientist

library(car)

# Data Importing from a CSV file
data <- read.csv("Data_Excel.csv")


###op1 - op6 = optimisim scale

#reverse items in op2, op4, op6 

data$op2_reversed <- recode(data$op2, "1=5; 2=4; 3=3; 4=2; 5=1")

data$op4_reversed <- recode(data$op4, "1=5; 2=4; 3=3; 4=2; 5=1")

data$op6_reversed <- recode(data$op6, "1=5; 2=4; 3=3; 4=2; 5=1")


#calculate optimism scale by taking mean of values

data$optimism_scale <- (data$op1 + data$op2_reversed + data$op3 + data$op4_reversed + data$op5 + data$op6_reversed)/6



###pss1 - pss10 = perceived stress scale 

#reverse items in pss4, pss5, pss7, pss8 

data$pss4_reversed <- recode(data$pss4, "1=5; 2=4; 3=3; 4=2; 5=1")

data$pss5_reversed <- recode(data$pss5, "1=5; 2=4; 3=3; 4=2; 5=1")

data$pss7_reversed <- recode(data$pss7, "1=5; 2=4; 3=3; 4=2; 5=1")

data$pss8_reversed <- recode(data$pss8, "1=5; 2=4; 3=3; 4=2; 5=1")


#calculate perceived stress scale by summing values 

data$perceivedstress <- data$pss1 + data$pss2 + data$pss3 + data$pss4_reversed + data$pss5_reversed + data$pss6 + data$pss7_reversed +
  data$pss8_reversed + data$pss9 + data$pss10


### sest1 - sest10 = self esteem scale 

#reverse items in sest3, sest5, sest7, sest9, sest10

data$sest3_reversed <- recode(data$sest3, "1=4; 2=3; 3=2; 4=1")

data$sest5_reversed <- recode(data$sest5, "1=4; 2=3; 3=2; 4=1")

data$sest7_reversed <- recode(data$sest7, "1=4; 2=3; 3=2; 4=1")

data$sest9_reversed <- recode(data$sest9, "1=4; 2=3; 3=2; 4=1")

data$sest10_reversed <- recode(data$sest10, "1=4; 2=3; 3=2; 4=1")


#calculate self esteem scale by summing values 

data$selfesteem <- data$sest1 + data$sest2 + data$sest3_reversed + data$sest4 + data$sest5_reversed + data$sest6 + data$sest7_reversed +
  data$sest8 + data$sest9_reversed + data$sest10_reversed


#Correlation Analysis (optimism scale, perceived stress scale, selfesteem scale)

# ggpubr is used to create graphs
library("ggpubr")

# Correlation analysis between optimisim scale and perceived stress scale 

cor.test(data$optimism_scale, data$perceivedstress)

ggscatter(data, x = "optimism_scale", y = "perceivedstress", 
          add = "reg.line", cor.coef = TRUE, cor.method = "pearson")

# Correlation analysis between optimism scale and selfesteem scale

cor.test(data$optimism_scale, data$selfesteem)

ggscatter(data, x = "optimism_scale", y = "selfesteem", 
          add = "reg.line", cor.coef = TRUE, cor.method = "pearson")

# Correlation analysis between perceived stress scale and selfesteem scale 

cor.test(data$selfesteem, data$perceivedstress)

ggscatter(data, x = "selfesteem", y = "perceivedstress", 
          add = "reg.line", cor.coef = TRUE, cor.method = "pearson")







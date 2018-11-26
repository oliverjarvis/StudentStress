setwd("/Users/oliverjarvis/Cognitive Science/ExpMeth/Assignment 4")

library(pacman)
p_load(tidyverse, stringr, lmer, car)

data <- read_csv("data.csv")

#Turning the conditions into factors
data$Acceptance <- as.factor(data$Acceptance)

#Summing all the scores
data$score <- rowSums(data[,2:11])
data$ID <- c(1:30)

#Anova
model <- lm(score ~ Acceptance, data=data)
summary(model)

#Checking normality of residuals
plot(model)

#Homogenity of variance (We want that.) The value is non-significant i
leveneTest(data$score ~ data$Acceptance, data)

#Doing t-test between all pairs.
pairwise.t.test(data$score, data$Acceptance, p.adjust.method = "BH")



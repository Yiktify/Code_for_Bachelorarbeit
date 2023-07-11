library(dplyr)
library(ltm)
library(ggplot2)
library(psych)
library(DescTools)
library(pscl)
library(caret)
library(car)
library(InformationValue)
library(glmnet)
library(MASS)
library(brant)
library(visreg)
library(effects)

#each regression
logis_all <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                      E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D10 != "NA" & C5 != "NA" &
                      C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                      B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                      B1_12 != "NA" & B1_13 != "NA" ,
                    select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D10, C5,
                               B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))

##1
logis_all$C5 <- 5 - logis_all$C5
logis_all$C5 <- factor(logis_all$C5, levels = c(0, 1, 2, 3, 4),
                       labels = c("None of the time", "A little of the time", "Some of the time",
                                  "Most of the time", "All of the time"))
logfit_1 <- glm(formula = scale ~ C5, data = logis_all, family = binomial)
summary(logfit_1)
exp(coef(logfit_1))


##2
logis_all$E3 <- factor(logis_all$E3, levels = c(1, 2), labels = c("male", "female"))
logfit_2 <- glm(formula = scale ~ E3, data = logis_all, family = binomial)
summary(logfit_2)
exp(coef(logfit_2))


##3
logis_all$E4 <- factor(logis_all$E4, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("18-24 years", "25-34 years", "35-44 years", "45-54 years",
                                  "55-64 years", "65-74 years", "75 years or older"))
logfit_3 <- glm(formula = scale ~ E4, data = logis_all, family = binomial)
summary(logfit_3)
exp(coef(logfit_3))


##4
logis_all$E8 <- factor(logis_all$E8, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("No formal schooling", "Less than primary school", "Primary school completed",
                                  "Secondary school completed", "High school (or equivalent) completed",
                                  "College/ pre-university/ University completed", "University post-graduate degree completed"))
logfit_4 <- glm(formula = scale ~ E8, data = logis_all, family = binomial)
summary(logfit_4)
exp(coef(logfit_4))

logfit_4_2 <- glm(formula = scale ~ school, data = logis_all, family = binomial)
summary(logfit_4_2)
exp(coef(logfit_4_2))


##5
logis_all$E2 <- factor(logis_all$E2, levels = c(1, 2, 3), labels = c("City", "Town", "Village or rural area"))
logfit_5 <- glm(formula = scale ~ E2, data = logis_all, family = binomial)
summary(logfit_5)
exp(coef(logfit_5))


##6
logis_all$V12 <- factor(logis_all$V12, levels = c(1, 2), labels = c("Yes", "No"))
logfit_6 <- glm(formula = scale ~ V12, data = logis_all, family = binomial)
summary(logfit_6)
exp(coef(logfit_6))


##7
logis_all$D10 <- factor(logis_all$D10, levels = c(7, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels = c("Health", "Agriculture", "Buying and selling", "Construction", "Education",
                                   "Electricity/ water/ gas/ waste", "Financial/ insurance/ real estate services",
                                   "Manufacturing", "Mining", "Personal services",
                                   "Professional/ scientific/ technical activities", "Public administration",
                                   "Tourism", "Transportation", "Other"))
logfit_7 <- glm(formula = scale ~ D10, data = logis_all, family = binomial)
summary(logfit_7)
exp(coef(logfit_7))

logfit_7_2 <- glm(formula = scale ~ health, data = logis_all, family = binomial)
summary(logfit_7_2)
exp(coef(logfit_7_2))


##8
logfit_8 <- glm(formula = scale ~ have, data = logis_all, family = binomial)
summary(logfit_8)
exp(coef(logfit_8))

logfit_9 <- glm(formula = scale ~ sym, data = logis_all, family = binomial)
summary(logfit_9)
exp(coef(logfit_9))


##10
logis_all_2 <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                        E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D7a != "NA" & C5 != "NA" &
                        C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                        B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                        B1_12 != "NA" & B1_13 != "NA" ,
                      select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D7a, C5,
                                 B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
logis_all_2 <- get_mental(logis_all_2)
logis_all_2$D7a <- factor(logis_all_2$D7a, levels = c(1, 2), labels = c("Employed", "Unemployed"))
logfit_10 <- glm(formula = scale ~ D7a, data = logis_all_2, family = binomial)
summary(logfit_10)
exp(coef(logfit_10))




#compare 6 models
logis_all <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                      E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D7a != "NA" & C5 != "NA" &
                      C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                      B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                      B1_12 != "NA" & B1_13 != "NA" ,
                    select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D7a, C5,
                               B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
logis_all$C5 <- 5 - logis_all$C5
logis_all[, c("E3", "E4", "E8", "E2", "V12", "D7a")] <- lapply(logis_all[, c("E3", "E4", "E8", "E2", "V12", "D7a")], factor)
logis_all$C5 <- as.factor(logis_all$C5)
logis_all <- get_mental(logis_all)
logis_all <- get_symptom(logis_all)
logis_all <- get_school(logis_all)
summary(logis_all)


output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym,
              data = logis_all, family = binomial)
options(scipen = 100)
summary(output)
exp(coef(output))
pscl::pR2(output)["McFadden"]
car::vif(output)
varimp <- caret::varImp(output)

imp <- cbind(rownames(varimp), varimp$Overall)
imp <- as.data.frame(imp)
colnames(imp) <- c("variables", "value")
imp$value <- as.numeric(imp$value)
imp$variables <- factor(imp$variables,
                        levels = c("C51", "C52", "C53", "C54", "E32", "E42", "E43", "E44", "E45",
                                   "E46", "E47", "schoolHigh School", "schoolUniversity or higher",
                                   "E22", "E23", "V122", "D7a2", "symModerate", "symSevere", "symCritical"),
                        labels = c("A little of the time", "Some of the time", "Most of the time", "All of the time",
                                   "Female", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years",
                                   "75 years or older", "High School", "University or higher", "Town", "Village or rural area",
                                   "Non-smoker", "Unemployed", "Moderate", "Severe", "Critical"))
ggplot(imp, aes(reorder(variables, value), value)) +
  geom_bar(stat = "identity") +
  coord_flip()


cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal)    
plotROC(test$scale, log_predict)




##########


logis_all <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                      E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D7a != "NA" & C5 != "NA" &
                      C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                      B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                      B1_12 != "NA" & B1_13 != "NA" ,
                    select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D7a, C5,
                               B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
logis_all$C5 <- 5 - logis_all$C5
logis_all[, c("E3", "E4", "E8", "E2", "V12", "D7a")] <- lapply(logis_all[, c("E3", "E4", "E8", "E2", "V12", "D7a")], factor)
logis_all$C5 <- as.factor(logis_all$C5)
logis_all <- get_mental(logis_all)
logis_all <- get_symptom(logis_all)
logis_all <- get_school(logis_all)
summary(logis_all)


output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + have,
              data = logis_all, family = binomial)
options(scipen = 100)
summary(output)
pscl::pR2(output)["McFadden"]
car::vif(output)
cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal)  
plotROC(test$scale, log_predict)



########

logis_all <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                      E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D10 != "NA" & C5 != "NA" &
                      C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                      B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                      B1_12 != "NA" & B1_13 != "NA" ,
                    select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D10, C5,
                               B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
logis_all$C5 <- 5 - logis_all$C5
logis_all[, c("E3", "E4", "E8", "E2", "V12")] <- lapply(logis_all[, c("E3", "E4", "E8", "E2", "V12")], factor)
logis_all$D10 <- factor(logis_all$D10, levels = c(7, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels = c("Health", "Agriculture", "Buying and selling", "Construction", "Education",
                                   "Electricity/ water/ gas/ waste", "Financial/ insurance/ real estate services",
                                   "Manufacturing", "Mining", "Personal services",
                                   "Professional/ scientific/ technical activities", "Public administration",
                                   "Tourism", "Transportation", "Other"))
logis_all$C5 <- as.factor(logis_all$C5)
logis_all <- get_mental(logis_all)
logis_all <- get_symptom(logis_all)
logis_all <- get_school(logis_all)
summary(logis_all)


output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D10 + have,
              data = logis_all, family = binomial)
options(scipen = 100)
summary(output)
pscl::pR2(output)["McFadden"]
car::vif(output)
cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal)
plotROC(test$scale, log_predict)


########

output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D10 + sym,
              data = logis_all, family = binomial)
options(scipen = 100)
summary(output)
pscl::pR2(output)["McFadden"]
car::vif(output)
cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal) 
plotROC(test$scale, log_predict)


#######

logis_all <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E3 != "NA" & E3 != 3 &
                      E3 != 4 & E4 != "NA" & E8 != "NA" & E2 != "NA" & V12 != "NA" & D10 != "NA" & C5 != "NA" &
                      C5 != 6 & B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                      B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                      B1_12 != "NA" & B1_13 != "NA" ,
                    select = c(D1, D2, D4, D5, E3, E4, E8, E2, V12, D10, C5,
                               B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
logis_all$C5 <- 5 - logis_all$C5
logis_all[, c("E3", "E4", "E8", "E2", "V12")] <- lapply(logis_all[, c("E3", "E4", "E8", "E2", "V12")], factor)
logis_all$D10 <- factor(logis_all$D10, levels = c(7, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels = c("Health", "Agriculture", "Buying and selling", "Construction", "Education",
                                   "Electricity/ water/ gas/ waste", "Financial/ insurance/ real estate services",
                                   "Manufacturing", "Mining", "Personal services",
                                   "Professional/ scientific/ technical activities", "Public administration",
                                   "Tourism", "Transportation", "Other"))
logis_all$C5 <- as.factor(logis_all$C5)
logis_all <- get_mental(logis_all)
logis_all <- get_symptom(logis_all)
logis_all <- get_school(logis_all)
logis_all <- get_health(logis_all)
summary(logis_all)

output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + health + sym,
              data = logis_all, family = binomial)

options(scipen = 100)
summary(output)
exp(coef(output))
pscl::pR2(output)["McFadden"] 
caret::varImp(output)
car::vif(output)

cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

exp(coef(output))  

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal) 
plotROC(test$scale, log_predict)


#################


output <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + health + have,
              data = logis_all, family = binomial)

options(scipen = 100)
summary(output)
exp(coef(output))
pscl::pR2(output)["McFadden"]  
#Variable Importance
caret::varImp(output)
#VIF Values
car::vif(output)

cook <- cooks.distance(output)
cook[cooks.distance(output) > 0.5]
max(cook)

#confint(output_1)
exp(coef(output))   #or value

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(logis_all), replace = TRUE, prob = c(0.7, 0.3))
train <- logis_all[sample, ]
test <- logis_all[!sample, ]

log_predict <- predict(output, test, type = "response")

test$scale <- ifelse(test$scale == "depression", 1, 0)
optimal <- optimalCutoff(test$scale, log_predict)[1]
optimal

confusionMatrix(test$scale, log_predict)
misClassError(test$scale, log_predict, threshold = optimal) 
plotROC(test$scale, log_predict)
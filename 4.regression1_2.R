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
library(ggplot2)
library(reshape2)
library(patchwork)

#regression with interaction
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


output1 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym +
                 E3:C5,
               data = logis_all, family = binomial)
options(scipen = 100)
summary(output1)
pscl::pR2(output1)["McFadden"]
car::vif(output1, type = "predictor")


output2 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym +
                 E3:C5 + D7a:C5 + E4:C5,
               data = logis_all, family = binomial)
options(scipen = 100)
summary(output2)
pscl::pR2(output2)["McFadden"]
car::vif(output2, type = "predictor")


output3 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym +
                 E3:C5 + D7a:C5 + E4:C5 + sym:C5,
               data = logis_all, family = binomial)
options(scipen = 100)
summary(output3)
pscl::pR2(output3)["McFadden"]
car::vif(output3, type = "predictor")



output_1 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + E3:C5, data = logis_all, family = binomial)
summary(output_1)

output_2 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + E4:C5, data = logis_all, family = binomial)
summary(output_2)

output_3 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + school:C5, data = logis_all, family = binomial)
summary(output_3)

output_4 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + E2:C5, data = logis_all, family = binomial)
summary(output_4)

output_5 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + V12:C5, data = logis_all, family = binomial)
summary(output_5)

output_6 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + D7a:C5, data = logis_all, family = binomial)
summary(output_6)

output_7 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym + sym:C5, data = logis_all, family = binomial)
summary(output_7)


summary(output_1)  #
summary(output_2)  #x
summary(output_3)  #x
summary(output_4)  #x
summary(output_5)  #x
summary(output_6)  #x
summary(output_7)  #



output2 <- glm(formula = scale ~ C5 + E3 + E4 + school + E2 + V12 + D7a + sym +
                 E3:C5 + + V12:C5 + D7a:C5,
               data = logis_all, family = binomial)
options(scipen = 100)
summary(output2)
car::vif(output2)

varimp2 <- caret::varImp(output2)

imp2 <- cbind(rownames(varimp2), varimp2$Overall)
imp2 <- as.data.frame(imp2)
colnames(imp2) <- c("variables", "value")
imp2$value <- as.numeric(imp2$value)
imp2$variables <- factor(imp2$variables,
                         levels = c("C51", "C52", "C53", "C54", "E32", "E42", "E43", "E44", "E45",
                                    "E46", "E47", "schoolHigh School", "schoolUniversity or higher",
                                    "E22", "E23", "V122", "D7a2", "symModerate", "symSevere", "symCritical",
                                    "C51:E32", "C52:E32", "C53:E32", "C54:E32", "C51:V122", "C52:V122", "C53:V122",
                                    "C54:V122", "C51:D7a2", "C52:D7a2", "C53:D7a2", "C54:D7a2"),
                         labels = c("A little of the time", "Some of the time", "Most of the time", "All of the time",
                                    "Female", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years",
                                    "75 years or older", "High School", "University or higher", "Town", "Village or rural area",
                                    "Non-smoker", "Unemployed", "Moderate", "Severe", "Critical", "inter1", "inter1", "inter1",
                                    "inter1", "inter1", "inter1", "inter1", "inter1", "inter1", "inter1", "inter1", ))
ggplot(imp2, aes(reorder(variables, value), value)) +
  geom_bar(stat = "identity") +
  coord_flip()



#interaction plot
get_exp <- function(x){
  exp(x) / (1 + exp(x))
}


a <- c(-0.237, 0.090, 0.235, 0.357, 0.289)
b <- c(-0.094, 0.282, 0.416, 0.631, 0.671)

x <- cbind(exp(a), exp(b), c(0, 1, 2, 3, 4))
x <- data.frame(x)
colnames(x) <- c("male", "female", "mask")

x <- melt(x, id.vars = "mask", measure.vars = c("male", "female"),
          variable.name = "gender", na.rm = FALSE, value.name = "value")

x$mask <- factor(x$mask, levels = c(0, 1, 2, 3, 4))
x$gender <- factor(x$gender, levels = c("male", "female"))
summary(x)

ggplot(x, aes(mask, value, group = gender)) +
  geom_point(aes(color = gender)) +
  geom_line(aes(color = gender)) +
  scale_color_manual(values = c("male" = "steelblue", "female" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(0.75, 2) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))







a <- c(-0.237, 0.090, 0.235, 0.357, 0.289)
b <- c(-0.745, -0.288, -0.147, 0.062, 0.087)

y <- cbind(exp(a), exp(b), c(0, 1, 2, 3, 4))
y <- data.frame(y)
colnames(y) <- c("smoker", "non-smoker", "mask")

y <- melt(y, id.vars = "mask", measure.vars = c("smoker", "non-smoker"),
          variable.name = "smoking", na.rm = FALSE, value.name = "value")

y$mask <- factor(y$mask, levels = c(0, 1, 2, 3, 4))
y$smoking <- factor(y$smoking, levels = c("smoker", "non-smoker"))
summary(y)

ggplot(y, aes(mask, value, group = smoking)) +
  geom_point(aes(color = smoking)) +
  geom_line(aes(color = smoking)) +
  scale_color_manual(values = c("smoker" = "steelblue", "non-smoker" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(0.4, 1.5) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))







a <- c(-0.237, 0.090, 0.235, 0.357, 0.289)
b <- c(0.084, 0.396, 0.536, 0.635, 0.456)

z <- cbind(exp(a), exp(b), c(0, 1, 2, 3, 4))
z <- data.frame(z)
colnames(z) <- c("employed", "unemployed", "mask")

z <- melt(z, id.vars = "mask", measure.vars = c("employed", "unemployed"),
          variable.name = "working", na.rm = FALSE, value.name = "value")

z$mask <- factor(z$mask, levels = c(0, 1, 2, 3, 4))
z$working <- factor(z$working, levels = c("employed", "unemployed"))
summary(z)

ggplot(z, aes(mask, value, group = working)) +
  geom_point(aes(color = working)) +
  geom_line(aes(color = working)) +
  scale_color_manual(values = c("employed" = "steelblue", "unemployed" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(0.75, 2) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))








###########################
a1 <- round(get_exp(c(-0.237, 0.090, 0.235, 0.357, 0.289)), 4)
b1 <- round(get_exp(c(-0.094, 0.282, 0.416, 0.631, 0.671)), 4)

x <- cbind(a1, b1, c(0, 1, 2, 3, 4))
x <- data.frame(x)
colnames(x) <- c("male", "female", "mask")

x <- melt(x, id.vars = "mask", measure.vars = c("male", "female"),
          variable.name = "gender", na.rm = FALSE, value.name = "value")

x$mask <- factor(x$mask, levels = c(0, 1, 2, 3, 4))
x$gender <- factor(x$gender, levels = c("male", "female"))
summary(x)

p1 <- ggplot(x, aes(mask, value, group = gender)) +
  geom_point(aes(color = gender)) +
  geom_line(aes(color = gender)) +
  scale_color_manual(values = c("male" = "steelblue", "female" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Probability of having depression") +
  ylim(0.4, 0.7) +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))



a2 <- c(1, 1.387, 1.603, 1.811, 1.693)
b2 <- round(c(1.153, 1.387*1.153*1.05, 1.603*1.153*1.038, 1.811*1.153*1.14, 1.693*1.153*1.27), 3)

x <- cbind(a2, b2, c(0, 1, 2, 3, 4))
x <- data.frame(x)
colnames(x) <- c("male", "female", "mask")

x <- melt(x, id.vars = "mask", measure.vars = c("male", "female"),
          variable.name = "gender", na.rm = FALSE, value.name = "value")

x$mask <- factor(x$mask, levels = c(0, 1, 2, 3, 4))
x$gender <- factor(x$gender, levels = c("male", "female"))
summary(x)

p2 <- ggplot(x, aes(mask, value, group = gender)) +
  geom_point(aes(color = gender)) +
  geom_line(aes(color = gender)) +
  scale_color_manual(values = c("male" = "steelblue", "female" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(1, 2.5) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))
p2


##############################
a3 <- round(get_exp(c(-0.237, 0.090, 0.235, 0.357, 0.289)), 4)
b3 <- round(get_exp(c(-0.745, -0.288, -0.147, 0.062, 0.087)), 4)

y <- cbind(a3, b3, c(0, 1, 2, 3, 4))
y <- data.frame(y)
colnames(y) <- c("smoker", "non-smoker", "mask")

y <- melt(y, id.vars = "mask", measure.vars = c("smoker", "non-smoker"),
          variable.name = "smoking", na.rm = FALSE, value.name = "value")

y$mask <- factor(y$mask, levels = c(0, 1, 2, 3, 4))
y$smoking <- factor(y$smoking, levels = c("smoker", "non-smoker"))
summary(y)

p3 <- ggplot(y, aes(mask, value, group = smoking)) +
  geom_point(aes(color = smoking)) +
  geom_line(aes(color = smoking)) +
  scale_color_manual(values = c("smoker" = "steelblue", "non-smoker" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Probability of having depression") +
  ylim(0.3, 0.6) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))


a4 <- c(1, 1.387, 1.603, 1.811, 1.693)
b4 <- round(c(0.602, 1.387*0.602*1.139, 1.603*0.602*1.135, 1.811*0.602*1.238, 1.693*0.602*1.358), 3)

y <- cbind(a4, b4, c(0, 1, 2, 3, 4))
y <- data.frame(y)
colnames(y) <- c("smoker", "non-smoker", "mask")

y <- melt(y, id.vars = "mask", measure.vars = c("smoker", "non-smoker"),
          variable.name = "smoking", na.rm = FALSE, value.name = "value")

y$mask <- factor(y$mask, levels = c(0, 1, 2, 3, 4))
y$smoking <- factor(y$smoking, levels = c("smoker", "non-smoker"))
summary(y)

p4 <- ggplot(y, aes(mask, value, group = smoking)) +
  geom_point(aes(color = smoking)) +
  geom_line(aes(color = smoking)) +
  scale_color_manual(values = c("smoker" = "steelblue", "non-smoker" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(0.6, 2) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))


###########
a5 <- round(get_exp(c(-0.237, 0.090, 0.235, 0.357, 0.289)), 4)
b5 <- round(get_exp(c(0.084, 0.396, 0.536, 0.635, 0.456)), 4)

z <- cbind(a5, b5, c(0, 1, 2, 3, 4))
z <- data.frame(z)
colnames(z) <- c("employed", "unemployed", "mask")

z <- melt(z, id.vars = "mask", measure.vars = c("employed", "unemployed"),
          variable.name = "working", na.rm = FALSE, value.name = "value")

z$mask <- factor(z$mask, levels = c(0, 1, 2, 3, 4))
z$working <- factor(z$working, levels = c("employed", "unemployed"))
summary(z)

p5 <- ggplot(z, aes(mask, value, group = working)) +
  geom_point(aes(color = working)) +
  geom_line(aes(color = working)) +
  scale_color_manual(values = c("employed" = "steelblue", "unemployed" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Probability of having depression") +
  ylim(0.4, 0.7) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))


a6 <- c(1, 1.387, 1.603, 1.811, 1.693)
b6 <- round(c(1.378, 1.387*1.378*0.985, 1.603*1.378*0.980, 1.811*1.378*0.958, 1.693*1.378*0.858), 3)

z <- cbind(a6, b6, c(0, 1, 2, 3, 4))
z <- data.frame(z)
colnames(z) <- c("employed", "unemployed", "mask")

z <- melt(z, id.vars = "mask", measure.vars = c("employed", "unemployed"),
          variable.name = "working", na.rm = FALSE, value.name = "value")

z$mask <- factor(z$mask, levels = c(0, 1, 2, 3, 4))
z$working <- factor(z$working, levels = c("employed", "unemployed"))
summary(z)

p6 <- ggplot(z, aes(mask, value, group = working)) +
  geom_point(aes(color = working)) +
  geom_line(aes(color = working)) +
  scale_color_manual(values = c("employed" = "steelblue", "unemployed" = "#C65C5C")) +
  labs(x = "Frequency of mask usage", y = "Odds ratio of having depression") +
  ylim(1, 2.5) +
  geom_hline(yintercept = 1, linetype = "dotted", size = 0.7) +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))
library(dplyr)
library(ltm)
library(ggplot2)
library(psych)
library(DescTools)
library(RColorBrewer)
library(scales)

#A2_2_1: country
#B1: following symptoms
#B7: Have you been tested for COVID-19
#B8a: Have you been tested positve for COVID-19
#V1: Have you had a COVID-19 vaccination?
#V6_4: Answer.I plan to use masks or other precautions instead
#E3: gender
#E4: age
#E8: highest level of education
#E2: the area where you are staying
#C14a: how often did you intentionally avoid contact with other people?
#C5: how often did you wear a mask when in public?
#C0a: in the past 24 hours, where have you been?
#C13a: during which palces in the past 24 hours did you wear a mask?
#G1: How much do you worry about catching COVID-19?
#G2: How effective is social distancing for preventing the spread of COVID-19?
#G3: How effective is wearing a face mask for preventing the spread of COVID-19?
#H1: how many people maintained a distance of at least 1 meter from others?
#H2: how many people would you estimate wore masks when out?
#I7:what information do you want?
#I7_7: How to maintain my mental health
#I7_8: How to maintain my social relationships despite physical distancing
#D1: feel so nervous
#D2: feel so depressed
#D4: worried about having enough to eat
#D5: worried about household’s finances
#V11: Are you currently pregnant?
#V12: Do you smoke cigarettes?
#J3: do you have a children under age 18?
#D7a: do you have work?
#D10: what kind of business or organization


#table 1

#None 0-2
#Mild 3-5
#Moderate 6-8
#Severe 9-12
get_mental <- function(df){
  df$D1[df$D1 == 5] <- 0
  df$D2[df$D2 == 5] <- 0
  df$D1[df$D1 == 4] <- 1
  df$D2[df$D2 == 4] <- 1
  df$D1[df$D1 == 3] <- 2
  df$D2[df$D2 == 3] <- 2
  df$D1[df$D1 == 2] <- 3
  df$D2[df$D2 == 2] <- 3
  df$D1[df$D1 == 1] <- 3
  df$D2[df$D2 == 1] <- 3
  df$D4 <- 4 - df$D4
  df$D5 <- 4 - df$D5
  df$PHQ <- rowSums(df[, colnames(df) %in% c("D1", "D2", "D4", "D5")])
  df$scale <- "0"
  df[df$PHQ <= 5, ]$scale <- "non-depression"
  df[df$PHQ >= 6, ]$scale <- "depression"
  df$scale <- factor(df$scale, levels = c("non-depression", "depression"))
  df$type <- "0"
  df[df$PHQ >= 0 & df$PHQ <= 2, ]$type <- "None"
  df[df$PHQ >= 3 & df$PHQ <= 5, ]$type <- "Mild"
  df[df$PHQ >= 6 & df$PHQ <= 8, ]$type <- "Moderate"
  df[df$PHQ >= 9, ]$type <- "Severe"
  df$type <- factor(df$type, levels = c("None", "Mild", "Moderate", "Severe"))
  return(df)
}

df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA", select = c(D1, D2, D4, D5))
cronbach.alpha(df[1:2])
cronbach.alpha(df[3:4])
cronbach.alpha(df[1:4])



df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" &
               E3 != "NA" & E3 != 3 & E3 != 4, select = c(D1, D2, D4, D5, E3))
df <- get_mental(df)

##plot1
df$PHQ <- factor(df$PHQ, levels = c("0", "1", "2", "3","4", "5", "6", "7", "8", "9", "10", "11", "12"))
tab <- as.data.frame(xtabs(~ PHQ, data = df))
color_1 <- colorRampPalette(brewer.pal(3, "BuPu"))(13)
ggplot(tab, aes(PHQ, Freq, fill = PHQ)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0, 1000000, 2000000), labels = c("0","1M","2M")) +
  scale_fill_manual(values = color_1) +
  labs(x = "PHQ-Value", y = "Number of people") +
  theme(legend.position = "none")

ggplot(tab, aes(PHQ, Freq)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0, 1000000, 2000000), labels = c("0","1M","2M")) +
  labs(x = "PHQ-Value", y = "Number of people") +
  theme(legend.position = "none")

tab <- as.data.frame(xtabs(~ scale, data = df))
ggplot(tab, aes("", Freq, fill = scale)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#D5E4EF", "#8B67AF")) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank())


pie(c(6543814, 4019353), labels = c("non-depression (61.95%)", "depression (38.05%)"),
    col = c("#D5E4EF", "#8B67AF"))
legend("topright", c("non-depression", "depression"), cex = 1,2, fill = c("#D5E4EF", "#8B67AF"))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" &
               E3 != "NA" & E3 != 3 & E3 != 4, select = c(D1, D2, D4, D5, E3))
df <- get_mental(df)
summary(df)

describeBy(df$PHQ, df$E3)
describeBy(df$D1, df$E3)
describeBy(df$D2, df$E3)
describeBy(df$D4, df$E3)
describeBy(df$D5, df$E3)

mean(df$PHQ)
mean(df$D1)
mean(df$D2)
mean(df$D4)
mean(df$D5)

sd(df$PHQ)
sd(df$D1)
sd(df$D2)
sd(df$D4)
sd(df$D5)

var.test(PHQ ~ E3, data = df)
var.test(D1 ~ E3, data = df)
var.test(D2 ~ E3, data = df)
var.test(D4 ~ E3, data = df)
var.test(D5 ~ E3, data = df)

t.test(df$PHQ ~ df$E3, var.equal = F)
t.test(df$D1 ~ df$E3, var.equal = F)
t.test(df$D2 ~ df$E3, var.equal = F)
t.test(df$D4 ~ df$E3, var.equal = F)
t.test(df$D5 ~ df$E3, var.equal = F)

xtabs(~ type, data = df)
xtabs(~ E3 + type, data = df)
chisq.test(xtabs(~ E3 + type, data = df))

xtabs(~ scale, data = df)
xtabs(~ E3 + scale, data = df)
chisq.test(xtabs(~ E3 + scale, data = df))





#table 2
df <- subset(df_1_60, E4 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(E4, E3))
df$E4 <- factor(df$E4, levels = c(1, 2, 3, 4, 5, 6, 7),
                labels = c("18-24 years", "25-34 years", "35-44 years", "45-54 years",
                           "55-64 years", "65-74 years", "75 years or older"))
xtabs(~ E3 + E4, data = df)
chisq.test(xtabs(~ E3 + E4, data = df))

df <- subset(df_1_60, E8 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(E8, E3))
get_school <- function(df){
  df$school <- "0"
  df$school[df$E8 == 1] <- "Elementary or below"
  df$school[df$E8 == 2] <- "Elementary or below"
  df$school[df$E8 == 3] <- "Elementary or below"
  df$school[df$E8 == 4] <- "High School"
  df$school[df$E8 == 5] <- "High School"
  df$school[df$E8 == 6] <- "University or higher"
  df$school[df$E8 == 7] <- "University or higher"
  df$school <- factor(df$school, levels = c("Elementary or below", "High School", "University or higher"))
  return(df)
}
df <- get_school(df)
xtabs(~ E3 + school, data = df)
chisq.test(xtabs(~ E3 + school, data = df))

df <- subset(df_1_60, E2 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(E2, E3))
xtabs(~ E3 + E2, data = df)
chisq.test(xtabs(~ E3 + E2, data = df))

df <- subset(df_1_60, V12 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(V12, E3))
xtabs(~ E3 + V12, data = df)
chisq.test(xtabs(~ E3 + V12, data = df))

df <- subset(df_1_60, D7a != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(D7a, E3))
xtabs(~ E3 + D7a, data = df)
chisq.test(xtabs(~ E3 + D7a, data = df))

df <- subset(df_1_60, D10 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(D10, E3))
get_health <- function(df){
  df$health <- "0"
  df[df$D10 == 7, ]$health <- "YES"
  df[df$D10 != 7, ]$health <- "NO"
  df$health <- factor(df$health, levels = c("YES", "NO"))
  return(df)
}
df <- get_health(df)
xtabs(~ E3 + D10, data = df)
chisq.test(xtabs(~ E3 + D10, data = df))
xtabs(~ E3 + health, data = df)
chisq.test(xtabs(~ E3 + health, data = df))


df <- subset(df_1_60, B1_1 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_1, E3))
xtabs(~ E3 + B1_1, data = df)
chisq.test(xtabs(~ E3 + B1_1, data = df))

df <- subset(df_1_60, B1_2 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_2, E3))
xtabs(~ E3 + B1_2, data = df)
chisq.test(xtabs(~ E3 + B1_2, data = df))

df <- subset(df_1_60, B1_3 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_3, E3))
xtabs(~ E3 + B1_3, data = df)
chisq.test(xtabs(~ E3 + B1_3, data = df))

df <- subset(df_1_60, B1_4 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_4, E3))
xtabs(~ E3 + B1_4, data = df)
chisq.test(xtabs(~ E3 + B1_4, data = df))

df <- subset(df_1_60, B1_5 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_5, E3))
xtabs(~ E3 + B1_5, data = df)
chisq.test(xtabs(~ E3 + B1_5, data = df))

df <- subset(df_1_60, B1_6 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_6, E3))
xtabs(~ E3 + B1_6, data = df)
chisq.test(xtabs(~ E3 + B1_6, data = df))

df <- subset(df_1_60, B1_7 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_7, E3))
xtabs(~ E3 + B1_7, data = df)
chisq.test(xtabs(~ E3 + B1_7, data = df))

df <- subset(df_1_60, B1_8 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_8, E3))
xtabs(~ E3 + B1_8, data = df)
chisq.test(xtabs(~ E3 + B1_8, data = df))

df <- subset(df_1_60, B1_9 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_9, E3))
xtabs(~ E3 + B1_9, data = df)
chisq.test(xtabs(~ E3 + B1_9, data = df))

df <- subset(df_1_60, B1_10 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_10, E3))
xtabs(~ E3 + B1_10, data = df)
chisq.test(xtabs(~ E3 + B1_10, data = df))

df <- subset(df_1_60, B1_12 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(B1_12, E3))
xtabs(~ E3 + B1_12, data = df)
chisq.test(xtabs(~ E3 + B1_12, data = df))

c
xtabs(~ E3 + B1_13, data = df)
chisq.test(xtabs(~ E3 + B1_13, data = df))


df <- subset(df_1_60, B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
               B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
               B1_12 != "NA" & B1_13 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4,
             select = c(B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13, E3))
#Mild 0-2
#Moderate 3-5
#Severe 6-8
#Critical 9-12
get_symptom <- function(df){
  df$score <- rowSums(df[, colnames(df) %in% c("B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6",
                                               "B1_7", "B1_8", "B1_9", "B1_10", "B1_12", "B1_13")])
  df$have <- "0"
  df[df$score < 24, ]$have <- "YES"
  df[df$score == 24, ]$have <- "NO"
  df$have <- factor(df$have, levels = c("YES", "NO"))
  df$sym <- "0"
  df[df$score <= 24 & df$score >= 22, ]$sym <- "Mild"
  df[df$score < 22 & df$score >= 19, ]$sym <- "Moderate"
  df[df$score < 19 & df$score >= 16, ]$sym <- "Severe"
  df[df$score < 16, ]$sym <- "Critical"
  df$sym <- factor(df$sym, levels = c("Mild", "Moderate", "Severe", "Critical"))
  return(df)
}
df <- get_symptom(df)
xtabs(~ E3 + have, data = df)
chisq.test(xtabs(~ E3 + have, data = df))
xtabs(~ E3 + sym, data = df)
chisq.test(xtabs(~ E3 + sym, data = df))



df <- subset(df_1_60, C5 != "NA" & C5 != 6 & E3 != "NA" & E3 != 3 & E3 != 4, select = c(C5, E3))
xtabs(~ E3 + C5, data = df)
chisq.test(xtabs(~ E3 + C5, data = df))



#table 3
df <- subset(df_1_60, C14a != "NA" & G1 != "NA" & G2 != "NA" & G3 != "NA",
             select = c(G1, G2, G3, C14a))
get_mask <- function(df){
  df$G1 <- 4 - df$G1
  df$G2 <- 4 - df$G2
  df$G3 <- 4 - df$G3
  df$C14a <- 5 - df$C14a
  return(df)
}
df <- get_mask(df)
cronbach.alpha(df[1:3])   #0.709
cronbach.alpha(df[2:3])   #0.766
cronbach.alpha(df[-c(2, 4)])
cronbach.alpha(df[1:4])   #0.687
cronbach.alpha(df[-c(2, 3)])

df <- subset(df_1_60, I7_7 != "NA" & I7_8 != "NA" & V6_4 != "NA", select = c(I7_7, I7_8, V6_4))
cronbach.alpha(df[1:2])
cronbach.alpha(df[1:3])


df <- subset(df_1_60, G1 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(G1, E3))
df$G1 <- 4 - df$G1
xtabs(~ E3 + G1, data = df)
chisq.test(xtabs(~ E3 + G1, data = df))

df <- subset(df_1_60, G2 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(G2, E3))
df$G2 <- 4 - df$G2
xtabs(~ E3 + G2, data = df)
chisq.test(xtabs(~ E3 + G2, data = df))

df <- subset(df_1_60, G3 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(G3, E3))
df$G3 <- 4 - df$G3
xtabs(~ E3 + G3, data = df)
chisq.test(xtabs(~ E3 + G3, data = df))

df <- subset(df_1_60, C14a != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(C14a, E3))
df$C14a <- 5 - df$C14a
xtabs(~ E3 + C14a, data = df)
chisq.test(xtabs(~ E3 + C14a, data = df))

df <- subset(df_1_60, I7_7 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(I7_7, E3))
xtabs(~ E3 + I7_7, data = df)
chisq.test(xtabs(~ E3 + I7_7, data = df))

df <- subset(df_1_60, I7_8 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(I7_8, E3))
xtabs(~ E3 + I7_8, data = df)
chisq.test(xtabs(~ E3 + I7_8, data = df))

df <- subset(df_1_60, V6_4 != "NA" & E3 != "NA" & E3 != 3 & E3 != 4, select = c(V6_4, E3))
xtabs(~ E3 + V6_4, data = df)
chisq.test(xtabs(~ E3 + V6_4, data = df))





#table 4
df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E4 != "NA",
             select = c(D1, D2, D4, D5, E4))
df <- get_mental(df)
xtabs(~ scale + E4, data = df)
chisq.test(xtabs(~ scale + E4, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E8 != "NA",
             select = c(D1, D2, D4, D5, E8))
df <- get_mental(df)
df <- get_school(df)
xtabs(~ scale + school, data = df)
chisq.test(xtabs(~ scale + school, data = df))
xtabs(~ scale + E8, data = df)
chisq.test(xtabs(~ scale + E8, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & E2 != "NA",
             select = c(D1, D2, D4, D5, E2))
df <- get_mental(df)
xtabs(~ scale + E2, data = df)
chisq.test(xtabs(~ scale + E2, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & V12 != "NA",
             select = c(D1, D2, D4, D5, V12))
df <- get_mental(df)
xtabs(~ scale + V12, data = df)
chisq.test(xtabs(~ scale + V12, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & D7a != "NA",
             select = c(D1, D2, D4, D5, D7a))
df <- get_mental(df)
xtabs(~ scale + D7a, data = df)
chisq.test(xtabs(~ scale + D7a, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & D10 != "NA",
             select = c(D1, D2, D4, D5, D10))
df <- get_mental(df)
xtabs(~ scale + D10, data = df)
chisq.test(xtabs(~ scale + D10, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & D10 != "NA",
             select = c(D1, D2, D4, D5, D10))
df <- get_mental(df)
df <- get_health(df)
xtabs(~ scale + health, data = df)
chisq.test(xtabs(~ scale + health, data = df))


df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & C14a != "NA",
             select = c(D1, D2, D4, D5, C14a))
df <- get_mental(df)
df$C14a <- 5 - df$C14a
xtabs(~ scale + C14a, data = df)
chisq.test(xtabs(~ scale + C14a, data = df))


df <- subset(data_21_07, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & V6_4 != "NA",
             select = c(D1, D2, D4, D5, V6_4))
df <- get_mental(df)
xtabs(~ scale + V6_4, data = df)
chisq.test(xtabs(~ scale + V6_4, data = df))



df <- subset(df_1_60, B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
               B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
               B1_12 != "NA" & B1_13 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13, D1, D2, D4, D5))
df <- get_symptom(df)
df <- get_mental(df)
xtabs(~ scale + have, data = df)
chisq.test(xtabs(~ scale + have, data = df))
xtabs(~ scale + sym, data = df)
chisq.test(xtabs(~ scale + sym, data = df))





df <- subset(df_1_60, B1_1 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_1, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_1, data = df)
chisq.test(xtabs(~ scale + B1_1, data = df))


df <- subset(df_1_60, B1_2 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_2, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_2, data = df)
chisq.test(xtabs(~ scale + B1_2, data = df))


df <- subset(df_1_60, B1_3 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_3, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_3, data = df)
chisq.test(xtabs(~ scale + B1_3, data = df))


df <- subset(df_1_60, B1_4 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_4, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_4, data = df)
chisq.test(xtabs(~ scale + B1_4, data = df))


df <- subset(df_1_60, B1_5 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_5, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_5, data = df)
chisq.test(xtabs(~ scale + B1_5, data = df))


df <- subset(df_1_60, B1_6 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_6, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_6, data = df)
chisq.test(xtabs(~ scale + B1_6, data = df))


df <- subset(df_1_60, B1_7 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_7, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_7, data = df)
chisq.test(xtabs(~ scale + B1_7, data = df))


df <- subset(df_1_60, B1_8 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_8, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_8, data = df)
chisq.test(xtabs(~ scale + B1_8, data = df))


df <- subset(df_1_60, B1_9 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_9, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_9, data = df)
chisq.test(xtabs(~ scale + B1_9, data = df))


df <- subset(df_1_60, B1_10 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_10, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_10, data = df)
chisq.test(xtabs(~ scale + B1_10, data = df))


df <- subset(df_1_60, B1_12 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_12, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_12, data = df)
chisq.test(xtabs(~ scale + B1_12, data = df))


df <- subset(df_1_60, B1_13 != "NA" & D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA",
             select = c(B1_13, D1, D2, D4, D5))
df <- get_mental(df)
xtabs(~ scale + B1_13, data = df)
chisq.test(xtabs(~ scale + B1_13, data = df))




df <- subset(df_1_60, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA" & C5 != "NA" & C5 != 6,
             select = c(D1, D2, D4, D5, C5))
df <- get_mental(df)
xtabs(~ scale + C5, data = df)
chisq.test(xtabs(~ scale + C5, data = df))




#mask use plot
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


df <- subset(logis_all, select = c(C5, E3))
xtabs(~ C5 + E3, data = df)
chisq.test(xtabs(~ C5 + E3, data = df))


df <- subset(logis_all, select = c(C5, E4))
xtabs(~ C5 + E4, data = df)
chisq.test(xtabs(~ C5 + E4, data = df))


df <- subset(logis_all, select = c(C5, E8))
xtabs(~ C5 + E8, data = df)
chisq.test(xtabs(~ C5 + E8, data = df))

df <- subset(logis_all, select = c(C5, school))
xtabs(~ C5 + school, data = df)
chisq.test(xtabs(~ C5 + school, data = df))


df <- subset(logis_all, select = c(C5, E2))
xtabs(~ C5 + E2, data = df)
chisq.test(xtabs(~ C5 + E2, data = df))


df <- subset(logis_all, select = c(C5, V12))
xtabs(~ C5 + V12, data = df)
chisq.test(xtabs(~ C5 + V12, data = df))


df <- subset(logis_all, select = c(C5, D7a))
xtabs(~ C5 + D7a, data = df)
chisq.test(xtabs(~ C5 + D7a, data = df))


df <- subset(logis_all, select = c(C5, sym))
xtabs(~ C5 + sym, data = df)
chisq.test(xtabs(~ C5 + sym, data = df))


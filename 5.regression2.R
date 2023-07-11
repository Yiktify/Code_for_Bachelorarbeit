library(cluster)
library(ggpubr)
library(factoextra)
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

#plot
df_1_360$time <- as.Date(df_1_360$time)
Sys.setlocale("LC_TIME", "US")

yrng <- range(df_1_360$PHQ_mean)
xrng <- range(df_1_360$time)

ggplot(df_1_360, aes(time, PHQ_mean)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  labs(x = NULL, y = "mean value of PHQ") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  annotate(geom = "text", x = xrng[1], y = yrng[2], label = "2021-12-17", hjust = -2.17, vjust = 27.6, size = 4)

#without 2021-21-17
ggplot(subset(df_1_360, time != "2021-12-17"), aes(time, PHQ_mean)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  ylim(3.8, 4.6) +
  labs(x = NULL, y = "mean value of PHQ") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(subset(df_1_360, time != "2021-12-17"), aes(time, wear_mask_score)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  ylim(2, 3.5) +
  labs(x = NULL, y = "mean value of mask wearing score") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ggplot(subset(df_1_360, time != "2021-12-17"), aes(wear_mask_score, PHQ_mean)) +
  geom_point() +
  ylim(3.993, 4.58) +
  xlim(2, 3.5) +
  labs(x = "mean value of mask wearing score", y = "mean value of PHQ") +
  stat_smooth(method = "lm", se = T, color = "#C65C5C")



#cluster analysis
par(mfrow = c(1,1))
df_1_360 <- read.csv("df_1_360.csv")
df_1_360 <- subset(df_1_360, time != "2021-12-17" & time != "2022-01-02" & time != "2022-01-03")
data_cl <- scale(df_1_360[-c(1)])
wss <- (nrow(data_cl) - 1) * sum(apply(data_cl, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data_cl, centers = i)$withinss) 
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
fit3 <- kmeans(data_cl, 2)
res <- data.frame(data_cl, fit3$cluster)
clusplot(data_cl, fit3$cluster, color = T, lines = 0, labels = 4,
         col.p =  ifelse(res$fit3.cluster == 2, "steelblue","darkred"))

fviz_cluster(fit3, data_cl,
             palette = c("steelblue","indianred"),
             geom = "point",
             ellipse.type = "convex")


par(mfrow = c(1,1))
df_1_360 <- read.csv("df_1_360.csv")
df_1_360 <- subset(df_1_360, time != "2021-12-17")
data_cl <- scale(df_1_360[-c(1)])
wss <- (nrow(data_cl) - 1) * sum(apply(data_cl, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data_cl, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
fit3 <- kmeans(data_cl, 2)
res <- data.frame(data_cl, fit3$cluster)
clusplot(data_cl, fit3$cluster, color = T, lines = 0, labels = 4,
         col.p =  ifelse(res$fit3.cluster == 2, "steelblue","darkred"))

fviz_cluster(fit3, data_cl,
             palette = c("steelblue","indianred"),
             geom = "point",
             ellipse.type = "convex")


#linear regression
##
df_1_360 <- cbind(df_1_360, res[35])
df_1_360$time <- as.Date(df_1_360$time)


###
df_1_360$fit3.cluster[df_1_360$fit3.cluster == 2] <- "cluster 1"
df_1_360$fit3.cluster[df_1_360$fit3.cluster == 1] <- "cluster 2"
df_1_360$fit3.cluster <- as.factor(df_1_360$fit3.cluster)

ggplot(df_1_360, aes(time, PHQ_mean, col = fit3.cluster)) + 
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  scale_color_manual(values = c("cluster 1" = "steelblue", "cluster 2" = "#C65C5C")) +
  labs(x = NULL, y = "mean value of PHQ") +
  ylim(3.8, 4.6) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = c(0.8, 0.07),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))

#legend.key.size = unit(15, "pt")

ggplot(df_1_360, aes(time, wear_mask_score, col = fit3.cluster)) + 
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  scale_color_manual(values = c("cluster 1" = "steelblue", "cluster 2" = "#C65C5C")) +
  labs(x = NULL, y = "mean value of mask wear score") +
  ylim(2, 3.5) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = c(0.8, 0.07),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, alpha = 0), colour = NA))
###


ggplot(subset(df_1_360, fit3.cluster == 2), aes(wear_mask_score, PHQ_mean)) +
  geom_point() +
  ylim(3.993, 4.4) +
  xlim(3.09, 3.33) +
  labs(x = "mean value of mask wearing score (cluster 1)", y = "mean value of PHQ (cluster 1)") +
  stat_smooth(method = "lm", se = T, color = "#C65C5C")

#annotate("text", x = 3.121, y = 4.37, label = "Cluster 1", size = 5)


ggplot(subset(df_1_360, fit3.cluster == 1), aes(wear_mask_score, PHQ_mean)) +
  geom_point() +
  ylim(3.993, 4.6) +
  xlim(2.05, 3.45) +
  labs(x = "mean value of mask wearing score (cluster 2)", y = "mean value of PHQ (cluster 2)") +
  stat_smooth(method = "lm", se = T, color = "#C65C5C")




#cluster 1
df_1_360_1 <- subset(df_1_360, fit3.cluster == 2)

model <- lm(PHQ_mean ~ wear_mask_score + gender_male.female +
              age_young.old + edu_nonuni.uni + live_city.town_or_village + smoke_yes.no +
              work_yes.no + work_for_health_yes.no,
            data = df_1_360_1)
summary(model)
car::vif(model)
par(mfrow = c(2,2)) 
plot(model)
par(mfrow = c(1,1))
confint(model, level = 0.95)


model <- lm(PHQ_mean ~ wear_mask_score + gender_male.female +
              live_city.town_or_village +
              work_yes.no + work_for_health_yes.no,
            data = df_1_360_1)
summary(model)
car::vif(model)
caret::varImp(model)
confint(model, level = 0.95)


#cluster 2
df_1_360_2 <- subset(df_1_360, fit3.cluster == 1)
df_1_360_2 <- subset(df_1_360_2, time != "2022-01-02" & time != "2022-01-03")


model <- lm(PHQ_mean ~ wear_mask_score + gender_male.female +
              age_young.old + edu_nonuni.uni + live_city.town_or_village + smoke_yes.no +
              work_yes.no + work_for_health_yes.no,
            data = df_1_360_2)
summary(model)
car::vif(model)
confint(model, level = 0.95)


model <- lm(PHQ_mean ~ wear_mask_score + age_young.old +
              live_city.town_or_village +
              work_yes.no,
            data = df_1_360_2)
summary(model)
car::vif(model)
par(mfrow = c(2,2)) 
plot(model)
par(mfrow = c(1,1))
caret::varImp(model)
confint(model, level = 0.95)




##cluster 2 with J3
df_180_360_day_withJ3 <- read.csv("df_180_360_day_withJ3.csv")
df_180_360_day_withJ3$time <- as.Date(df_180_360_day_withJ3$time)


ggplot(df_180_360_day_withJ3, aes(time, PHQ_mean)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  ylim(4.0, 4.6) +
  labs(x = NULL, y = "mean value of PHQ") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ggplot(df_180_360_day_withJ3, aes(time, have_childer_yes.no)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  ylim(0.45, 0.6) +
  labs(x = NULL, y = "household with childern under 18 ratio") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ggplot(df_180_360_day_withJ3, aes(have_childer_yes.no, PHQ_mean)) +
  geom_point() +
  ylim(4.0, 4.6) +
  xlim(0.45, 0.61) +
  labs(x = "household with childern under 18 ratio", y = "mean value of PHQ") +
  stat_smooth(method = "lm", se = T, color = "#C65C5C")


model <- lm(PHQ_mean ~ wear_mask_score + gender_male.female +
              age_young.old + edu_nonuni.uni + live_city.town_or_village + smoke_yes.no +
              work_yes.no + work_for_health_yes.no + have_childer_yes.no,
            data = df_180_360_day_withJ3)
summary(model)
car::vif(model)
confint(model, level = 0.95)


model <- lm(PHQ_mean ~ wear_mask_score + gender_male.female +
              age_young.old  + live_city.town_or_village  +
              work_yes.no + have_childer_yes.no,
            data = df_180_360_day_withJ3)
summary(model)
car::vif(model)
caret::varImp(model)
confint(model, level = 0.95)
par(mfrow = c(2,2)) 
plot(model)
par(mfrow = c(1,1))



model <- lm(PHQ_mean ~ wear_mask_score + I(wear_mask_score^2) + gender_male.female +
              age_young.old  + live_city.town_or_village  +
              work_yes.no + have_childer_yes.no,
            data = df_180_360_day_withJ3)
summary(model)
car::vif(model)
caret::varImp(model)
confint(model, level = 0.95)
par(mfrow = c(2,2)) 
plot(model)
par(mfrow = c(1,1))



linear <- function(x){
  -0.501+2.593*x-0.461*x^2
}

ggplot(data.frame(x = c(0, 4)), aes(x)) +
  stat_function(fun = linear) +
  labs(x = "mask wear score", y = "PHQ score") +
  geom_vline(xintercept = 2.812364, linetype = "dashed")
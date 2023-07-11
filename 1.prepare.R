library(readr)
library(tidyverse)
library(data.table)

#all dataset
temp <- list.files(path = "all_dataset", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

names1 <- c("A2_2_1", "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7", "B1_8", "B1_9", "B1_10", "B1_12",
            "B1_13", "B7", "B8a", "V1", "V6_4", "E3", "E4", "E8", "E2", "C14a", "C5", "C0a_1", "C0a_2", "C0a_3", "C0a_4",
            "C0a_5", "C0a_6", "C0a_7", "C13a_1", "C13a_2", "C13a_3", "C13a_4", "C13a_5", "C13a_6", "C13a_7", "C13a_8",
            "G1", "G2", "G3", "H1", "H2", "I7_7", "I7_8", "D1", "D2", "D4", "D5", "V11", "V12", "D7a", "D10")

names2 <- c("A2_2_1", "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7", "B1_8", "B1_9", "B1_10", "B1_12",
            "B1_13", "B7", "B8a", "V1", "V6_4", "E3", "E4", "E8", "E2", "C14a", "C5", "C0a_1", "C0a_2", "C0a_3", "C0a_4",
            "C0a_5", "C0a_6", "C0a_7", "C13a_1", "C13a_2", "C13a_3", "C13a_4", "C13a_5", "C13a_6", "C13a_7", "C13a_8",
            "G1", "G2", "G3", "H1", "H2", "I7_7", "I7_8", "D1", "D2", "D4", "D5", "V11", "V12", "J3", "D7a", "D10")

get_6days_df_1 <- function(i){
  df1 <- read_csv(temp[6 * i - 5], id = "file_name", col_select = all_of(names1))
  df2 <- read_csv(temp[6 * i - 4], id = "file_name", col_select = all_of(names1))
  df3 <- read_csv(temp[6 * i - 3], id = "file_name", col_select = all_of(names1))
  df4 <- read_csv(temp[6 * i - 2], id = "file_name", col_select = all_of(names1))
  df5 <- read_csv(temp[6 * i - 1], id = "file_name", col_select = all_of(names1))
  df6 <- read_csv(temp[6 * i], id = "file_name", col_select = all_of(names1))
  df <- bind_rows(df1, df2, df3, df4, df5, df6)
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  return(df)
}

get_6days_df_2 <- function(i){
  df1 <- read_csv(temp[6 * i - 5], id = "file_name", col_select = all_of(names2))
  df2 <- read_csv(temp[6 * i - 4], id = "file_name", col_select = all_of(names2))
  df3 <- read_csv(temp[6 * i - 3], id = "file_name", col_select = all_of(names2))
  df4 <- read_csv(temp[6 * i - 2], id = "file_name", col_select = all_of(names2))
  df5 <- read_csv(temp[6 * i - 1], id = "file_name", col_select = all_of(names2))
  df6 <- read_csv(temp[6 * i], id = "file_name", col_select = all_of(names2))
  df <- bind_rows(df1, df2, df3, df4, df5, df6)
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  return(df)
}

df_01 <- get_6days_df_1(1)
df_02 <- get_6days_df_1(2)
df_03 <- get_6days_df_1(3)
df_04 <- get_6days_df_1(4)
df_05 <- get_6days_df_1(5)
df_06 <- get_6days_df_1(6)
df_07 <- get_6days_df_1(7)
df_08 <- get_6days_df_1(8)
df_09 <- get_6days_df_1(9)

df_10 <- get_6days_df_1(10)
df_11 <- get_6days_df_1(11)
df_12 <- get_6days_df_1(12)
df_13 <- get_6days_df_1(13)
df_14 <- get_6days_df_1(14)
df_15 <- get_6days_df_1(15)
df_16 <- get_6days_df_1(16)
df_17 <- get_6days_df_1(17)
df_18 <- get_6days_df_1(18)
df_19 <- get_6days_df_1(19)

df_20 <- get_6days_df_1(20)
df_21 <- get_6days_df_1(21)
df_22 <- get_6days_df_1(22)
df_23 <- get_6days_df_1(23)
df_24 <- get_6days_df_1(24)
df_25 <- get_6days_df_1(25)
df_26 <- get_6days_df_1(26)
df_27 <- get_6days_df_1(27)
df_28 <- get_6days_df_1(28)
df_29 <- get_6days_df_1(29)

df_30 <- get_6days_df_2(30)
df_31 <- get_6days_df_2(31)
df_32 <- get_6days_df_2(32)
df_33 <- get_6days_df_2(33)
df_34 <- get_6days_df_2(34)
df_35 <- get_6days_df_2(35)
df_36 <- get_6days_df_2(36)
df_37 <- get_6days_df_2(37)
df_38 <- get_6days_df_2(38)
df_39 <- get_6days_df_2(39)

df_40 <- get_6days_df_2(40)
df_41 <- get_6days_df_2(41)
df_42 <- get_6days_df_2(42)
df_43 <- get_6days_df_2(43)
df_44 <- get_6days_df_2(44)
df_45 <- get_6days_df_2(45)
df_46 <- get_6days_df_2(46)
df_47 <- get_6days_df_2(47)
df_48 <- get_6days_df_2(48)
df_49 <- get_6days_df_2(49)

df_50 <- get_6days_df_2(50)
df_51 <- get_6days_df_2(51)
df_52 <- get_6days_df_2(52)
df_53 <- get_6days_df_2(53)
df_54 <- get_6days_df_2(54)
df_55 <- get_6days_df_2(55)
df_56 <- get_6days_df_2(56)
df_57 <- get_6days_df_2(57)
df_58 <- get_6days_df_2(58)
df_59 <- get_6days_df_2(59)
df_60 <- get_6days_df_2(60)

df_1_29 <- rbind(df_01, df_02, df_03, df_04, df_05, df_06, df_07, df_08, df_09, df_10,
                 df_11, df_12, df_13, df_14, df_15, df_16, df_17, df_18, df_19, df_20,
                 df_21, df_22, df_23, df_24, df_25, df_26, df_27, df_28, df_29)

df_30_60 <- rbind(df_30, df_31, df_32, df_33, df_34, df_35, df_36, df_37, df_38, df_39,
                  df_40, df_41, df_42, df_43, df_44, df_45, df_46, df_47, df_48, df_49,
                  df_50, df_51, df_52, df_53, df_54, df_55, df_56, df_57, df_58, df_59, df_60)

df_1_60 <- rbind(df_1_29, df_30_60[-53])



#daily dataset

temp <- list.files(path = "all_dataset", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

names1 <- c("A2_2_1", "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7", "B1_8", "B1_9", "B1_10", "B1_12",
            "B1_13", "B7", "B8a", "V1", "V6_4", "E3", "E4", "E8", "E2", "C14a", "C5", "C0a_1", "C0a_2", "C0a_3", "C0a_4",
            "C0a_5", "C0a_6", "C0a_7", "C13a_1", "C13a_2", "C13a_3", "C13a_4", "C13a_5", "C13a_6", "C13a_7", "C13a_8",
            "G1", "G2", "G3", "H1", "H2", "I7_7", "I7_8", "D1", "D2", "D4", "D5", "V11", "V12", "D7a", "D10")

names2 <- c("A2_2_1", "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7", "B1_8", "B1_9", "B1_10", "B1_12",
            "B1_13", "B7", "B8a", "V1", "V6_4", "E3", "E4", "E8", "E2", "C14a", "C5", "C0a_1", "C0a_2", "C0a_3", "C0a_4",
            "C0a_5", "C0a_6", "C0a_7", "C13a_1", "C13a_2", "C13a_3", "C13a_4", "C13a_5", "C13a_6", "C13a_7", "C13a_8",
            "G1", "G2", "G3", "H1", "H2", "I7_7", "I7_8", "D1", "D2", "D4", "D5", "V11", "V12", "J3", "D7a", "D10")

get_info_2 <- function(data){
  x <- paste(substr(unique(data$file_name)[1], 13, 22))
  df <- get_mental(subset(data, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA", select = c(D1, D2, D4, D5)))
  a <- mean(df$PHQ)
  b <- table(df$scale)[[2]] / sum(table(df$scale))
  df <- subset(data, B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                 B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                 B1_12 != "NA" & B1_13 != "NA",
               select = c(B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
  df$sym <- rowSums(df[, c(1:12)])
  c0 <- table(df$sym)[[13]] / sum(table(df$sym))
  c1 <- table(df$B1_1)[[1]] / sum(table(df$B1_1))
  c2 <- table(df$B1_2)[[1]] / sum(table(df$B1_2))
  c3 <- table(df$B1_3)[[1]] / sum(table(df$B1_3))
  c4 <- table(df$B1_4)[[1]] / sum(table(df$B1_4))
  c5 <- table(df$B1_5)[[1]] / sum(table(df$B1_5))
  c6 <- table(df$B1_6)[[1]] / sum(table(df$B1_6))
  c7 <- table(df$B1_7)[[1]] / sum(table(df$B1_7))
  c8 <- table(df$B1_8)[[1]] / sum(table(df$B1_8))
  c9 <- table(df$B1_9)[[1]] / sum(table(df$B1_9))
  c10 <- table(df$B1_10)[[1]] / sum(table(df$B1_10))
  c12 <- table(df$B1_12)[[1]] / sum(table(df$B1_12))
  c13 <- table(df$B1_13)[[1]] / sum(table(df$B1_13))
  df <- subset(data, E3 != "NA" & E3 != 3 & E3 != 4, select = E3)
  d <- table(df$E3)[[1]] / table(df$E3)[[2]]
  df <- subset(data, E4 != "NA", select = E4)
  e <- (sum(table(df$E4)) - table(df$E4)[[6]] - table(df$E4)[[7]]) / (table(df$E4)[[6]] + table(df$E4)[[7]])
  df <- subset(data, E8 != "NA", select = E8)
  f <- (sum(table(df$E8)) - table(df$E8)[[6]] - table(df$E8)[[7]]) / (table(df$E8)[[6]] + table(df$E8)[[7]])
  df <- subset(data, E2 != "NA", select = E2)
  g <- table(df$E2)[[1]] / (table(df$E2)[[2]] + table(df$E2)[[3]])
  df <- subset(data, V12 != "NA", select = V12)
  h <- table(df$V12)[[1]] / table(df$V12)[[2]]
  df <- subset(data, D7a != "NA", select = D7a)
  i <- table(df$D7a)[[1]] / table(df$D7a)[[2]]
  df <- subset(data, D10 != "NA", select = D10)
  j <- table(df$D10)[[7]] / (sum(table(df$D10)) - table(df$D10)[[7]])
  df <- subset(data, C5 != "NA" & C5 != 6, select = C5)
  df$C5 <- 5 - df$C5
  k <- mean(df$C5)
  df <- subset(data, H1 != "NA" & H1 != 6, select = H1)
  df$H1 <- df$H1 - 1
  l <- mean(df$H1)
  df <- subset(data, H2 != "NA" & H2 != 6, select = H2)
  df$H2 <- df$H2 - 1
  m <- mean(df$H2)
  df <- subset(data, B7 != "NA", select = B7)
  n <- table(df$B7)[[1]] / table(df$B7)[[2]]
  df <- subset(data, B8a != "NA" & B8a != 3, select = B8a)
  o <- table(df$B8a)[[1]] / table(df$B8a)[[2]]
  df <- subset(data, V1 != "NA" & V1 != 3, select = V1)
  p <- table(df$V1)[[1]] / table(df$V1)[[2]]
  df <- subset(data, C14a != "NA", select = C14a)
  df$C14a <- 5 - df$C14a
  q <- mean(df$C14a)
  df <- subset(data, G1 != "NA", select = G1)
  df$G1 <- 4 - df$G1
  r <- mean(df$G1)
  df <- subset(data, G2 != "NA", select = G2)
  df$G2 <- 4 - df$G2
  s <- mean(df$G2)
  df <- subset(data, G3 != "NA", select = G3)
  df$G3 <- 4 - df$G3
  t <- mean(df$G3)
  df <- subset(data, I7_7 != "NA", select = I7_7)
  u <- table(df$I7_7)[[2]] / table(df$I7_7)[[1]]
  df <- subset(data, I7_8 != "NA", select = I7_8)
  v <- table(df$I7_8)[[2]] / table(df$I7_8)[[1]]
  result <- data.frame(time = x, PHQ_mean = a, depression_percen = b, symptoms_percen = c0, fever = c1,
                       cough = c2, difficulty_breathing = c3, fatigue = c4, stuffy_or_runny_nose = c5,
                       aches_or_muscle_pain = c6, sore_throat = c7, chest_pain = c8, nausea = c9,
                       loss_of_smell_or_taste = c10, headache = c12, chills = c13, "gender_male/female" = d,
                       "age_young/old" = e, "edu_nonuni/uni" = f, "live_city/town_or_village" = g,
                       "smoke_yes/no" = h, "work_yes/no" = i, "work_for_health_yes/no" = j, wear_mask_score = k,
                       other_keep_distance_score = l, other_wear_mask_score = m, "test_for_COVID_yes/no" = n,
                       "test_positive_yes/no" = o, "have_vaccine_yes/no" = p, intentionally_avoid_other_score = q,
                       worry_about_COVID_score = r, believe_social_distancing_score = s,
                       believe_wearing_mask_score = t, "want_info_to_mantain_mental_health_yes/no" = u,
                       "want_info_to_conquer_physical_distancing_yes/no" = v)
  return(result)
}


get_info_3 <- function(data){
  x <- paste(substr(unique(data$file_name)[1], 13, 22))
  df <- get_mental(subset(data, D1 != "NA" & D2 != "NA" & D4 != "NA" & D5 != "NA", select = c(D1, D2, D4, D5)))
  a <- mean(df$PHQ)
  b <- table(df$scale)[[2]] / sum(table(df$scale))
  df <- subset(data, B1_1 != "NA" & B1_2 != "NA" & B1_3 != "NA" & B1_4 != "NA" & B1_5 != "NA" &
                 B1_6 != "NA" & B1_7 != "NA" & B1_8 != "NA" & B1_9 != "NA" & B1_10 != "NA" &
                 B1_12 != "NA" & B1_13 != "NA",
               select = c(B1_1, B1_2, B1_3, B1_4, B1_5, B1_6, B1_7, B1_8, B1_9, B1_10, B1_12, B1_13))
  df$sym <- rowSums(df[, c(1:12)])
  c0 <- table(df$sym)[[13]] / sum(table(df$sym))
  c1 <- table(df$B1_1)[[1]] / sum(table(df$B1_1))
  c2 <- table(df$B1_2)[[1]] / sum(table(df$B1_2))
  c3 <- table(df$B1_3)[[1]] / sum(table(df$B1_3))
  c4 <- table(df$B1_4)[[1]] / sum(table(df$B1_4))
  c5 <- table(df$B1_5)[[1]] / sum(table(df$B1_5))
  c6 <- table(df$B1_6)[[1]] / sum(table(df$B1_6))
  c7 <- table(df$B1_7)[[1]] / sum(table(df$B1_7))
  c8 <- table(df$B1_8)[[1]] / sum(table(df$B1_8))
  c9 <- table(df$B1_9)[[1]] / sum(table(df$B1_9))
  c10 <- table(df$B1_10)[[1]] / sum(table(df$B1_10))
  c12 <- table(df$B1_12)[[1]] / sum(table(df$B1_12))
  c13 <- table(df$B1_13)[[1]] / sum(table(df$B1_13))
  df <- subset(data, E3 != "NA" & E3 != 3 & E3 != 4, select = E3)
  d <- table(df$E3)[[1]] / table(df$E3)[[2]]
  df <- subset(data, E4 != "NA", select = E4)
  e <- (sum(table(df$E4)) - table(df$E4)[[6]] - table(df$E4)[[7]]) / (table(df$E4)[[6]] + table(df$E4)[[7]])
  df <- subset(data, E8 != "NA", select = E8)
  f <- (sum(table(df$E8)) - table(df$E8)[[6]] - table(df$E8)[[7]]) / (table(df$E8)[[6]] + table(df$E8)[[7]])
  df <- subset(data, E2 != "NA", select = E2)
  g <- table(df$E2)[[1]] / (table(df$E2)[[2]] + table(df$E2)[[3]])
  df <- subset(data, V12 != "NA", select = V12)
  h <- table(df$V12)[[1]] / table(df$V12)[[2]]
  df <- subset(data, D7a != "NA", select = D7a)
  i <- table(df$D7a)[[1]] / table(df$D7a)[[2]]
  df <- subset(data, D10 != "NA", select = D10)
  j <- table(df$D10)[[7]] / (sum(table(df$D10)) - table(df$D10)[[7]])
  df <- subset(data, C5 != "NA" & C5 != 6, select = C5)
  df$C5 <- 5 - df$C5
  k <- mean(df$C5)
  df <- subset(data, H1 != "NA" & H1 != 6, select = H1)
  df$H1 <- df$H1 - 1
  l <- mean(df$H1)
  df <- subset(data, H2 != "NA" & H2 != 6, select = H2)
  df$H2 <- df$H2 - 1
  m <- mean(df$H2)
  df <- subset(data, B7 != "NA", select = B7)
  n <- table(df$B7)[[1]] / table(df$B7)[[2]]
  df <- subset(data, B8a != "NA" & B8a != 3, select = B8a)
  o <- table(df$B8a)[[1]] / table(df$B8a)[[2]]
  df <- subset(data, V1 != "NA" & V1 != 3, select = V1)
  p <- table(df$V1)[[1]] / table(df$V1)[[2]]
  df <- subset(data, C14a != "NA", select = C14a)
  df$C14a <- 5 - df$C14a
  q <- mean(df$C14a)
  df <- subset(data, G1 != "NA", select = G1)
  df$G1 <- 4 - df$G1
  r <- mean(df$G1)
  df <- subset(data, G2 != "NA", select = G2)
  df$G2 <- 4 - df$G2
  s <- mean(df$G2)
  df <- subset(data, G3 != "NA", select = G3)
  df$G3 <- 4 - df$G3
  t <- mean(df$G3)
  df <- subset(data, I7_7 != "NA", select = I7_7)
  u <- table(df$I7_7)[[2]] / table(df$I7_7)[[1]]
  df <- subset(data, I7_8 != "NA", select = I7_8)
  v <- table(df$I7_8)[[2]] / table(df$I7_8)[[1]]
  df <- subset(data, J3 != "NA", select = J3)
  w <- table(df$J3)[[1]] / table(df$J3)[[2]]
  result <- data.frame(time = x, PHQ_mean = a, depression_percen = b, symptoms_percen = c0, fever = c1,
                       cough = c2, difficulty_breathing = c3, fatigue = c4, stuffy_or_runny_nose = c5,
                       aches_or_muscle_pain = c6, sore_throat = c7, chest_pain = c8, nausea = c9,
                       loss_of_smell_or_taste = c10, headache = c12, chills = c13, "gender_male/female" = d,
                       "age_young/old" = e, "edu_nonuni/uni" = f, "live_city/town_or_village" = g,
                       "smoke_yes/no" = h, "work_yes/no" = i, "work_for_health_yes/no" = j, wear_mask_score = k,
                       other_keep_distance_score = l, other_wear_mask_score = m, "test_for_COVID_yes/no" = n,
                       "test_positive_yes/no" = o, "have_vaccine_yes/no" = p, intentionally_avoid_other_score = q,
                       worry_about_COVID_score = r, believe_social_distancing_score = s,
                       believe_wearing_mask_score = t, "want_info_to_mantain_mental_health_yes/no" = u,
                       "want_info_to_conquer_physical_distancing_yes/no" = v, "have_childer_yes/no" = w)
  return(result)
}






##without J3

df_1_30_day <- data.frame()
for (i in 1:30) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_1_30_day <- rbind(df_1_30_day, df_info)
}

df_31_60_day <- data.frame()
for (i in 31:60) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_31_60_day <- rbind(df_31_60_day, df_info)
}

df_61_90_day <- data.frame()
for (i in 61:90) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_61_90_day <- rbind(df_61_90_day, df_info)
}

df_91_120_day <- data.frame()
for (i in 91:120) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_91_120_day <- rbind(df_91_120_day, df_info)
}

df_121_150_day <- data.frame()
for (i in 121:150) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_121_150_day <- rbind(df_121_150_day, df_info)
}

df_151_180_day <- data.frame()
for (i in 151:180) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_151_180_day <- rbind(df_151_180_day, df_info)
}

df_181_210_day <- data.frame()
for (i in 181:210) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_181_210_day <- rbind(df_181_210_day, df_info)
}

df_211_240_day <- data.frame()
for (i in 211:240) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_211_240_day <- rbind(df_211_240_day, df_info)
}

df_241_270_day <- data.frame()
for (i in 241:270) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_241_270_day <- rbind(df_241_270_day, df_info)
}

df_271_300_day <- data.frame()
for (i in 271:300) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_271_300_day <- rbind(df_271_300_day, df_info)
}

df_301_330_day <- data.frame()
for (i in 301:330) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_301_330_day <- rbind(df_301_330_day, df_info)
}

df_331_360_day <- data.frame()
for (i in 331:360) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names1))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_2(df)
  df_331_360_day <- rbind(df_331_360_day, df_info)
}

df_1_360 <- rbind(df_1_30_day, df_31_60_day, df_61_90_day, df_91_120_day, df_121_150_day,
                  df_151_180_day, df_181_210_day, df_211_240_day, df_241_270_day,
                  df_271_300_day, df_301_330_day, df_331_360_day)
fwrite(df_1_360, "df_1_360.csv")


df_180_360_day_withJ3 <- data.frame()
for (i in 180:360) {
  df <- read_csv(temp[i], id = "file_name", col_select = all_of(names2))
  df[df == -77] <- NA
  df[df == -88] <- NA
  df[df == -99] <- NA
  df_info <- get_info_3(df)
  df_180_360_day_withJ3 <- rbind(df_180_360_day_withJ3, df_info)
}
fwrite(df_180_360_day_withJ3, "df_180_360_day_withJ3.csv")


df_1_360 <- read.csv("df_1_360.csv")
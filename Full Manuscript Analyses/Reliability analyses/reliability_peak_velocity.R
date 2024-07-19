
library(tidyverse)
library(irr)
library(afex)
library(emmeans)
library(stats)
library(reshape2)
library(pastecs)
library(MOTE)
library(janitor)

# Reliability of the load-velocity profile was calculated using intraclass correlation coefficients (ICCs) 
# and the coefficient of variation (CV) with 95% confidence intervals 

# peak velocity data ---------------------------

# Load rep_data

rep_data <- read_csv("data.csv") %>%
  clean_names()
head(rep_data) 

# 45% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_45 <- rep_data %>%
  filter(load == "45") %>% # 45% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_45 <- peak_data_45 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_45 <- peak_data_45 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_45 <- rep_1_45 %>%
  select(-c(time)) %>%
  full_join(rep_2_45) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_45 <- data.frame(
  mean_45_t1 = mean(reliability_peak_45$rep_1, na.rm = TRUE),
  sd_45_t1 = sd(reliability_peak_45$rep_1, na.rm = TRUE),
  mean_45_t2 = mean(reliability_peak_45$rep_2, na.rm = TRUE),
  sd_45_t2 = sd(reliability_peak_45$rep_2, na.rm = TRUE))
descriptives_45

## ICC -------

icc_result_45 <- icc(reliability_peak_45, model = "twoway", type = "consistency", unit = "single")

print(icc_result_45)

## CV  --------
# (SD/mean)*100

cv_45 <- reliability_peak_45 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by mean difference of trials 1 and 2 before the intervention

# 55% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_55 <- rep_data %>%
  filter(load == "55") %>% # 55% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_55 <- peak_data_55 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_55 <- peak_data_55 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_55 <- rep_1_55 %>%
  select(-c(time)) %>%
  full_join(rep_2_55) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_55 <- data.frame(
  mean_55_t1 = mean(reliability_peak_55$rep_1, na.rm = TRUE),
  sd_55_t1 = sd(reliability_peak_55$rep_1, na.rm = TRUE),
  mean_55_t2 = mean(reliability_peak_55$rep_2, na.rm = TRUE),
  sd_55_t2 = sd(reliability_peak_55$rep_2, na.rm = TRUE))
descriptives_55

## ICC -------

icc_result_55 <- icc(reliability_peak_55, model = "twoway", type = "consistency", unit = "single")

print(icc_result_55)

## CV  --------
# (SD/mean)*100

cv_55 <- reliability_peak_55 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by peak difference of trials 1 and 2 before the intervention

# 65% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_65 <- rep_data %>%
  filter(load == "65") %>% # 65% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_65 <- peak_data_65 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_65 <- peak_data_65 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_65 <- rep_1_65 %>%
  select(-c(time)) %>%
  full_join(rep_2_65) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_65 <- data.frame(
  mean_65_t1 = mean(reliability_peak_65$rep_1, na.rm = TRUE),
  sd_65_t1 = sd(reliability_peak_65$rep_1, na.rm = TRUE),
  mean_65_t2 = mean(reliability_peak_65$rep_2, na.rm = TRUE),
  sd_65_t2 = sd(reliability_peak_65$rep_2, na.rm = TRUE))
descriptives_65

## ICC -------

icc_result_65 <- icc(reliability_peak_65, model = "twoway", type = "consistency", unit = "single")

print(icc_result_65)

## CV  --------
# (SD/mean)*100

cv_65 <- reliability_peak_65 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by peak difference of trials 1 and 2 before the intervention

# 75% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_75 <- rep_data %>%
  filter(load == "75") %>% # 75% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_75 <- peak_data_75 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_75 <- peak_data_75 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_75 <- rep_1_75 %>%
  select(-c(time)) %>%
  full_join(rep_2_75) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_75 <- data.frame(
  mean_75_t1 = mean(reliability_peak_75$rep_1, na.rm = TRUE),
  sd_75_t1 = sd(reliability_peak_75$rep_1, na.rm = TRUE),
  mean_75_t2 = mean(reliability_peak_75$rep_2, na.rm = TRUE),
  sd_75_t2 = sd(reliability_peak_75$rep_2, na.rm = TRUE))
descriptives_75

## ICC -------

icc_result_75 <- icc(reliability_peak_75, model = "twoway", type = "consistency", unit = "single")

print(icc_result_75)

## CV  --------
# (SD/mean)*100

cv_75 <- reliability_peak_75 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by mean difference of trials 1 and 2 before the intervention

# 85% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_85 <- rep_data %>%
  filter(load == "85") %>% # 85% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_85 <- peak_data_85 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_85 <- peak_data_85 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_85 <- rep_1_85 %>%
  select(-c(time)) %>%
  full_join(rep_2_85) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_85 <- data.frame(
  mean_85_t1 = mean(reliability_peak_85$rep_1, na.rm = TRUE),
  sd_85_t1 = sd(reliability_peak_85$rep_1, na.rm = TRUE),
  mean_85_t2 = mean(reliability_peak_85$rep_2, na.rm = TRUE),
  sd_85_t2 = sd(reliability_peak_85$rep_2, na.rm = TRUE))
descriptives_85

## ICC -------

icc_result_85 <- icc(reliability_peak_85, model = "twoway", type = "consistency", unit = "single")

print(icc_result_85)

## CV  --------
# (SD/mean)*100

cv_85 <- reliability_peak_85 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by mean difference of trials 1 and 2 before the intervention

# 95% 1RM - peak velocity  -----------------

# Separate datasets by load of 1RM 

peak_data_95 <- rep_data %>%
  filter(load == "95") %>% # 95% 1RM
  select(id, pre_pv_rep_1:post_pv_rep_2) # only interested in peak velocity right now

# prepare data

rep_1_95 <- peak_data_95 %>%
  select(-c(pre_pv_rep_2, post_pv_rep_2)) %>%
  pivot_longer(cols = c(pre_pv_rep_1, post_pv_rep_1),
               names_to = "time",
               values_to = "rep_1")

rep_2_95 <- peak_data_95 %>%
  select(-c(pre_pv_rep_1, post_pv_rep_1)) %>%
  pivot_longer(cols = c(pre_pv_rep_2, post_pv_rep_2),
               names_to = "time",
               values_to = "rep_2")


reliability_peak_95 <- rep_1_95 %>%
  select(-c(time)) %>%
  full_join(rep_2_95) %>%
  select(-c(time, id))

## Descriptives -------

descriptives_95 <- data.frame(
  mean_95_t1 = mean(reliability_peak_95$rep_1, na.rm = TRUE),
  sd_95_t1 = sd(reliability_peak_95$rep_1, na.rm = TRUE),
  mean_95_t2 = mean(reliability_peak_95$rep_2, na.rm = TRUE),
  sd_95_t2 = sd(reliability_peak_95$rep_2, na.rm = TRUE))
descriptives_95

## ICC -------

icc_result_95 <- icc(reliability_peak_95, model = "twoway", type = "consistency", unit = "single")

print(icc_result_95)

## CV  --------
# (SD/mean)*100

cv_95 <- reliability_peak_95 %>%
  mutate(mean1 = mean (rep_1, na.rm = TRUE),
         mean2 = mean (rep_2, na.rm = TRUE),
         sd1 = sd(rep_1, na.rm = TRUE),
         sd2 = sd(rep_2, na.rm = TRUE),
         cv1 = ((sd1/mean1)*100),
         cv2 = ((sd2/mean2)*100),
         mean_cv = mean(c(cv1, cv2))) # sd divided by mean difference of trials 1 and 2 before the intervention



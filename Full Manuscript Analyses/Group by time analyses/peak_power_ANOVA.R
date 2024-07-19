# Load packages 
library(tidyverse)
library(afex)
library(emmeans)
library(stats)
library(reshape2)
library(pastecs)
library(MOTE)
library(janitor)

# Overall peak power data ---------------------------

# Load rep_data

rep_data <- read_csv("data.csv") %>%
  clean_names()
head(rep_data)

# 45% 1RM - peak power  -----------------

# Separate datasets by load of 1RM 
peak_data_45 <- rep_data %>%
  filter(load == "45") %>% # 45% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now


## Use the best of the two trials for analyses as per original paper

peak_data_45 <- peak_data_45 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_45 <- peak_data_45 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_45$group <-  as.factor(anova_peak_45$group)
anova_peak_45$time <-  as.factor(anova_peak_45$time)


## Descriptives ---------------------

summary_45 <- anova_peak_45 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_45

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_45 <- anova_peak_45 %>%
  select(peak_power)

hist_45$id <- 1:nrow(hist_45)
hist_45 <- melt(hist_45, id.vars = "id")

# Plot histogram
hist_45_plot <- ggplot(data = hist_45, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_45_plot

### Q-Q plots 

ggplot(anova_peak_45, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_45, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 45% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_45_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_45,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_45_afx

summary(anova_45_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_45_afx$lm$residuals) # residuals are not normally distributed

anova_peak_45 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_45_afx)

### Outliers check -------

anova_peak_45 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_45_emm_time <-
  emmeans::emmeans(anova_45_afx, ~ time, model = "multivariate")
anova_45_emm_time

posthoc_45 <- pairs(anova_45_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_45

# group

anova_45_emm_group <-
  emmeans::emmeans(anova_45_afx, ~ group, model = "multivariate")
anova_45_emm_group

posthoc_45_group <- pairs(anova_45_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_45_group

### Eta 45%  --------------------------------------

eta_time_45 <- eta.F(
  dfm = anova_45_afx$anova_table$`num Df`[2],
  dfe = anova_45_afx$anova_table$`den Df`[2],
  Fvalue = anova_45_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

eta_inter_45 <- eta.F(
  dfm = anova_45_afx$anova_table$`num Df`[3],
  dfe = anova_45_afx$anova_table$`den Df`[3],
  Fvalue = anova_45_afx$anova_table$F[3],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

# 55% 1RM - peak power  -----------------

# Separate datasets by load of 1RM 
peak_data_55 <- rep_data %>%
  filter(load == "55") %>% # 55% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now


## Use the best of the two trials for analyses as per original paper

peak_data_55 <- peak_data_55 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_55 <- peak_data_55 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_55$group <-  as.factor(anova_peak_55$group)
anova_peak_55$time <-  as.factor(anova_peak_55$time)



## Descriptives ---------------------

summary_55 <- anova_peak_55 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_55

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_55 <- anova_peak_55 %>%
  select(peak_power)

hist_55$id <- 1:nrow(hist_55)
hist_55 <- melt(hist_55, id.vars = "id")

# Plot histogram
hist_55_plot <- ggplot(data = hist_55, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_55_plot

### Q-Q plots 

ggplot(anova_peak_55, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_55, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 55% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_55_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_55,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_55_afx

summary(anova_55_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_55_afx$lm$residuals) # residuals are not normally distributed

anova_peak_55 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_55_afx)

### Outliers check -------

anova_peak_55 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_55_emm_time <-
  emmeans::emmeans(anova_55_afx, ~ time, model = "multivariate")
anova_55_emm_time

posthoc_55_time <- pairs(anova_55_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_55_time

# group

anova_55_emm_group <-
  emmeans::emmeans(anova_55_afx, ~ group, model = "multivariate")
anova_55_emm_group

posthoc_55_group <- pairs(anova_55_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_55_group


### Eta 55% --------------------------------------

eta_time_55 <- eta.F(
  dfm = anova_55_afx$anova_table$`num Df`[2],
  dfe = anova_55_afx$anova_table$`den Df`[2],
  Fvalue = anova_55_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

# 65% 1RM - peak power  -----------------

# Separate datasets by load of 1RM 
peak_data_65 <- rep_data %>%
  filter(load == "65") %>% # 65% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now

## Use the best of the two trials for analyses as per original paper

peak_data_65 <- peak_data_65 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_65 <- peak_data_65 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_65$group <-  as.factor(anova_peak_65$group)
anova_peak_65$time <-  as.factor(anova_peak_65$time)

## Descriptives ---------------------

summary_65 <- anova_peak_65 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_65

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_65 <- anova_peak_65 %>%
  select(peak_power)

hist_65$id <- 1:nrow(hist_65)
hist_65 <- melt(hist_65, id.vars = "id")

# Plot histogram
hist_65_plot <- ggplot(data = hist_65, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_65_plot

### Q-Q plots 

ggplot(anova_peak_65, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_65, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 65% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_65_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_65,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_65_afx

summary(anova_65_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_65_afx$lm$residuals) # residuals are not normally distributed

anova_peak_65 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_65_afx)

### Outliers check -------

anova_peak_65 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_65_emm_time <-
  emmeans::emmeans(anova_65_afx, ~ time, model = "multivariate")
anova_65_emm_time

posthoc_65_time <- pairs(anova_65_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_65_time

# group

anova_65_emm_group <-
  emmeans::emmeans(anova_65_afx, ~ group, model = "multivariate")
anova_65_emm_group

posthoc_65_group <- pairs(anova_65_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_65_group

### Eta 65% --------------------------------------

eta_time_65 <- eta.F(
  dfm = anova_65_afx$anova_table$`num Df`[2],
  dfe = anova_65_afx$anova_table$`den Df`[2],
  Fvalue = anova_65_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

# 75% 1RM - peak power  -----------------

# Separate datasets by load of 1RM 
peak_data_75 <- rep_data %>%
  filter(load == "75") %>% # 75% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now

## Use the best of the two trials for analyses as per original paper

peak_data_75 <- peak_data_75 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_75 <- peak_data_75 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_75$group <-  as.factor(anova_peak_75$group)
anova_peak_75$time <-  as.factor(anova_peak_75$time)

## Descriptives ---------------------

summary_75 <- anova_peak_75 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_75

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_75 <- anova_peak_75 %>%
  select(peak_power)

hist_75$id <- 1:nrow(hist_75)
hist_75 <- melt(hist_75, id.vars = "id")

# Plot histogram
hist_75_plot <- ggplot(data = hist_75, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_75_plot

### Q-Q plots 

ggplot(anova_peak_75, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_75, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 75% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_75_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_75,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_75_afx

summary(anova_75_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_75_afx$lm$residuals) # residuals are not normally distributed

anova_peak_75 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_75_afx)

### Outliers check -------

anova_peak_75 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_75_emm_time <-
  emmeans::emmeans(anova_75_afx, ~ time, model = "multivariate")
anova_75_emm_time

posthoc_75_time <- pairs(anova_75_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_75_time

# group

anova_75_emm_group <-
  emmeans::emmeans(anova_75_afx, ~ group, model = "multivariate")
anova_75_emm_group

posthoc_75_group <- pairs(anova_75_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_75_group

### Eta 75% --------------------------------------

eta_time_75 <- eta.F(
  dfm = anova_75_afx$anova_table$`num Df`[2],
  dfe = anova_75_afx$anova_table$`den Df`[2],
  Fvalue = anova_75_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

# 85% 1RM - peak power  -----------------


# Separate datasets by load of 1RM 
peak_data_85 <- rep_data %>%
  filter(load == "85") %>% # 85% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now


## Use the best of the two trials for analyses as per original paper

peak_data_85 <- peak_data_85 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_85 <- peak_data_85 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_85$group <-  as.factor(anova_peak_85$group)
anova_peak_85$time <-  as.factor(anova_peak_85$time)


## Descriptives ---------------------

summary_85 <- anova_peak_85 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_85

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_85 <- anova_peak_85 %>%
  select(peak_power)

hist_85$id <- 1:nrow(hist_85)
hist_85 <- melt(hist_85, id.vars = "id")

# Plot histogram
hist_85_plot <- ggplot(data = hist_85, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_85_plot

### Q-Q plots 

ggplot(anova_peak_85, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_85, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 85% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_85_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_85,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_85_afx

summary(anova_85_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_85_afx$lm$residuals) # residuals are not normally distributed

anova_peak_85 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_85_afx)

### Outliers check -------

anova_peak_85 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_85_emm_time <-
  emmeans::emmeans(anova_85_afx, ~ time, model = "multivariate")
anova_85_emm_time

posthoc_85_time <- pairs(anova_85_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_85_time

# group

anova_85_emm_group <-
  emmeans::emmeans(anova_85_afx, ~ group, model = "multivariate")
anova_85_emm_group

posthoc_85_group <- pairs(anova_85_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_85_group

### Eta 85% --------------------------------------

eta_time_85 <- eta.F(
  dfm = anova_85_afx$anova_table$`num Df`[2],
  dfe = anova_85_afx$anova_table$`den Df`[2],
  Fvalue = anova_85_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier

# 95% 1RM - peak power  -----------------


# Separate datasets by load of 1RM 
peak_data_95 <- rep_data %>%
  filter(load == "95") %>% # 95% 1RM
  select(id, group, pre_pp_rep_1:post_pp_rep_2) # only interested in peak power right now


## Use the best of the two trials for analyses as per original paper

peak_data_95 <- peak_data_95 %>% 
  rowwise() %>%
  mutate(pre = max(pre_pp_rep_1, pre_pp_rep_2, na.rm = TRUE),
         post = max(post_pp_rep_1, post_pp_rep_2, na.rm = TRUE)) %>%
  as.data.frame()

# Long format data

anova_peak_95 <- peak_data_95 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_peak_95$group <-  as.factor(anova_peak_95$group)
anova_peak_95$time <-  as.factor(anova_peak_95$time)




## Descriptives ---------------------

summary_95 <- anova_peak_95 %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_95

## Plots -------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_95 <- anova_peak_95 %>%
  select(peak_power)

hist_95$id <- 1:nrow(hist_95)
hist_95 <- melt(hist_95, id.vars = "id")

# Plot histogram
hist_95_plot <- ggplot(data = hist_95, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist_95_plot

### Q-Q plots 

ggplot(anova_peak_95, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_peak_95, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## 95% Peak Power ANOVA -----------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_95_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_peak_95,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_95_afx

summary(anova_95_afx)


### Assumption checking ---------

# Normality test

shapiro.test(anova_95_afx$lm$residuals) # residuals are not normally distributed

anova_peak_95 %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_95_afx)

### Outliers check -------

anova_peak_95 %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ---------------------------------------------

# time

anova_95_emm_time <-
  emmeans::emmeans(anova_95_afx, ~ time, model = "multivariate")
anova_95_emm_time

posthoc_95_time <- pairs(anova_95_emm_time, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_95_time

# group

anova_95_emm_group <-
  emmeans::emmeans(anova_95_afx, ~ group, model = "multivariate")
anova_95_emm_group

posthoc_95_group <- pairs(anova_95_emm_group, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthoc_95_group

### Eta 95% --------------------------------------

eta_time_95 <- eta.F(
  dfm = anova_95_afx$anova_table$`num Df`[2],
  dfe = anova_95_afx$anova_table$`den Df`[2],
  Fvalue = anova_95_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier
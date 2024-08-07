# Load packages 
library(tidyverse)
library(afex)
library(emmeans)
library(stats)
library(reshape2)
library(pastecs)
library(MOTE)
library(janitor)

set.seed(21)

# Replication data --------------------------------------------------------------------
# Overall peak power data ---------------------------

# Load rep_data

rep_data <- read_csv("rep_data.csv") %>%
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

anova_rep_data <- peak_data_45 %>%
  select(-c(pre_pp_rep_1:post_pp_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_rep_data$group <-  as.factor(anova_rep_data$group)
anova_rep_data$time <-  as.factor(anova_rep_data$time)


## Descriptives ---------------------

summary_rep_data <- anova_rep_data %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_rep_data


## Descriptives ---------------------

summary_rep_data <- anova_rep_data %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_rep_data

anova_rep_data %>%
  group_by(time) %>%
  summarise(mean = mean(peak_power),
            sd = sd(peak_power))

### Plots ---------------------------------------------------------------------------

### Histogram 

# Prepare anova_rep_data
hist_dat <- anova_rep_data %>%
  select(peak_power)

hist_dat$id <- 1:nrow(hist_dat)
hist_dat <- melt(hist_dat, id.vars = "id")

# Plot histogram
hist <- ggplot(data = hist_dat, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Peak Power")
hist

### Q-Q plots 

ggplot(anova_rep_data, aes(sample = peak_power)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")


### Boxplot 

ggplot(anova_rep_data, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## Replication ANOVA ----------------------------------------------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_rep_data_afx <- afex::aov_4(
  peak_power ~ group * time + (time | id),
  data = anova_rep_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anova_rep_data_afx

summary(anova_rep_data_afx)

### Assumption checking ---------

# Normality test

shapiro.test(anova_rep_data_afx$lm$residuals) 

anova_rep_data %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_rep_data_afx)

### Outliers check -------

anova_rep_data %>%
  group_by(time) %>%
  rstatix::identify_outliers(peak_power)

### Post hoc contrasts ----------------------------------------------------------------------------

anova_rep_data_emm <-
  emmeans::emmeans(anova_rep_data_afx, ~ time, model = "multivariate")
anova_rep_data_emm

posthocresults <- pairs(anova_rep_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

## Replication effect size ----------------------------------------------------------------------------

pes_rep <- eta.F(
  dfm = anova_rep_data_afx$anova_table$`num Df`[2],
  dfe = anova_rep_data_afx$anova_table$`den Df`[2],
  Fvalue = anova_rep_data_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier
pes_rep

# Original data ------------------
## Data prep 

# Load orig_data

orig_data <- read_csv("peak_power_original.csv")
head(orig_data)

# Prepare orig_data

orig_data <- orig_data %>% 
  select(where(~ !(all(is.na(.)) | all(. == ""))))


orig_data <- orig_data %>%
  clean_names() %>%
  rowwise() %>%
  drop_na()

orig_data <- orig_data %>%
  rename(pre = best_peak_power_45_percent_pre,
         post = best_peak_power_45_percent_post)

# Use the best of the two trials for analyses

anova_orig_data <- orig_data %>%
  select(-c(peak_power_45_percent_trial_1_pre, peak_power_45_percent_trial_2_pre, 
            peak_power_45_percent_trial_1_post, peak_power_45_percent_trial_2_post)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "peak_power") 

anova_orig_data$group <-  as.factor(anova_orig_data$group)
anova_orig_data$time <-  as.factor(anova_orig_data$time)

## Descriptives ---------------------

summary_orig_data <- anova_orig_data %>%
  group_by(group, time) %>%
  summarise(count = n (),
            mean = mean(peak_power),
            sd = sd(peak_power))
summary_orig_data

# Boxplot 

ggplot(anova_orig_data, aes(x = time, y = peak_power)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## Original ANOVA  ----------------------------------------------------------------------------

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_orig_data_afx <- afex::aov_4(
  peak_power ~ group * time + (time | participant),
  data = anova_orig_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction if needed and partial eta squared
anova_orig_data_afx

summary(anova_orig_data_afx)

### Assumption checking ---------

# Normality test

shapiro.test(anova_orig_data_afx$lm$residuals) 

anova_orig_data %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(peak_power)

# Homeogeneity test
performance::check_homogeneity(anova_orig_data_afx)

## Original effect size ----------------------------------------------------------------------------

pes_orig <- eta.F(
  dfm = anova_orig_data_afx$anova_table$`num Df`[2],
  dfe = anova_orig_data_afx$anova_table$`den Df`[2],
  Fvalue = anova_orig_data_afx$anova_table$F[2],
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Original study")) # add identifier
pes_orig

# Replication test -----
# main effect of time

pes_rep_eta = anova_rep_data_afx$anova_table$pes[2]
df_rep = anova_rep_data_afx$anova_table$`den Df`[2]
pes_orig_eta = anova_orig_data_afx$anova_table$pes[2]
df_ori = anova_orig_data_afx$anova_table$`den Df`[2]

rho_ori = 2 * sqrt(pes_orig_eta) - 1
rho_rep = 2 * sqrt(pes_rep_eta) - 1

rep_test = TOSTER::compare_cor(r1 = rho_ori,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test

# Forest plot ---------

## Labels for peak_power forest plot -------------
label_rep <- "0.129 [0.00, 0.42]"
label_ori <- "0.304 [0.00, 0.68]"

## Join rep_datasets -----------------
plot <-
  merge(
    pes_orig,
    pes_rep,
    by = c("eta", "etalow", "etahigh", "study_id"),
    all = TRUE
  )

## Plot -----------------------------
ggplot(plot,
       aes(
         y = study_id,
         x = eta,
         xmin = etalow,
         xmax = etahigh
       )) +
  ggtitle("Partial eta squared [95% CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-1, 2.2)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 1.8,
           y = 2,
           label = label_rep) +
  annotate("text",
           x = 1.8,
           y = 1,
           label = label_ori) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.94),
    panel.background = element_blank()
  )

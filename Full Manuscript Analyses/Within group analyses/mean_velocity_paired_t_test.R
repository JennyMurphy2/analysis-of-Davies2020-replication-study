# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Run ANOVA one first

# Cluster paired t-test 45 --------------

paired_45 <- mean_data_45 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_45 <- mean_data_45 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_45, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_45, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_45 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_45 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster45_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_45, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster45_ttest

### Effect size calculation ------

clus_45_dz <- d.dep.t.diff.t(t = cluster45_ttest$statistic, n = 14, a = 0.05)
clus_45_dz

# Trad paired t-test 45 --------------

paired_long_trad_45 <- mean_data_45 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_45, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_45, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_45 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_45 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad45_ttest <- t.test(mean_velocity ~ time, paired_long_trad_45, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
trad45_ttest

### Effect size calculation ------

trad_45_dz <- d.dep.t.diff.t(t = trad45_ttest$statistic, n = 11, a = 0.05)
trad_45_dz

# Cluster paired t-test 55 --------------

paired_55 <- mean_data_55 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_55 <- mean_data_55 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_55, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_55, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_55 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_55 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster55_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_55, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster55_ttest

### Effect size calculation ------

clus_55_dz <- d.dep.t.diff.t(t = cluster55_ttest$statistic, n = 14, a = 0.05)
clus_55_dz

# Trad paired t-test 55 --------------

paired_long_trad_55 <- mean_data_55 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_55, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_55, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_55 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_55 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad55_ttest <- t.test(mean_velocity ~ time, paired_long_trad_55, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
trad55_ttest

### Effect size calculation ------

trad_55_dz <- d.dep.t.diff.t(t = trad55_ttest$statistic, n = 11, a = 0.05)
trad_55_dz

# Cluster paired t-test 65 --------------

paired_65 <- mean_data_65 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_65 <- mean_data_65 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_65, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_65, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_65 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_65 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster65_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_65, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster65_ttest

### Effect size calculation ------

clus_65_dz <- d.dep.t.diff.t(t = cluster65_ttest$statistic, n = 14, a = 0.05)
clus_65_dz

# Trad paired t-test 65 --------------

paired_long_trad_65 <- mean_data_65 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_65, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_65, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_65 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_65 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad65_ttest <- t.test(mean_velocity ~ time, paired_long_trad_65, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
trad65_ttest

### Effect size calculation ------

trad_65_dz <- d.dep.t.diff.t(t = trad65_ttest$statistic, n = 11, a = 0.05)
trad_65_dz

# Cluster paired t-test 75 --------------

paired_75 <- mean_data_75 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_75 <- mean_data_75 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_75, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_75, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_75 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_75 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster75_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_75, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster75_ttest

### Effect size calculation ------

clus_75_dz <- d.dep.t.diff.t(t = cluster75_ttest$statistic, n = 14, a = 0.05)
clus_75_dz

# Trad paired t-test 75 --------------

paired_long_trad_75 <- mean_data_75 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_75, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_75, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_75 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_75 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad75_ttest <- t.test(mean_velocity ~ time, paired_long_trad_75, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
trad75_ttest

### Effect size calculation ------

trad_75_dz <- d.dep.t.diff.t(t = trad75_ttest$statistic, n = 11, a = 0.05)
trad_75_dz

# Cluster paired t-test 85 --------------

paired_85 <- mean_data_85 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_85 <- mean_data_85 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_85, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_85, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_85 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_85 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster85_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_85, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster85_ttest

### Effect size calculation ------

clus_85_dz <- d.dep.t.diff.t(t = cluster85_ttest$statistic, n = 14, a = 0.05)
clus_85_dz

# Trad paired t-test 85 --------------

paired_long_trad_85 <- mean_data_85 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_85, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_85, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_85 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_85 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad85_ttest <- t.test(mean_velocity ~ time, paired_long_trad_85, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.85) %>%
  broom::tidy()
trad85_ttest

### Effect size calculation ------

trad_85_dz <- d.dep.t.diff.t(t = trad85_ttest$statistic, n = 11, a = 0.05)
trad_85_dz

# Cluster paired t-test 95 --------------

paired_95 <- mean_data_95 %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2))  %>% 
  mutate(differences =  pre - post) # add differences column to wide dataset

paired_long_cluster_95 <- mean_data_95 %>% 
  filter(group != "trad") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_cluster_95, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_cluster_95, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_95 %>% 
  filter(group != "trad") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_95 %>% 
  filter(group != "trad") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

cluster95_ttest <- t.test(mean_velocity ~ time, paired_long_cluster_95, 
                          alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
cluster95_ttest

### Effect size calculation ------

clus_95_dz <- d.dep.t.diff.t(t = cluster95_ttest$statistic, n = 14, a = 0.05)
clus_95_dz

# Trad paired t-test 95 --------------

paired_long_trad_95 <- mean_data_95 %>% 
  filter(group != "cluster") %>%
  select(-c(pre_mv_rep_1:post_mv_rep_2)) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time",
               values_to = "mean_velocity")

## Resolving assumvtions  --------------------

## Distribution check 

ggplot(paired_long_trad_95, aes(mean_velocity)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(paired_long_trad_95, aes(time, mean_velocity, color = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_95 %>% 
  filter(group != "cluster") %>%
  rstatix::identify_outliers(differences)

### Normality check  

paired_95 %>% 
  filter(group != "cluster") %>%
  rstatix::shapiro_test(differences) 

## Paired t-test  -----------------------------

trad95_ttest <- t.test(mean_velocity ~ time, paired_long_trad_95, 
                       alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  broom::tidy()
trad95_ttest

### Effect size calculation ------

trad_95_dz <- d.dep.t.diff.t(t = trad95_ttest$statistic, n = 11, a = 0.05)
trad_95_dz


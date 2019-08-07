#this file analyzes the some quasi quantitative responses with categorical variables

#dependencies
library(tidyverse)

#data 
clean_wp <- read_csv('data/clean_wp.csv')
wp_quant <- clean_wp[,69:96]

#create employer size variable that's easy to call
clean_wp$employer_size <- clean_wp$`What is the size of your current employer?`

#note all data in df must be quantitative
all_aov <- function(df, x){ 
  y_var <- c()
  f <- c()
  pvalue <- c()
  n <- c()
  for (i in 1:ncol(df)){
    y_var[i] <- names(df)[[i]]
    a <- aov(df[[i]] ~ x)
    as <- summary(a)[[1]]
    f[i] <- as[1,4]
    pvalue[i] <- as[1,5]
    n[i] <- as[2,1] + as[1,1] + 1
  }
  return(data.frame(y_var, n, f, pvalue))
}
table(clean_wp$employer_size)
employer_size_anova <- wp_quant %>% all_aov(clean_wp$employer_size)
employer_size_anova
employer_size_anova %>% filter(pvalue < .05)

ggplot(clean_wp, aes(employer_size)) + geom_bar()
ggplot(clean_wp, aes(employer_size, lonely)) +  geom_violin()
ggplot(clean_wp, aes(employer_size, bad_sleep)) +  geom_violin()
ggplot(clean_wp, aes(employer_size, bad_sleep)) +  geom_boxplot()

#making only male and female gender because only 1 other gender response
wp_gender <- clean_wp %>% filter(Gender == 'Female' | Gender == 'Male') %>% 
  select(Gender, Org_satisfaction:house_income_midpoint)

all_t <- function(df, x){ 
  y_var <- c()
  pvalue <- c()
  n <- c()
  t <- c()
  for (i in 2:ncol(df)){
    y_var[i] <- names(df)[[i]]
    a <- t.test(df[[i]] ~ x)
    t[i] <- a$statistic
    pvalue[i] <- a$p.value
  }
  return(data.frame(y_var, t, pvalue))
}

gender_trends <- all_t(df= wp_gender, x = wp_gender$Gender)
gender_trends %>% filter(pvalue < .05)

ggplot(wp_gender, aes(Gender, use_time_efficient)) + geom_violin()
wp_gender %>% group_by(Gender) %>% summarize(mean(use_time_efficient, na.rm = T))
ggplot(wp_gender, aes(Gender, efficiency_compared_to_peers)) + geom_violin()


ggplot(wp_gender, aes(Gender, feel_constant_need_to_look_at_phone)) + geom_violin()
ggplot(wp_gender, aes(Gender, coworkers_happier)) + geom_violin()

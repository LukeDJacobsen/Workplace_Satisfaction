#dependencies
library(tidyverse)

#data 
clean_wp <- read_csv('data/clean_wp.csv')

#EDA
#phone time at work and personal satisfaction
names(clean_wp)
ggplot(clean_wp, aes(phone_at_work_time ,personal_satisfaction))  +
  geom_jitter(height = .4, width = .4)
sat_mod <- lm(personal_satisfaction ~ phone_at_work_time, data = clean_wp)
summary(sat_mod)

#engaged while at work and phone use at work
ggplot(clean_wp, aes(phone_at_work_time, still_engaged)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth()
ggplot(clean_wp, aes(as.factor(still_engaged),phone_at_work_time)) + geom_boxplot()
engaged_mod <- lm(personal_satisfaction ~ still_engaged, data = clean_wp)
summary(engaged_mod)
plot(engaged_mod)

#
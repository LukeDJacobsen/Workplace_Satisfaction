#dependencies
library(tidyverse)

#data 
clean_wp <- read_csv('data/clean_wp.csv')

#EDA
#phone time at work and personal satisfaction
names(clean_wp)
ggplot(clean_wp, aes(phone_at_work_time ,personal_satisfaction))  +
  geom_jitter(height = .4, width = .4) + geom_smooth(method = lm)
sat_mod <- lm(personal_satisfaction ~ phone_at_work_time, data = clean_wp)
summary(sat_mod)

#engaged while at work and phone use at work
ggplot(clean_wp, aes(phone_at_work_time, still_engaged)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth()
ggplot(clean_wp, aes(as.factor(still_engaged),phone_at_work_time)) + geom_boxplot()
engaged_mod <- lm(still_engaged ~ phone_at_work_time, data = clean_wp)
summary(engaged_mod)

#depressed while at work and phone use
ggplot(clean_wp, aes(phone_at_work_time, Depressed_while_at_work)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth()
ggplot(clean_wp, aes(phone_at_work_time, Depressed_while_at_work)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth(method = lm)
depressed_mod <- lm(Depressed_while_at_work ~ phone_at_work_time, data = clean_wp)
summary(depressed_mod)

#lonely to phone at work
ggplot(clean_wp, aes(phone_at_work_time, lonely)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth()

#looking for new work to phone use
ggplot(clean_wp, aes(phone_at_work_time, searching_new_employment)) + 
  geom_jitter(height = .4, width = .4) + geom_smooth()

#efficiency to phone use
ggplot(clean_wp, aes(phone_at_work_time, use_time_efficient)) + 
  geom_jitter(height = .4, width = .4) +geom_smooth(method = lm)
efficient_mod <- lm(use_time_efficient ~ phone_at_work_time, data = clean_wp)
summary(efficient_mod)


ggplot(clean_wp, aes(phone_at_work_time, go_above)) + 
  geom_jitter(height = .4, width = .4) +geom_smooth(method = lm)


#======================================================================
#largest linear relationship with phone time at work
#======================================================================
names(clean_wp)
pvals <- c()
estimates <- c()
for (i in 7:ncol(clean_wp)){
  if (typeof(clean_wp[[i]]) == "double"){
    #extracts slope p-value and slope estimate
    mod_table <- as.data.frame(summary(lm(clean_wp[[i]] ~ clean_wp$phone_at_work_time))[4])
    pvals[[i]] <- mod_table[2,4]
    estimates[[i]] <- mod_table[2,1]
  }
}
pvals
estimates
#variables with worth while to look at
#depressed, personal_satisfaction
#go_above
#use time efficient
#difficult to concentrate
#efficient compared to peers
#motivated
#other better lives

#variables with very small p-values indicating very likely some relationship
#go above and use time efficient
names(clean_wp)
summary(lm(use_time_efficient ~ Gender + phone_at_work_time, data = clean_wp))
summary(lm(use_time_efficient ~ Gender * phone_at_work_time, data = clean_wp))

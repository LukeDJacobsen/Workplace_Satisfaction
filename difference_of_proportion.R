#dependencies and data import
library(tidyverse)
clean_wp <- read_csv('data/clean_wp.csv')

#new variables needed
clean_wp$difficult_to_concentrate_p <- ifelse(clean_wp$difficult_to_concentrate <=3,'no', 'absolutely') 
clean_wp$feel_constant_need_to_look_at_phone_p <- ifelse(clean_wp$feel_constant_need_to_look_at_phone <=3,'no', 'absolutely') 
clean_wp$stress_level_p <- ifelse(clean_wp$stress_level <=2,'no', 'absolutely') 
clean_wp$depressive_tendencies_p <- ifelse(clean_wp$depressive_tendencies <=3,'no', 'absolutely') 
clean_wp$is_phone_distracting_p <- ifelse(clean_wp$is_phone_distracting <=3,'no', 'absolutely') 
clean_wp$bad_sleep_p <- ifelse(clean_wp$bad_sleep <=3,'no', 'absolutely') 
clean_wp$mobile_per_day_p <- ifelse(clean_wp$mobile_per_day <=3,'no', 'absolutely') 

#tests
phone_con <- table(clean_wp$difficult_to_concentrate_p,clean_wp$feel_constant_need_to_look_at_phone_p)
prop.test(phone_con)
dep_need <- table(clean_wp$depressive_tendencies_p,clean_wp$feel_constant_need_to_look_at_phone_p)
prop.test(dep_need)
con_stress <- table(clean_wp$difficult_to_concentrate_p,clean_wp$stress_level_p)
prop.test(con_stress)
sleep_mobile <- table(clean_wp$bad_sleep_p,clean_wp$mobile_per_day_p)
prop.test(sleep_mobile)
phone_stress <- table(clean_wp$feel_constant_need_to_look_at_phone_p,clean_wp$stress_level_p)
prop.test(phone_stress)
mobile_stress <- table(clean_wp$mobile_per_day_p,clean_wp$stress_level_p)
prop.test(mobile_stress)


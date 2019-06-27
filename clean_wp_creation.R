======================================================================
#Dependencies
library(tidyverse)
======================================================================

======================================================================
#data import
Full_wp <- read_csv('data/Full_wp.csv')
Full_wp
======================================================================
  
======================================================================
#Wrangling. End goal to have same dataset, but with quantitative responses 
#for responses which that could make sense.

#first step, get rid of completely NA columns (good thing these
#columns are NAs or there would be privacy concerns).

Full_wp <- Full_wp %>% dplyr::select(-c('IP Address', 'Email Address', 'First Name',
                             'Last Name'))
names(Full_wp)
#need to add quantitative responses where appropriate

table(Full_wp$`Overall, I am satisfied working in the organization I'm in.`)
Full_wp$Overall_satisfaction <- ifelse(Full_wp$`Overall, I am satisfied working in the organization I'm in.` == "Agree", 4,
                                       ifelse(Full_wp$`Overall, I am satisfied working in the organization I'm in.` == "Disagree", 2,
                                           ifelse(Full_wp$`Overall, I am satisfied working in the organization I'm in.` == "Neither agree nor disagree", 3,
                                              ifelse(Full_wp$`Overall, I am satisfied working in the organization I'm in.` == "Strongly agree", 5,
                                                     ifelse(Full_wp$`Overall, I am satisfied working in the organization I'm in.` == "Strongly disagree", 1, NA)))))
ordinal_to_quant <- function(column){
  ifelse(column == "Agree", 4,
  ifelse(column == "Disagree", 2,
  ifelse(column == "Neither agree nor disagree", 3,
  ifelse(column == "Strongly agree", 5,
  ifelse(column == "Strongly disagree", 1, NA)))))
}

freq_to_quant <- function(column){
  ifelse(column == "Always", 4,
         ifelse(column == "Rarely", 2,
                ifelse(column == "Sometimes", 3,
                      ifelse(column == "Never", 1, NA))))
}
Full_wp$Org_satisfaction <- ordinal_to_quant(Full_wp$`Overall, I am satisfied working in the organization I'm in.`)
Full_wp$Better_org <- ordinal_to_quant(Full_wp$`This organization is a better place to work than other organizations that I've had experience with`)
Full_wp$still_engaged <- ordinal_to_quant(Full_wp$`Functions of my job that once engaged me no longer do so`)
Full_wp$professional_path_sat <- ordinal_to_quant(Full_wp$`I am satisfied on the professional path I am on`)
Full_wp$coworkers_happier <- ordinal_to_quant(Full_wp$`I perceive co-workers to be more fulfilled in their jobs than I am.`)
Full_wp$Depressed_while_at_work <- ordinal_to_quant(Full_wp$`I feel depressed while at work`)
Full_wp$searching_new_employment <- ordinal_to_quant(Full_wp$`I am likely to search for new opportunities of employement`)
Full_wp$personal_satisfaction <- ordinal_to_quant(Full_wp$`My time at work brings me personal satisfaction`)
Full_wp$go_above <- ordinal_to_quant(Full_wp$`I go above and beyond what is expected of me at work`)
Full_wp$use_time_efficient <- ordinal_to_quant(Full_wp$`I utilize my time at work efficiently`)
Full_wp$fatigued <- freq_to_quant(Full_wp$`At times I feel fatigued and drowsy at work`)
Full_wp$lonely <- freq_to_quant(Full_wp$`At times I feel lonely`)
Full_wp$difficult_to_concentrate <- ordinal_to_quant(Full_wp$`I find it difficult to concentrate on tasks for sustained amounts of time`)
Full_wp$efficiency_compared_to_peers <- ordinal_to_quant(Full_wp$`Compared to my peers, I feel I am less efficient at work`)
Full_wp$positive_feedback <- ordinal_to_quant(Full_wp$`I receive positive feedback on the quality of my work`)
Full_wp$motivated <- ordinal_to_quant(Full_wp$`I feel motivated to do my job to the best of my ability`)
Full_wp$other_better_lives <- ordinal_to_quant(Full_wp$`I think others have a better life than me`)
Full_wp$depressive_tendencies <- freq_to_quant(Full_wp$`I feel I have depressive tendencies`)
Full_wp$bad_sleep <- freq_to_quant(Full_wp$`I suffer from poor sleep - either too much, too little, or irregular sleeping patterns`)
Full_wp$lonely <- freq_to_quant(Full_wp$`I feel lonely`)
Full_wp$`On average, how many cumulative hours per day do you spend on your mobile device?`
Full_wp$social_media_makes_lonely <- ordinal_to_quant(Full_wp$`Social Media usage makes me feel less lonely`)
Full_wp$feel_constant_need_to_look_at_phone <- Full_wp$`I feel a constant need to check my smartphone`
Full_wp$is_phone_distracting <- freq_to_quant(Full_wp$`In general, how much do you feel your smartphone is a distraction while you are trying to concentrate?`)
Full_wp$phone_better_life <-  ordinal_to_quant(Full_wp$`My day-to-day smartphone usage enhances the quality of my life`)
table(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?`)

Full_wp$phone_at_work_time <- ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "0-1 Hour", 0,
       ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "1-2 Hours", 1,
              ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "2-3 Hours",2,
                     ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "3-4 Hours", 3,
                            ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "5 + Hours", 5,
                                   ifelse(Full_wp$`People check their phones multiple times a day. During your 8 hour workday, please estimate the total time spent on your mobile device on tasks unrelated to work?` == "4-5 Hours", 4, NA))))))
======================================================================
write_csv(Full_wp, 'clean_wp.csv')
?write_csv

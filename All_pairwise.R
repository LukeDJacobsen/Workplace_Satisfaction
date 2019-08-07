#dependencies
library(tidyverse)

#data 
clean_wp <- read_csv('data/clean_wp.csv')

################################################################################
#Goal: Correlation matrix of quantitative variables to understand strongest relationships
################################################################################

#just take quantitatives
wp_quant <- clean_wp[,69:96]


#function that creats dataframe with simple linear regression summaries
#for all pairwise comparisons

pairwise_lm <- function(df){
  var_1 <- c()
  var_2 <- c()
  intercept_est <- c()
  slope_est <- c()
  p_value <- c()
  n <- c()
  k <- 0
  for (i in 1:ncol(df)){
    for(j in 1:ncol(df)){
      k <- k + 1
      j <- ifelse(i > j, i, j)
      m <- lm(df[[i]] ~ df[[j]])
      ms <- summary(m)
      var_1[[k]] <- names(df)[[i]]
      var_2[[k]] <- names(df)[[j]]
      intercept_est[[k]] <- ms$coefficients[1,1]
      slope_est[[k]] <- ms$coefficients[2,1]
      p_value[[k]] <- ms$coefficients[2,4]
      n[[k]] <- ms$df[2] + 1
    }
  }
  desired_df <- data.frame(var_1, var_2, intercept_est, slope_est, p_value, n)
  desired_df <- desired_df[!duplicated(desired_df), ]
  return(desired_df)
}
all_pairwise <- pairwise_lm(wp_quant)
all_pairwise
#all significant with very conservative p-value threshold (bonferroni)
conservative_sig <- all_pairwise %>% filter(all_pairwise$p_value < .05/(381 - 26) & all_pairwise$p_value > 0)
dim(conservative_sig)
conservative_sig

#all significant with very liberal p-value threshold
liberal_sig <- all_pairwise %>% filter(all_pairwise$p_value < .05 & all_pairwise$p_value > 0)
dim(liberal_sig)

#significant with very liberal p-value threshold, but not conservative threshold
only_liberal_sig <- all_pairwise %>% filter(all_pairwise$p_value < .05 & all_pairwise$p_value > .05/(381-26))

variables_significant_with <- function(df, variable){
  return(df %>% filter(var_1 == variable | var_2 == variable))
}

conservative_sig %>% variables_significant_with('phone_at_work_time')
liberal_sig %>% variables_significant_with('phone_at_work_time')

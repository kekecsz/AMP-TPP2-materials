
### Confirmatory data analysis code of the AMP-TPP2 project at preregistration

#############################################################
#                                                           #
#                        Packages                           #
#                                                           #
#############################################################

library(lme4) # for glmer()
library(tidyverse)


#############################################################
#                                                           #
#                   Custom functions                        #
#                                                           #
#############################################################

### to convert logit to probability
### this is used for conversion of the results of the
### logistic regression to the probability scale

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


#############################################################
#                                                           #
#                 Set analysis parameters                   #
#                                                           #
#############################################################

max_num_trials = 127000
Inference_threshold_NHST = 0.05
M0_prob = 0.5 # probability of correct guesses if M0 is true


#############################################################
#                                                           #
#                Load and manage data                       #
#                                                           #
#############################################################

raw_data = read.csv("LINKTOTHEDATA")

## exclude test data
## we exclude rows which were generated with the test account

raw_data[,"sides_match"] = as.factor(tolower(as.logical(raw_data[,"sides_match"])))
raw_data[,"participant_ID"] = as.factor(raw_data[,"participant_ID"])

# sessions conducted with the test accounts or without lab_IDs are excluded
data_nontest = raw_data %>% 
  filter(experimenter_ID_code != "29d6375047925c1fbf618f0c9bf68084c5d8c25e8840c2bdcb0945213eda6052")

# add a row_counter, which will be useful to distinguish data coming in after the stopping rule was met.
data_nontest[, "row_counter"] = 1:nrow(data_nontest)

# extract data from erotic trials 
data_nontest_trials = data_nontest[!is.na(data_nontest[, "trial_number"]),]
data_nontest_trials_erotic = data_nontest_trials[data_nontest_trials[, "reward_type"] == "erotic", ]
# drop unused factor levels
data_nontest_trials_erotic[,"participant_ID"] = droplevels(data_nontest_trials_erotic[,"participant_ID"])

# drop any data that is above the maximum trial size
if(nrow(data_nontest_trials_erotic) > max_num_trials){
  data_nontest_trials_erotic_maxtrialnum = data_nontest_trials_erotic[1:max_num_trials,]
} else {data_nontest_trials_erotic_maxtrialnum = data_nontest_trials_erotic}

# row number of the final row in the analysis
last_row = data_nontest_trials_erotic_maxtrialnum[nrow(data_nontest_trials_erotic_maxtrialnum), "row_counter"]

# sides matching as a numerical variable
data_nontest_trials_erotic_maxtrialnum[,"sides_match_numeric"] = as.numeric(as.logical(data_nontest_trials_erotic_maxtrialnum[,"sides_match"]))

######################################################################
#                                                                    #
#                    Primary confirmatory test                       #
#                                                                    #
######################################################################


### Primary confirmatory analysis: mixed model binary logistic regression

mod_mixed = glmer(sides_match_numeric ~ 1 + (1|participant_ID), data = data_nontest_trials_erotic_maxtrialnum, family = "binomial")

estimate_mixed = summary(mod_mixed)$coefficients[1,1]
se_mixed = summary(mod_mixed)$coefficients[1,2]

wald_ci_mixed_logit <- c(estimate_mixed - se_mixed* qnorm(1-((Inference_threshold_NHST)/2)),
                         estimate_mixed + se_mixed* qnorm(1-((Inference_threshold_NHST)/2)))
wald_ci_mixed = logit2prob(wald_ci_mixed_logit)

CI_lower_mixed = wald_ci_mixed[1]
CI_upper_mixed = wald_ci_mixed[2]

# results of the mixed model analysis
CI_lower_mixed
CI_upper_mixed


# final statistical inference based on the mixed model
if(CI_upper_mixed < M0_prob){conclusion = "M1"} else if(CI_lower_mixed > M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
conclusion

### Robustness analysis using binomial test

successes = sum(as.logical(data_nontest_trials_erotic_maxtrialnum[,"sides_match"]))
total_n_of_trials = nrow(data_nontest_trials_erotic_maxtrialnum)


CI_lower_binomtest = binom.test(x = successes, n = total_n_of_trials, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[1]
CI_upper_binomtest = binom.test(x = successes, n = total_n_of_trials, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[2]

## results of the binomial test
CI_lower_binomtest
CI_upper_binomtest





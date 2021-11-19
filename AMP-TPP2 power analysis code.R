library(lme4)
library(pbapply)

mround <- function(x,base){ 
  base*round(x/base) 
} 

talent_randomizer <- function(True_prob, SD_personal_differences){
  talent = True_prob + rnorm(mean = 0, sd = SD_personal_differences, n = 1)
  if(talent<0.001){talent = 0.001}else if(talent>0.999){talent = 0.999}
  return(talent)
}

# convert logit to probability
# this is used for conversion of the results of the
# logistic regression to the probability scale
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}




data_generator <- function(erotic_trial_per_person, True_prob, max_num_trials, SD_personal_differences){
  if((SD_personal_differences != 0) & (True_prob != 0.5)){
    data_all_M1 <- as.vector(replicate(mround(max_num_trials, base = erotic_trial_per_person)/erotic_trial_per_person, rbinom(erotic_trial_per_person, size = 1, prob=talent_randomizer(True_prob = True_prob, SD_personal_differences = SD_personal_differences))))
  } else {
    data_all_M1 <- rbinom(mround(max_num_trials, base = erotic_trial_per_person), size = 1, prob=True_prob)
  }
  data_all_M1_dataframe = as.data.frame(cbind(data_all_M1, rep(1:(mround(max_num_trials, base = erotic_trial_per_person)/erotic_trial_per_person), each = erotic_trial_per_person)))
  data_all_M1_dataframe[,2] = as.factor(data_all_M1_dataframe[,2])
  names(data_all_M1_dataframe) = c("outcome","ID")
  
  data_dataframe = data_all_M1_dataframe
  return(data_dataframe)
}






analyser <- function(Primary_analysis, data_dataframe, Inference_threshold_robustness_NHST, M0_prob){
  if(any(Primary_analysis == "Mixed_NHST")){  
    mod_mixed = glmer(outcome ~ 1 + (1|ID), data = data_dataframe, family = "binomial")
    
    estimate_mixed = summary(mod_mixed)$coefficients[1,1]
    se_mixed = summary(mod_mixed)$coefficients[1,2]
    
    wald_ci_mixed_logit <- c(estimate_mixed - se_mixed* qnorm(1-((Inference_threshold_robustness_NHST)/2)),
                             estimate_mixed + se_mixed* qnorm(1-((Inference_threshold_robustness_NHST)/2)))
    wald_ci_mixed = logit2prob(wald_ci_mixed_logit)
    
    wald_ci_mixed_lb = wald_ci_mixed[1]
    wald_ci_mixed_ub = wald_ci_mixed[2]
    
    CI_lower = wald_ci_mixed_lb
    CI_upper = wald_ci_mixed_ub
    
  } else if(any(Primary_analysis == "binom.test")){
    num_successes = sum(data_dataframe$outcome)
    total_n_of_trials = length(data_dataframe$outcome)
    
    
    CI_lower = binom.test(x = num_successes, n = total_n_of_trials, p = 0.5, conf.level = (1-Inference_threshold_robustness_NHST))$conf.int[1]
    CI_upper = binom.test(x = num_successes, n = total_n_of_trials, p = 0.5, conf.level = (1-Inference_threshold_robustness_NHST))$conf.int[2]
    
    
  }
  
  if(CI_upper < M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
  return(conclusion)
}

simulation_function <- function(erotic_trial_per_person, True_prob, max_num_trials, SD_personal_differences, Primary_analysis, data_dataframe, Inference_threshold_robustness_NHST, M0_prob = M0_prob){
  data_dataframe = data_generator(erotic_trial_per_person = erotic_trial_per_person, True_prob = True_prob, max_num_trials = max_num_trials, SD_personal_differences = SD_personal_differences)
  result = analyser(Primary_analysis = Primary_analysis, data_dataframe = data_dataframe, Inference_threshold_robustness_NHST = Inference_threshold_robustness_NHST, M0_prob = M0_prob)
  return(result)
}





# power to detect 0.5% lower than chance correct guess rate when using binomial test

erotic_trial_per_person = 18
True_prob = 0.495 # true probability of correct guesses in the population
max_num_trials = 110000
SD_personal_differences = 0 # 0.15 would produce some very lucky people but still realistic
Inference_threshold_robustness_NHST = 0.1 # 0.1 means 90% CI, but if it is one sided, this will result in 5% alpha
M0_prob = 0.5 # probability of correct guesses if M0 is true
Primary_analysis = "binom.test" # "binom.test" or "Mixed_NHST"


results_of_simulation = pbreplicate(1000, simulation_function(erotic_trial_per_person = erotic_trial_per_person, True_prob = True_prob, max_num_trials = max_num_trials, SD_personal_differences = SD_personal_differences, Primary_analysis = Primary_analysis, data_dataframe = data_dataframe, Inference_threshold_robustness_NHST = Inference_threshold_robustness_NHST, M0_prob = M0_prob))

M1_detection_rate = sum((results_of_simulation == "M1"))/length(results_of_simulation)
M1_detection_rate # power = 0.952


# false detection rate (alpha error probability) when using binomial test

erotic_trial_per_person = 18
True_prob = 0.5 # true probability of correct guesses in the population
max_num_trials = 110000
SD_personal_differences = 0 # 0.15 would produce some very lucky people but still realistic
Inference_threshold_robustness_NHST = 0.1 # 0.1 means 90% CI, but if it is one sided, this will result in 5% alpha
M0_prob = 0.5 # probability of correct guesses if M0 is true
Primary_analysis = "binom.test" # "binom.test" or "Mixed_NHST"


results_of_simulation = pbreplicate(1000, simulation_function(erotic_trial_per_person = erotic_trial_per_person, True_prob = True_prob, max_num_trials = max_num_trials, SD_personal_differences = SD_personal_differences, Primary_analysis = Primary_analysis, data_dataframe = data_dataframe, Inference_threshold_robustness_NHST = Inference_threshold_robustness_NHST, M0_prob = M0_prob))

M1_detection_rate = sum((results_of_simulation == "M1"))/length(results_of_simulation)
M1_detection_rate  # alpha = 0.036







# power to detect 0.5% lower than chance correct guess rate when using mixed logistic regression

erotic_trial_per_person = 18
True_prob = 0.495 # true probability of correct guesses in the population
max_num_trials = 110000
SD_personal_differences = 0 # 0.15 would produce some very lucky people but still realistic
Inference_threshold_robustness_NHST = 0.1 # 0.1 means 90% CI, but if it is one sided, this will result in 5% alpha
M0_prob = 0.5 # probability of correct guesses if M0 is true
Primary_analysis = "Mixed_NHST" # "binom.test" or "Mixed_NHST"


results_of_simulation = pbreplicate(100, simulation_function(erotic_trial_per_person = erotic_trial_per_person, True_prob = True_prob, max_num_trials = max_num_trials, SD_personal_differences = SD_personal_differences, Primary_analysis = Primary_analysis, data_dataframe = data_dataframe, Inference_threshold_robustness_NHST = Inference_threshold_robustness_NHST, M0_prob = M0_prob))

M1_detection_rate = sum((results_of_simulation == "M1"))/length(results_of_simulation)
M1_detection_rate # power = 0.95


# false detection rate (alpha error probability) when using mixed logistic regression

erotic_trial_per_person = 18
True_prob = 0.5 # true probability of correct guesses in the population
max_num_trials = 110000
SD_personal_differences = 0 # 0.15 would produce some very lucky people but still realistic
Inference_threshold_robustness_NHST = 0.1 # 0.1 means 90% CI, but if it is one sided, this will result in 5% alpha
M0_prob = 0.5 # probability of correct guesses if M0 is true
Primary_analysis = "Mixed_NHST" # "binom.test" or "Mixed_NHST"


results_of_simulation = pbreplicate(100, simulation_function(erotic_trial_per_person = erotic_trial_per_person, True_prob = True_prob, max_num_trials = max_num_trials, SD_personal_differences = SD_personal_differences, Primary_analysis = Primary_analysis, data_dataframe = data_dataframe, Inference_threshold_robustness_NHST = Inference_threshold_robustness_NHST, M0_prob = M0_prob))

M1_detection_rate = sum((results_of_simulation == "M1"))/length(results_of_simulation)
M1_detection_rate  # alpha = ...
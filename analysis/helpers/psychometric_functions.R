library(stats)
library(ggplot2)
library(dplyr)

### Fit a logit function to choices for each individual subject
logit_fit <- function(data){
  model_fit <- glm(data=data, choice_ab ~ vDiff_ab, family = "binomial")
  return(model_fit)
}

plot_logit_fit_individual <- function(data){
  ggplot(data, aes(x=vDiff_ab, y=choice_ab)) + geom_point() +
    stat_smooth(method="glm", color="black", se=FALSE,
                method.args = list(family=binomial)) + 
    theme_classic()
}

logit_fit_estimates <- function(data){
  VDiff <- seq(min(data$vDiff_ab), max(data$vDiff_ab),len=50)
  colnames <- c("subject", "vDiff", "choice_prob", "p", "r", "h")
  logit_estimates_df <- setNames(data.frame(matrix(ncol = length(colnames), nrow = 0)), colnames)
  for(subj in unique(data$subject)){
    for(block_type in unique(data$type)){
      subj_data <- data %>% filter(subject==subj)
      subj_model <- logit_fit(subj_data)
      choice_prob_estimate <- predict(subj_model, VDiff, type="response")  
      tmp_df <- data.frame()
    }
  }
}

plot_logit_fit_stats <- function(data){
  
  
  
  
  ggplot(data, aes(x=vDiff_ab, y=choice_ab)) + geom_point() +
    stat_smooth(method="glm", color="black", se=FALSE,
                method.args = list(family=binomial)) + 
    theme_classic()
}

check_LR_bias <- function(data){
  choice_count_data <- data %>% 
    mutate(choice_LR = ifelse(choice_lr==1, 'L', 'R')) %>%
    group_by(subject, choice_LR) %>%
    summarize(count=n())
  
  ggplot(data=choice_count_data, aes(x=choice_LR, y=count)) + 
    geom_boxplot() +
    geom_point(color="grey", alpha=0.4) + 
    geom_line(aes(group=subject), color="grey", alpha=0.4) + 
    theme_classic()
}











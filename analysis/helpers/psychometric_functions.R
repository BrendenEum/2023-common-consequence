library(stats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

### Fit a logit function to choices for each individual subject
logit_fit <- function(data){
  model_fit <- glm(data=data, choice_ab ~ vDiff_ab, family = "binomial")
  return(model_fit)
}

plot_logit_fit_individual <- function(data){
  fig <- ggplot(data, aes(x=vDiff_ab, y=choice_ab, group=subject, color=type)) + 
    #geom_point(aes(group=subject, color=type)) +
    stat_smooth(aes(group=subject), method="glm", se=FALSE,
                method.args = list(family=binomial)) + 
    xlab("EV(A)-EV(B)") +
    ylab("Choice probability") +
    theme_classic() + 
    facet_grid(cols=vars(type))
  ggsave("../outputs/figures/individual_choice_curves.jpg", plot=fig)
}

logit_fit_params <- function(data){
  colnames <- c("subject", "type", "p", "r", "intercept", "slope", "indifference_point")
  logit_params_df <- setNames(data.frame(matrix(ncol = length(colnames), nrow = 0)), colnames)
  for(subj in unique(data$subject)){
    for(block_type in unique(data$type)){
      subj_data <- data %>% filter(subject==subj, type==block_type)
      subj_model <- logit_fit(subj_data)
      tmp_df <- data.frame(subject=subj, type=block_type, 
                           p=subj_data$p[1],
                           r=subj_data$r[1],
                           intercept=coef(subj_model)[1],
                           slope=coef(subj_model)[2],
                           indifference_point=-coef(subj_model)[1]/coef(subj_model)[2])
      
      logit_params_df <- rbind(logit_params_df, tmp_df)
    }
  }
  return(logit_params_df)
}

logit_fit_estimates <- function(data){
  VDiff_values <- seq(min(data$vDiff_ab), max(data$vDiff_ab),len=100)
  colnames <- c("subject", "type", "p", "r", "vDiff_ab", "choice_prob")
  logit_estimates_df <- setNames(data.frame(matrix(ncol = length(colnames), nrow = 0)), colnames)
  for(subj in unique(data$subject)){
    for(block_type in unique(data$type)){
      subj_data <- data %>% filter(subject==subj, type==block_type)
      subj_model <- logit_fit(subj_data)
      tmp_df <- data.frame(vDiff_ab=VDiff_values)
      tmp_df$choice_prob <- predict(subj_model, tmp_df, type="response")  
      tmp_df$subject <- subj
      tmp_df$type <- block_type
      tmp_df$p <- subj_data$p[1]
      tmp_df$r <- subj_data$r[1]
      
      logit_estimates_df <- rbind(logit_estimates_df, tmp_df)
    }
  }
  return(logit_estimates_df)
}

plot_logit_fit_stats <- function(logit_fits_df, plot_type="individual"){
  if(plot_type=="individual"){
    fig <- ggplot(logit_fits_df, aes(x=vDiff_ab, y=choice_prob)) +
      geom_line(aes(color=type, group=subject)) + 
      geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=0.5) + 
      #ylim(c(0,1)) +
      xlab("EV(A)-EV(B)") +
      ylab("Choice probability") +
      theme_classic() + 
      facet_grid(cols=vars(type))  
    ggsave("../outputs/figures/individual_choice_curves_fitted.jpg", plot=fig)
  }else if(plot_type=="stats_all"){
    logit_fit_stats_df <- logit_fits_df %>% 
      dplyr::group_by(vDiff_ab) %>% 
      dplyr::summarize(choice_prob_mean = mean(choice_prob),
                       choice_prob_sd = sd(choice_prob))
    
    fig <- ggplot(logit_fit_stats_df, aes(x=vDiff_ab, y=choice_prob_mean, 
                                   ymin=choice_prob_mean-choice_prob_sd,
                                   ymax=choice_prob_mean+choice_prob_sd)) +
      geom_line() + 
      geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=0.5) + 
      geom_ribbon(alpha=0.5) +
      #ylim(c(0,1)) +
      xlab("EV(A)-EV(B)") +
      ylab("Choice probability") +
      theme_classic()
    ggsave("../outputs/figures/stats_all_choice_curves_fitted.jpg", plot=fig)
  }else if(plot_type=="stats_type"){
    logit_fit_stats_df <- logit_fits_df %>% 
      dplyr::group_by(vDiff_ab, type) %>% 
      dplyr::summarize(choice_prob_mean = mean(choice_prob),
                       choice_prob_sd = sd(choice_prob))
    
    fig <- ggplot(logit_fit_stats_df, aes(x=vDiff_ab, y=choice_prob_mean, 
                                   ymin=choice_prob_mean-choice_prob_sd,
                                   ymax=choice_prob_mean+choice_prob_sd)) +
      geom_line(aes(color=type)) + 
      geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=0.5) + 
      geom_ribbon(aes(color=type, fill=type), alpha=0.3) +
      #ylim(c(0,1)) +
      xlab("EV(A)-EV(B)") +
      ylab("Choice probability") +
      theme_classic()
      #facet_grid(cols=vars(type))
    ggsave("../outputs/figures/stats_type_choice_curves_fitted.jpg", plot=fig)
  }else if(plot_type=="stats_type_r"){
    logit_fit_stats_df <- logit_fits_df %>% #filter(type=="ApBp") %>%
      dplyr::group_by(vDiff_ab, r, type) %>% 
      dplyr::summarize(choice_prob_mean = mean(choice_prob),
                       choice_prob_sd = sd(choice_prob))

    fig <- ggplot(logit_fit_stats_df, aes(x=vDiff_ab, y=choice_prob_mean, 
                                   ymin=choice_prob_mean-choice_prob_sd,
                                   ymax=choice_prob_mean+choice_prob_sd)) +
      geom_line(aes(color=factor(r))) + 
      geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=0.5) + 
      geom_ribbon(aes(color=factor(r), fill=factor(r)), alpha=0.3) +
      #ylim(c(0,1)) +
      xlab("EV(A)-EV(B)") +
      ylab("Choice probability") +
      theme_classic() + 
      facet_grid(cols=vars(type))
    ggsave("../outputs/figures/stats_type_r_choice_curves_fitted.jpg", plot=fig)
  }
  
}

extract_CC_effects <- function(data){
  AB_data <- data %>% filter(type=="AB") %>% 
    dplyr::select(c(subject, p, r, choice_ab, H)) %>%
    rename(choice_AB=choice_ab)
  ApBp_data <- data %>% filter(type=="ApBp") %>% 
    dplyr::select(c(subject, p, r, choice_ab, H)) %>%
    rename(choice_ApBp=choice_ab)
  merged_df <- merge(x=AB_data, y=ApBp_data, by=c("subject", "p", "r", "H"))
  CCE_minus_RCCE <- c()
  for(idx in seq(1,nrow(merged_df))){
    if(merged_df$choice_AB[idx]==1){
      if(merged_df$choice_ApBp[idx]==0){
        CCE_minus_RCCE[idx] <- 1  
      }else{
        CCE_minus_RCCE[idx] <- 0 
      }
    }else{
      if(merged_df$choice_ApBp[idx]==0){
        CCE_minus_RCCE[idx] <- 0  
      }else{
        CCE_minus_RCCE[idx] <- -1 
      }
    }
  }
  merged_df$CCE_minus_RCCE <- CCE_minus_RCCE
  return(merged_df)
}

check_LR_bias <- function(data){
  choice_count_data <- data %>% 
    mutate(choice_LR = ifelse(choice_lr==1, 'L', 'R')) %>%
    group_by(subject, choice_LR) %>%
    summarize(count=n())
  
  fig <- ggplot(data=choice_count_data, aes(x=choice_LR, y=count)) + 
    geom_boxplot() +
    geom_point(color="grey", alpha=0.4) + 
    geom_line(aes(group=subject), color="grey", alpha=0.4) + 
    theme_classic()
  ggsave("../outputs/figures/LR_bias.jpg", plot=fig)
}

plot_CCE_CCP_dist <- function(data){
  plot_CCP <- ggplot(data %>% filter(CCP<100, CCP>-100), aes(x=CCP)) + 
    geom_histogram() + 
    xlab("CCP") +
    ylab("Count") +
    theme_classic()
  
  plot_CCE <- ggplot(data, aes(x=CCE_RCCE_percent)) + 
    geom_histogram() +
    xlab("CCE - RCCE") +
    ylab("Count") +
    theme_classic()
  
  fig <- grid.arrange(plot_CCP, plot_CCE, ncol=2)
  ggsave("../outputs/figures/CCP_CCE_histograms.jpg", plot=fig)
}


all.data <- read.csv("../../data/processed_data/pilotdata.csv")

plot_logit_fit_individual(all.data)

logit_estimates_df <- logit_fit_estimates(all.data)
plot_logit_fit_stats(logit_estimates_df, plot_type="individual")
plot_logit_fit_stats(logit_estimates_df, plot_type="stats_all")
plot_logit_fit_stats(logit_estimates_df, plot_type="stats_type")
plot_logit_fit_stats(logit_estimates_df, plot_type="stats_type_r")

## Look for CCE and CCP
logit_params_df <- logit_fit_params(all.data)
CCE_CCP_df <- logit_params_df %>% select(-c("intercept", "slope")) %>% 
  tidyr::spread(type, indifference_point) %>% rename(h_AB=AB, h_ApBp=ApBp) %>%
  mutate(CCP=h_AB-h_ApBp)

CCE_df <- extract_CC_effects(all.data)
tmp <- CCE_df %>% 
  dplyr::group_by(subject) %>%
  dplyr::summarize(n_trials=n(),
                   CCE_RCCE_total=sum(CCE_minus_RCCE),
                   CCE_RCCE_percent=sum(CCE_minus_RCCE)/n_trials*100)
CCE_CCP_df <- merge(CCE_CCP_df, tmp, by="subject")

plot_CCE_CCP_dist(CCE_CCP_df)








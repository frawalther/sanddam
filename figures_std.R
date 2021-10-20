library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(bayesplot)
library(matrixStats)
library(rethinking)
library(data.table)
library(reshape2)

library(performance)
library(Metrics)

# Model Diagnostics
  #Check Rhat 
  rhat_GP2_int <- rhat(fit_GP_unp_int)
  mcmc_rhat_hist(rhat_GP2_int)
  
  rhat <- as.data.frame(rhat_GP2_int)
  rhat$names <- rownames(rhat)
  #check where Rhat is > 1.01
  rhat[( rhat_GP2_int > 1.01),]

  #ESS: Check n_eff/N ratio (ratio of effective sample size (n_eff) to total sample size (N))
  
  ratios_GP2int <- neff_ratio(fit_GP_unp_int)
  mcmc_neff(ratios_GP2int)
  
  neff <- as.data.frame(ratios_GP2int)
  neff$names <- rownames(neff)
  #check if and where neff is < 0.1
  neff[( ratios_GP2int < 0.1),]

  fit_summary <- summary(fit_GP_unp_int)
  print(names(fit_summary))
  print(fit_summary$summary)

# Sampler diagnostics 
  sampler_params <- get_sampler_params(fit_GP_unp_int, inc_warmup = FALSE)
  sampler_params_chain1 <- sampler_params[[1]]
  colnames(sampler_params_chain1)
  
    #average value of acceptance probabilities of all possible samples by chain 
  mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
  print(mean_accept_stat_by_chain)
  
    # maximum value of treedepth for each chain 
  max_treedepth_by_chain <- sapply(sampler_params, function(x) max(x[, "treedepth__"]))
  print(max_treedepth_by_chain)

  
# Draw posterior samples 
post_evi <- as.matrix(fit_GP_unp_int, pars= "evi_pred")
head(post_evi)
nrow(post_evi)
ncol(post_evi)
    #   try later again to calculate r2 / rmse on full data (for all 8000 draws)
    # post_evilong <- melt(post_evi) 
    # post_evilong$X <- stringr::str_extract_all(post_evilong[[2]], "[[:digit:]]+")
    # cbind df_s$X and df_s$evi_obs with evilong df
    # head(post_evilong)
    # nrow(post_evilong)
    # #R-squared
    # cor(post_evilong$value, post_evilong$evi_obs)^2
    # #RMSE
    # sqrt(mean((post_evilong$value - post_evilong$evi_obs)^2))
    # rmse(post_evilong$evi_obs, post_evilong$value)

#create data table with median predicted evi for each observed evi
post_e <- data.table(evi_pred = colMedians(post_evi), sd = apply(post_evi, 2, sd), #ci_89
          lower = apply(post_evi, 2, quantile, 0.055), 
          upper = apply(post_evi, 2, quantile, 0.955), 
          evi_obs = standat$evi)
head(post_e)
nrow(post_e)
ncol(post_e)
save(post_e, file = "post_e.RData")
load("~/01Master/MasterThesis/Pius/R/sand dam/post_e.RData")

#SLOPES:
        post_s <- as.data.frame(fit_GP_unp_int, pars = c("b1", "b2", "b3[1]", "b3[2]", "b4")) 
        
        save(post_s, file = "post_s.RData")
        load("~/01Master/MasterThesis/Pius/R/sand dam/post_s.RData")
        nrow(post_s)
        ncol(post_s)
        head(post_s)
        
        #vizualise slopes:
          #b1 Precipitation, b2 sand dam presence, b4 P:SD 
          mcmc_areas(post_s, pars=c("b1", "b2", "b4"), 
                                  prob = .89)
            
            #b3: Land cover - [1] shrubs, [2] cropland
          mcmc_areas(post_s, regex_pars = "b3", prob=0.89)
        
        # Calculate median + credible intervals (ci 89%)
          median(post_s$b1)
          quantile(post_s$b1, probs = c(0.055, 0.945)) #ci_89
          
          median(post_s$b2)
          quantile(post_s$b2, probs = c(0.055, 0.945)) #ci_89
          
          median(post_s$b4)
          quantile(post_s$b4, probs = c(0.055, 0.945)) #ci_89
          
          median(post_s$`b3[1]`)
          quantile(post_s$`b3[1]`, probs = c(0.055, 0.945)) #ci_89
          
          median(post_s$`b3[2]`)
          quantile(post_s$`b3[2]`, probs = c(0.055, 0.945)) #ci_89


# Posterior predicitve check: PPC distribution - comparison of empirical distribution (y) 
                                  #to simulated posterior predicitve distributions (yrep)
ppc_dens_overlay(y = post_e$evi_obs, #observed
                   yrep = post_evi) #predicted


# Model performance: Crossvalidation 
# plot observed (y) vs. predictved (x) EVI  (Pineiro et al., 2008)
ggplot(post_e) +
  labs(x= "Predicted EVI", y="Observed EVI", title="Model performance") +
  geom_errorbarh(aes(y=evi_obs, xmin = lower, xmax=upper), color="darkgrey", width=5) + #CI = 89% #alpha = 0.4, 
 # geom_ribbon(aes(ymin = lower, ymax=upper, x=evi_obs, fill="lightblue"), alpha =0.3) +
  geom_point(aes(x=evi_pred, y=evi_obs),alpha = 1/5) + 
  geom_abline(intercept = 0, slope = 1, color="blue") +
  theme_bw() 

#frequentist measures
  #R-squared
  cor(post_e$evi_pred, post_e$evi_obs)^2
  #RMSE
  sqrt(mean((post_e$evi_obs - post_e$evi_pred)^2)) #or
  rmse(post_e$evi_obs, post_e$evi_pred)

# r2 loo [under construction]
    r2_loo(fit_GP_unp_int, ci=89, robust = TRUE)
    r2_bayes(fit_GP_unp_int)
    r2(fit_GP_unp_int)
    # Error in UseMethod("r2_loo_posterior") : 
    #   no applicable method for 'r2_loo_posterior' applied to an object of class "stanfit"


load("~/01Master/MasterThesis/Pius/R/sand dam/df_seas_all.RData")
df_s <- df_s %>%
  filter(!is.na(presence))

post_dt <- post_e
post_dt$P <- df_s$P # unit[mm]
post_dt$presence <- standat$presence #0,1
post_dt$lc_class <-df_s$class #shrubs, cropland 
post_dt$time <- standat$time #timesteps [96]
post_dt$gp_id <- standat$gp_id
post_dt$season <- df_s$season
post_dt$area <- df_s$area_m2

head(post_dt)


#EVI and Precipitation 
  ggplot(post_dt, aes(x=P, y=evi_pred)) + #group=presence, color=factor(presence)
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey", alpha=0.1) +
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(presence)) +
    labs(x="Precipitation [mm]", y="EVI (Predicted Median)", title="EVI and Precipitation") +
    theme_bw() +
    geom_hline
  
  #grouped by sand dam presence
  post_dt %>%
    group_by(presence) %>%
    mutate(w_med = weighted.mean(evi_pred, area)) %>%
    ggplot(aes(x=P, y=evi_pred)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey", alpha=0.1) +
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(presence)) +
    labs(x="Precipitation [mm]", y="EVI (Predicted Median)", title="EVI and Precipitation") +
    theme_bw() +
    geom_hline(aes(yintercept=w_med, group = presence), colour="red")

  #grouped by land cover class 
  post_dt %>%
    group_by(lc_class) %>%
    mutate(w_med = weighted.mean(evi_pred, area)) %>% #"averaged median" weighted by area <- way to go?
    ggplot(aes(x=P, y=evi_pred)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey", alpha=0.1) +
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(lc_class)) +
    labs(x="Precipitation [mm]", y="EVI (Predicted Median)", title="EVI and Precipitation") +
    theme_bw() +
    geom_hline(aes(yintercept=w_med, group = presence), colour="red")

  
### VISUALIZE TIME SERIES ###
#predicted EVI
#one GP example (GP_id== )
plot <- post_dt %>% #plot1
  #     filter (gp_id==1) %>%
  group_by(!!!syms(c("time", "sd_id"))) %>%
  #     group_by(time) %>%
  summarize("mean_evi" = weighted.mean(evi_est, area),
            "evi_Q25" = quantile(evi_est, probs = 0.25),
            "evi_Q75" = quantile(evi_est, probs = 0.75),
            "evi_Q05" = quantile(evi_est, probs = 0.05),
            "evi_Q95" = quantile(evi_est, probs = 0.95)) %>%
  ggplot(aes(x=time, y=mean_evi)) + 
  #        geom_ribbon(aes(ymin = evi_Q05, ymax=evi_Q95, fill="lightblue"), alpha =0.3) +
  #        geom_ribbon(aes(ymin = evi_Q25, ymax=evi_Q75, fill="yellow"), alpha = 0.4) +
  geom_point(aes(x=time, y=mean_evi, group=sd_id, color=sd_id)) +
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021")) 

#Precipitation (observed) 
prec <- df_s %>%
  group_by(!!!syms(c("time_mean", "ID"))) %>%
  summarize("mean_P" = weighted.mean(P, area_m2),
            "time" = weighted.mean(time_mean, area_m2))  %>% #, "
  ggplot(aes(x=time, y = mean_P)) + 
  #geom_point() +  #stat = "identity", color="blue", fill="blue", width = 0.5
  geom_point(aes(x=time, y=mean_P, group=ID, color=ID)) +
  labs(y = "Monthly Precipitation [mm]") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))
grid.arrange(plot, prec)

# plot <- yrep_t_long %>%
#    group_by(c("season", "lc_id") %>%
#    # summarize("mean_evi" = mean(evi_est),
#    #           "evi_Q25" = quantile(evi_est, probs = 0.25),
#    #           "evi_Q75" = quantile(evi_est, probs = 0.75),
#    #           "evi_Q05" = quantile(evi_est, probs = 0.05),
#    #           "evi_Q95" = quantile(evi_est, probs = 0.95)) %>%
#    ggplot(aes(x=time, y=evi_est)) + 
#    #   geom_ribbon(aes(ymin = evi_Q05, ymax=evi_Q95, fill="lightblue"), alpha =0.3) +
#    #   geom_ribbon(aes(ymin = evi_Q25, ymax=evi_Q75, fill="yellow"), alpha = 0.4) +
#    geom_point(aes(x=time, y=evi_est, color="blue")) +
#    scale_x_continuous("Time", 
#                       breaks = seq(0, 92, by=12),
#                       labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))
# 

epred <- yrep_t_long %>%
  filter (lc_sd == 5) %>%
  group_by(time) %>%
  summarize("mean_evi" = mean(evi_est)) %>%
  ggplot(aes(x=time, y=mean_evi)) + geom_point() + geom_line() +
  labs(y = "EVI", title="EVI prediction (lc_sd == 5)") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))

eobs <- df_s %>%
  filter (X == 5) %>% #messed up numbers
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() +
  labs(y = "EVI", title="EVI observed (lc_sd == 5)") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))

grid.arrange(epred, eobs)



group_cols <- c("presence", "lc_sd", "time")  #seas_y (df_s)?
yrep_t_long %>% 
  group_by(!!!syms(group_cols)) %>% 
  summarize("mean_evi" = weighted.mean(evi_est, area)) %>%
  ggplot(aes(y=mean_evi, x=time, colour=factor(presence))) + geom_point() +
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))


# TIME 

#???

# PLOT: SEASON 

#mean, global mean 
mean_evi_pred <- stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area)
mean_evi_obs <- stats::weighted.mean(df_s$evi, df_s$area_m2)
mean_lc_pred <- yrep_t_long %>%
  group_by(lc_class) %>%
  summarise(stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area))
mean_lc_obs <- df_s %>%
  group_by(class) %>%
  summarise(stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area))

#0.228

#observed
obs_seas <- df_s %>%
  group_by(!!!syms(c("X", "class", "season", "presence"))) %>%
  summarise("evi_mean_obs" = weighted.mean(evi, area_m2)) %>%
  ggplot(aes(y=evi_mean_obs, x=season, colour=factor(presence))) + 
  geom_boxplot() +
  labs(x="Seasons", y="Observed EVI", title="Observed data", colour = " ") +
  scale_color_manual(labels = c("No sand dam", "Sand dam"),
                     values = c("darkgrey", "brown")) +
  facet_grid(cols = vars(class)) #+
#   geom_hline(yintercept=mean_lc_pred$`stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area)`, linetype=2, size=1/4)

#predicted
pred_seas <- yrep_t_long %>%
  group_by(!!!syms(c("lc_sd", "season", "lc_class", "presence"))) %>% #"rn" to include all samples - no averaging 
  summarise("evi_pred_mean" =weighted.mean(evi_est, area),
            "evi_Q055" = quantile(evi_est, probs = 0.055),
            "evi_Q955" = quantile(evi_est, probs = 0.955)) %>%
  ggplot(aes(y=evi_pred_mean, x=season, colour=factor(presence))) +
  geom_boxplot() +
  labs(x="Seasons", y="Predicted EVI", title="Predicted data", colour = " ") +
  scale_color_manual(labels = c("No sand dam", "Sand dam"),
                     values = c("lightblue", "blue")) +
  facet_grid(cols = vars(lc_class)) 
  #    geom_hline(yintercept=mean_lc_pred$`stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area)`, linetype=2, size=1/4) +
  #    geom_ribbon(aes(ymin = evi_Q055, ymax=evi_Q955, fill="lightblue"), alpha =0.3) 
  grid.arrange(obs_seas, pred_seas)

  pred_seas$data
#lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles) == 50% CI
ggplot(yrep_t_long) + geom_boxplot(aes(y=evi_est, x=season)) #, colour=factor(presence)))


#######
p_lcover <- yrep_t_long %>%
  group_by(lc_class) %>%
  summarise("evi_pred_mean" =weighted.mean(evi_est, area),
            "evi_Q055" = quantile(evi_est, probs = 0.055),
            "evi_Q955" = quantile(evi_est, probs = 0.955),
  ) %>%
  ggplot() +
  geom_point(aes(x=lc_class, y=evi_pred_mean)) +
  labs(x="lc_class", y="Predicted EVI", title="Land cover") +
  geom_errorbar(aes(x=lc_class, ymin = evi_Q055, ymax=evi_Q955), alpha =0.3) 

p_sd <- yrep_t_long %>%
  group_by(presence) %>%
  summarise("evi_pred_mean" =weighted.mean(evi_est, area),
            "evi_Q055" = quantile(evi_est, probs = 0.055),
            "evi_Q955" = quantile(evi_est, probs = 0.955),
            "evi_Q25" = quantile(evi_est, probs = 0.25),
            "evi_Q75" = quantile(evi_est, probs = 0.75)) %>%
  ggplot() +
  labs(x="Sand dam presence", y="Predicted EVI", title="Sand dam presence") +
  geom_errorbar(aes(x=factor(presence), ymin = evi_Q055, ymax=evi_Q955)) +
  geom_errorbar(aes(x=factor(presence), ymin = evi_Q25, ymax=evi_Q75), color="darkgrey") +
  geom_point(aes(x=factor(presence), y=evi_pred_mean), size=4, shape=21, fill="white")


yrep_t_long %>%
  group_by(presence) %>%
  summarise("var" = var(evi_est),
            "sd" = sd(evi_est))


#PLOT: 

# Predicted mean EVI per GP 

p_pred1 <- yrep_t_long %>%
  group_by(lc_sd) %>%
  summarise("GP_mean_evi" = stats::weighted.mean(evi_est, area)) %>%
  ggplot(aes(y=GP_mean_evi, x=lc_sd)) + geom_point() +
  labs(x="GP (LC/SD)", y="Predicted mean EVI", title="Predicted mean EVI per GP (n=336)") +
  geom_hline(yintercept=mean_evi_pred, linetype = 2, size = 1/4) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))

# Histogram Predicted mean EVI
p_pred2 <- yrep_t_long %>%
  group_by(!!!syms(c("lc_sd", "presence"))) %>%
  summarize("GP_mean_evi" = stats::weighted.mean(evi_est, area),
            "evi_Q055" = quantile(evi_est, probs = 0.055),
            "evi_Q955" = quantile(evi_est, probs = 0.955)) %>% #89%
  ggplot(aes(x=GP_mean_evi, fill=factor(presence))) + #fill=factor(presence), 
  geom_histogram(alpha = 0.3, position = "identity") + 
  scale_fill_discrete(labels = c("No sand dam", "Sand dam")) +
  labs(x="Predicted mean EVI", y="counts", title="Distribution of predicted mean EVI (n=336 GPs)", fill = " ") +
  geom_vline(xintercept=0.21, linetype = 2, size = 1/4, colour = "#F8766D") +
  geom_vline(xintercept=0.23, linetype = 2, size = 1/4, colour = "#00BFC4") +
  
  labels = scales::number_format(accuracy = 0.01)) +
  
  
  #      #    # summarize("mean_evi" = mean(evi_est),
  #    #           "evi_Q25" = quantile(evi_est, probs = 0.25),
  #    #           "evi_Q75" = quantile(evi_est, probs = 0.75),
  #    #           "evi_Q05" = quantile(evi_est, probs = 0.05),
  #    #           "evi_Q95" = quantile(evi_est, probs = 0.95)) %>%
  
  #calculate weighted means 
  yrep_t_long %>%
  filter(presence == 0) %>%
  summarise(stats::weighted.mean(evi_est, area))
#0.21
yrep_t_long %>%
  filter(presence == 1) %>%
  summarise(stats::weighted.mean(evi_est, area))
#0.23

grid.arrange(p_pred1, p_pred2, nrow= 1)

#comments: add observational data
#          add credible intervals 
#   p_obs <- 
df_s %>%
  group_by(!!!syms(c("X", "presence"))) %>%
  summarize("X_mean_evi" = stats::weighted.mean(evi, area_m2)) %>% #mean() would be enough; area the same
  ggplot(aes(x=X_mean_evi,fill=factor(presence))) + #fill=factor(presence) 
  geom_histogram(alpha = 0.5, position = "identity") +
  labs(x="Observed mean EVI", y="counts", title="Distribution of observed EVI (n(X)=336)", fill= " ") +
  geom_vline(xintercept=mean_evi_obs, linetype = 2, size = 1/4) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_discrete(labels = c("No sand dam", "Sand dam")) 


grid.arrange(p_pred2, p_obs)

#SHRINKAGE?


### outer (?) cross-validation ###

cv_pred <- yrep_t_long %>%
  group_by(!!!syms(c("lc_sd", "lc_class"))) %>% #"time", , "presence"
  summarise(evi_pred=weighted.mean(evi_est, area))

cv_obs <- df_s %>%
  group_by(!!!syms(c("X", "class"))) %>% #"time_mean", , "presence"
  summarise(evi_obs = weighted.mean(evi, area_m2))

nrow(cv_pred)
nrow(cv_obs)
cv_obs <- as.data.frame(cv_obs)
cv_pred <- as.data.frame(cv_pred)

cv <- cv_obs
cv$evi_pred <- cv_pred$evi_pred

lm_eqn <- function(cv){
  m <- lm(evi_obs ~ evi_pred, cv);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p <- ggplot(data = cv, aes(x = evi_pred, y = evi_obs)) +
  geom_point(alpha = 1/5) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(x = 0.2, y = 0.32, label = lm_eqn(cv), parse = TRUE) +
  #coord_cartesian(xlim= c(0,0.45),ylim=c(0,0.45))
  labs(x="Predicted EVI", y="Observed EVI", title="Cross-Validation")

#multiple cv? grouped by lc or presence? or not necessary? 
#group by time or not? (by time would mean way more data points)
#outer or inner CV?


#Land cover class:

yrep_t_long %>%
  filter(lc_class == "cropland") %>%
  summarise(stats::weighted.mean(evi_est, area))
#0.23
yrep_t_long %>%
  filter(lc_class == "shrubs") %>%
  summarise(stats::weighted.mean(evi_est, area))
#0.21

############################
library(lattice)
?lattice::levelplot
mcmc_areas
mcmc_areas(posterior, pars = c("eta[1]", "eta[2]", "eta[3]", "eta[4]"))
mcmc_areas(GP_int, pars = "evi_pred[1]")

mcmc_areas(GP_int, regex_pars = "[1]")

####### vizualize input data (not predicted but preprocessed through hyfrological pre-analysis)
ggplot(df_s)

obs1 <- df_s %>%
  filter (X == 1) %>%
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() +
  labs(y = "EVI") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))

# PLOTTING AROUND: DATA VISUALIZATION (EVI, Precipitation)
#für x == 1
e1 <- df_s %>%
  filter (X == 1) %>%
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() +
  labs(y = "EVI") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))


p1 <- df_s %>%
  filter (X == 1) %>%
  ggplot(aes(x=time_mean, y =P))+geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5) +
  labs(y = "Precipitation [mm]") + 
  scale_x_continuous("Time", 
                     breaks = seq(0, 92, by=12),
                     labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))

grid.arrange(e1, p1)

df_s %>%
  ggplot(aes(x=P, y = evi,colour = factor(presence))) + geom_smooth()

#0= NO sand dam
#1= sand dam
# check again geom_smooth -> alernative 

df_s %>%
  ggplot(aes(y=P, x=season)) + geom_point()

####
#evi vs prec 
p_meanmonth <- yrep_t_long %>%
  group_by(!!!syms(c("lc_sd", "presence"))) %>%
  summarize("evi_mean" = mean(evi_est),
            "p_mean" = mean(P)) %>%
  ggplot(aes(x=p_mean, y=evi_mean, colour=factor(presence))) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x="average monthly precipitation [mm]", y="predicted mean EVI", title="EVI vs. Precipitation (per SDIA)", colour = "Sand dam presence")

p_meanmonth2 <- yrep_t_long %>%
  group_by(!!!syms(c("sd_id", "presence"))) %>%
  summarize("evi_mean" = weighted.mean(evi_est, area),
            "p_mean" = weighted.mean(P, area)) %>%
  ggplot(aes(x=p_mean, y=evi_mean, colour=factor(presence))) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x="average monthly precipitation [mm]", y="predicted mean EVI", title="EVI vs. Precipitation (per SD)", colour = "Sand dam presence")

p_seasons <- yrep_t_long %>%
  group_by(!!!syms(c("sd_id", "presence", "season"))) %>%
  summarize("evi_mean" = weighted.mean(evi_est, area),
            "p_mean" = mean(P)) %>%
  ggplot(aes(x=p_mean, y=evi_mean, colour=factor(presence))) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x="average monthly precipitation [mm]", y="predicted mean EVI", 
       title="EVI vs. Precipitation (per SD)", colour = " ") +
  facet_grid(cols = vars(season), scales="free") + #+ scales="free" 
  #geom_text(x = 0.2, y = 0.32, label = eq(cv_p), parse = TRUE)
  scale_color_manual(labels = c("No sand dam", "Sand dam"),
                     values = c("#F8766D", "#00BFC4"))

# cv_p <- yrep_t_long %>%
#    group_by(!!!syms(c("sd_id", "presence", "season"))) %>%
#    summarize("evi_mean" = weighted.mean(evi_est, area),
#              "p_mean" = weighted.mean(P, area))
# 
# eq <- function(cv_p){
#    m <- lm(evi_mean ~ p_mean, cv_p); #create input data (here=cv)
#    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                     list(a = format(unname(coef(m)[1]), digits = 2),
#                          b = format(unname(coef(m)[2]), digits = 2),
#                          r2 = format(summary(m)$r.squared, digits = 3)))
#    as.character(as.expression(eq));
# }



#####
#add over all or mcmc_area

df_s %>%
  ggplot(aes(x=evi, colour = factor(presence))) + geom_histogram()

df_s%>%
  ggplot(aes(y=evi, x=factor(presence))) + geom_boxplot()

e2 <- df_s %>%
  filter (X == 2) %>%
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() 

p2 <- df_s %>%
  filter (X == 2) %>%
  ggplot(aes(x=time_mean, y =P))+geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5)

grid.arrange(e2, p2)

e3 <- df_s %>%
  filter (X == 3) %>%
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() 

p3 <- df_s %>%
  filter (X == 3) %>%
  ggplot(aes(x=time_mean, y =P))+geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5)

grid.arrange(e3, p3)

ggplot(df_s, aes(x=time_mean, y=evi, color=season)) + geom_point() + geom_smooth(method=lm)

ggplot(df_s, aes(x = time_mean, y = evi, color = X)) + geom_point() +
  geom_line()

ggplot(df_s, aes(x=time_mean, y=P)) + geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5) 
#Warning message:
#position_stack requires non-overlapping x intervals 


#observed values
#EVI
# > min(df_s$evi)
# [1] 0.0176544
# > max(df_s$evi)
# [1] 0.5875966
# > stats::weighted.mean(df_s$evi, df_s$area_m2)
# [1] 0.227434

#Precipitation:
# > max(df_s$P)
# [1] 416.0772
# >  min(df_s$P)
# [1] 0
# > stats::weighted.mean(df_s$P, df_s$area_m2)
# [1] 65.84407

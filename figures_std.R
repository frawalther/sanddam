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
library(ggrepel)
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
          upper = apply(post_evi, 2, quantile, 0.945), 
          evi_obs = standat$evi)
head(post_e)
nrow(post_e)
ncol(post_e)
save(post_e, file = "post_e.RData")
load("~/01Master/MasterThesis/Pius/R/sand dam/post_e.RData")

#SLOPES:
        post_s <- as.matrix(fit_GP_unp_int, pars = c("b1", "b2", "b3[1]", "b3[2]", "b4")) 
        
        save(post_s, file = "post_s.RData")
        load("~/01Master/MasterThesis/Pius/R/sand dam/post_s.RData")
        nrow(post_s)
        ncol(post_s)
        head(post_s)
        post_sum <- data.table(median = colMedians(post_s), sd = apply(post_s, 2, sd), #ci_89
                               lower = apply(post_s, 2, quantile, 0.055), 
                               upper = apply(post_s, 2, quantile, 0.945))
        post_sum
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
post_dt$year <- df_s$year

head(post_dt)
head(df_s)


### EVI and Precipitation ###
  ggplot(post_dt, aes(x=P, y=evi_pred)) + #group=presence, color=factor(presence)
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey") + #, alpha=0.1
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(presence)) +
    labs(x="Precipitation [mm]", y="EVI (Predicted Median)", title="EVI and Precipitation") +
    theme_bw() 
  
  #grouped by sand dam presence
  post_dt %>%
    group_by(presence) %>%
    mutate(w_med = weighted.Median(evi_pred, area), #weighted by areas of SDiAs
           m_med = median(evi_pred)) %>% 
    ggplot(aes(x=P, y=evi_pred)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey") + #, alpha=0.1
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(presence)) +
    labs(x="Precipitation [mm]", y="EVI estimates", title="EVI and Precipitation") +
    theme_bw() +
    geom_hline(aes(yintercept=m_med, group = presence), color="red") +      #, linetype="dashed"
    geom_hline(aes(yintercept=w_med, group = presence), color="blue", linetype="dashed") +
    annotate("text", x = 325, y = 0, label = "Weighted Global Median", color="blue") +
    annotate("text", x = 370, y = -0.02, label = "Global Median", color="red")
    # geom_hline(aes(yintercept=Q055, group = presence), color="red", linetype=j) +
    # geom_hline(aes(yintercept=Q945, group = presence), color="red", linetype=j)

  #grouped by land cover class 
  post_dt %>%
    group_by(lc_class) %>%
    mutate(w_med = weightedMedian(evi_pred, area),
           m_med = median(evi_pred)) %>% #"averaged median" weighted by area <- way to go?
    ggplot(aes(x=P, y=evi_pred)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), color="grey") + #, alpha=0.1
    geom_point(alpha = 0.4) +
    facet_grid(cols = vars(lc_class)) +
    labs(x="Precipitation [mm]", y="EVI estimates", title="EVI and Precipitation") +
    theme_bw() +
    geom_hline(aes(yintercept=w_med, group = lc_class), color="blue", linetype="dashed") +
    # geom_hline(aes(yintercept=m_med, group = lc_class), color="red", linetype="dashed") + 
    annotate("text", x = 325, y = 0, label = "Weighted Global Median", color="blue") #+
    # annotate("text", x = 370, y = -0.03, label = "Global Median", color="red")
    #check differences between: w_med, m_med
    # problem: dashed lines overlay 
  

#SEASONALITY
  # post_dt %>%
  #   group_by(!!!syms(c("season", "presence"))) %>%
  #   mutate(m_med = mean(evi_pred),
  #          w_med = weighted.mean(evi_pred, area)) %>%
  #   ggplot(aes(x=P, y=evi_pred, color=factor(lc_class))) +
  #   geom_point(alpha=0.25) +
  #   facet_grid(cols=vars(presence), rows = vars(season)) +# ,scales="free"
  #   geom_errorbar(aes(ymin=lower, ymax=upper, color=factor(lc_class)), alpha=0.15) +
  #   labs(x="Precipitation [mm]", y="EVI esitamtes", title="Seasonality") + #change title 
  #   # geom_hline(aes(yintercept=m_med, group = presence), colour="black") + #check differences between: w_med, m_med
  #   # geom_hline(aes(yintercept=w_med, group = presence), colour="black", linetype="dashed") + 
  #   theme_bw() +
  #   scale_color_manual(name="Land cover",labels=c("Cropland", "Shrubs"), values=c("#D55E00", "#009E73"))

  
  pres.labs <- c("0" = "Sand dam absent (0)",
                 "1" = "Sand dam present (1)")

  post_dt %>%
    group_by(!!!syms(c("season", "presence"))) %>%
    mutate(m_med = median(evi_pred),
           w_med = weightedMedian(evi_pred, area)) %>%
    ggplot(aes(x=P, y=evi_pred, color=factor(lc_class))) +
    geom_point(alpha=0.25) +
    facet_grid(season ~ presence, 
               labeller=labeller(presence = pres.labs)) +# ,scales="free" #labeller=pres.labs
    geom_errorbar(aes(ymin=lower, ymax=upper, color=factor(lc_class)), alpha=0.15) +
    labs(x="Precipitation [mm]", y="EVI estimates", title="a") +  
    # geom_hline(aes(yintercept=w_med, group = presence), colour="blue", linetype="dashed") +
    # geom_hline(aes(yintercept=m_med, group = lc_class), color="red", linetype="dashed") +
    theme_bw() +
    scale_color_manual(name="Land cover",labels=c("Cropland", "Shrubs"), values=c("#D55E00", "#009E73")) 
  
  post_dt %>%
    group_by(!!!syms(c("season", "presence", "lc_class"))) %>%
    mutate(w_evi = weightedMedian(evi_pred, area),
           w_lower = weightedMedian(lower, area),
           w_upper = weightedMedian(upper, area)) %>%
    ggplot() +    
    facet_grid(cols=vars(presence), rows=vars(season), labeller=labeller(presence = pres.labs)) +
    geom_errorbar(aes(x=lc_class, ymin=w_lower, ymax=w_upper, color=factor(lc_class), width=0.5)) +
    scale_color_manual(name="",labels=c("Cropland", "Shrubs"), values=c("#D55E00", "#009E73")) +
    geom_point(aes(x=lc_class, y=w_evi, color=factor(lc_class)), size=3) +
    theme_bw() +
    labs(x=" ", y="EVI", title="b")  
  # 
  # post_dt %>%
  #   group_by(!!!syms(c("season", "presence", "lc_class"))) %>%
  #   mutate(med_evi = mean(evi_pred),
  #          med_lower = mean(lower),
  #          med_upper = mean(upper)) %>%
  #   ggplot() +    
  #   facet_grid(cols=vars(presence), rows=vars(season), labeller=labeller(presence = pres.labs)) +
  #   geom_errorbar(aes(x=lc_class, ymin=med_lower, ymax=med_upper, color=factor(lc_class), width=0.5)) +
  #   scale_color_manual(name="",labels=c("Cropland", "Shrubs"), values=c("#D55E00", "#009E73")) +
  #   geom_point(aes(x=lc_class, y=med_evi, color=factor(lc_class)), size=3) +
  #  theme_bw() +
  #   labs(x="Land cover", y="EVI", title="Seasonality") #+

    
#Precipitation over seasons 
   post_dt %>%
    group_by(season) %>%
    mutate(w_P_med = weightedMedian(P, area),
           Pmed = median(P),
           lower_p = quantile(P, probs = 0.055),
           upper_p = quantile(P, probs = 0.945),
           lower_pw = weightedMedian(lower_p, area),
           upper_pw = weightedMedian(upper_p, area))%>%
    ggplot() +    
    facet_grid(rows=vars(season)) +#, labeller=labeller(presence = pres.labs)) + #rows=vars(season), 
    #  facet_grid(cols=vars(presence), labeller=labeller(presence = pres.labs)) + #rows=vars(season), 
    geom_errorbar(aes(y=1, xmin=lower_pw, xmax=upper_pw)) + 
    geom_point(aes(x=w_P_med, y=1), size=3) +
    theme_bw() +
    labs(x="Precipitation [mm]", y="season", title="c") +
    scale_y_discrete(breaks = NULL) +
    ylab(NULL)
  
  # 
  # post_dt %>%
  #   group_by(!!!syms(c("season"))) %>%
  #   mutate(m_med = mean(evi_pred),
  #          w_med = weighted.mean(evi_pred, area)) %>%
  #   ggplot(aes(x=P, y=evi_pred, color=factor(presence))) +
  #   geom_point(alpha=0.25) +
  #   facet_grid(cols = vars(season), scales="free") +# ,scales="free"
  #   geom_errorbar(aes(ymin=lower, ymax=upper, color=factor(presence)), alpha=0.15) +
  #   labs(x="Precipitation [mm]", y="EVI estimates", title="Seasonality") + #change title 
  #   # geom_hline(aes(yintercept=m_med, group = presence), colour="black") + #check differences between: w_med, m_med
  #   # geom_hline(aes(yintercept=w_med, group = presence), colour="black", linetype="dashed") + 
  #   theme_bw() +
  #   scale_color_manual(name="Sand dam",labels=c("absent", "present"), values=c("#F8766D", "#00BFC4"))
  # 
  
  
#TIME SERIES (BY YEAR)
  post_dt %>%
    group_by(!!!syms(c("year", "presence"))) %>%
    mutate(m_med = median(evi_pred),
           w_med = weightedMedian(evi_pred, area)) %>%
    ggplot(aes(x=P, y=evi_pred, color=factor(lc_class))) +
    geom_point(alpha=0.2) +
    facet_grid(cols=vars(year), rows = vars(presence)) +# ,scales="free"
    geom_errorbar(aes(ymin=lower, ymax=upper, color=factor(lc_class)), alpha=0.1) +
    labs(x="Precipitation [mm]", y="EVI estimates", title="Time series 2014-2020") + #change title 
    geom_hline(aes(yintercept=m_med, group = presence), colour="black") + #check differences between: w_med, m_med
    geom_hline(aes(yintercept=w_med, group = presence), colour="black", linetype="dashed") + 
    theme_bw()
  
#time series - boxplots
 a<-  post_dt %>%
    group_by(!!!syms(c("year", "presence"))) %>%
    mutate(m_med = median(evi_pred),
           w_med = weightedMedian(evi_pred, area)) %>%
    ggplot(aes(x=factor(presence),y=evi_pred, color=factor(presence))) +
    geom_boxplot(alpha=0.2) +
    facet_grid(cols=vars(year), rows = vars(lc_class)) +# ,scales="free"
    labs(x=" ", y="EVI estimates", title="Enhanced Vegetation Index (EVI) per year") + #change title 
  theme_bw() +
   theme(legend.position="bottom") +
   scale_color_manual(name="Sand dam",labels=c("absent", "present"), values=c("#F8766D", "#00BFC4"))

 c <- post_dt%>%
   filter(lc_class=="cropland") %>%
   group_by(!!!syms(c("year", "presence"))) %>%
   mutate(m_med = median(evi_pred),
          w_med = weightedMedian(evi_pred, area)) %>%
   ggplot(aes(x=factor(presence),y=evi_pred, color=factor(presence))) +
   geom_boxplot(alpha=0.2) +
   facet_grid(cols=vars(year)) +# ,scales="free"
   labs(x=" ", y="EVI (Cropland)", title="EVI") + #change title 
   theme_bw() +
   theme(legend.position="none")
 
 s <- post_dt%>%
   filter(lc_class=="shrubs") %>%
   group_by(!!!syms(c("year", "presence"))) %>%
   mutate(m_med = median(evi_pred),
          w_med = weightedMedian(evi_pred, area)) %>%
   ggplot(aes(x=factor(presence),y=evi_pred, color=factor(presence))) +
   geom_boxplot(alpha=0.2) +
   facet_grid(cols=vars(year)) +# ,scales="free"
   labs(x=" ", y="EVI (Shrubs)", title=" ") + #change title 
   theme_bw() +
   theme(legend.position="none")
   # theme(legend.position="bottom") +
   # scale_color_manual(name="Sand dam",labels=c("absent", "present"), values=c("#F8766D", "#00BFC4")) 
 
#precipitation boxplot 
 b<-  post_dt %>%
    group_by(year) %>%
    mutate(med_P = median(P),
           w_P = weightedMedian(P, area)) %>%
    ggplot(aes(y=P)) +
   scale_x_discrete(breaks = NULL) +
   xlab(NULL) +
    geom_boxplot(alpha=0.2) + #geom_histogram 
    facet_grid(cols=vars(year)) +# ,scales="free"
    labs(x=" ", y="Precipitation [mm]", title="Precipitation") + #change title 
    theme_bw()

#precipitation histogram 
 b<-  post_dt %>%
   group_by(year) %>%
   mutate(m_P = median(P),
          w_P = weightedMedian(P, area)) %>%
   ggplot(aes(y=P)) +
   # scale_x_discrete(breaks = NULL) +
   # xlab(NULL) +
   geom_histogram(alpha=0.2) + #geom_histogram 
   facet_grid(cols=vars(year)) +# ,scales="free"
   labs(x="Count", y="Precipitation [mm]", title="Precipitation") + #change title 
   theme_bw() +
   # geom_hline(aes(yintercept=m_P, group=year), colour="black") + #check differences between: w_med, m_med
   geom_hline(aes(yintercept=w_P, group=year), colour="black", linetype="dashed")
 
  
 grid.arrange(c, s, b)
 

 
e <- post_dt %>%
   group_by(year) %>%
   mutate(m_med = median(evi_pred),
          w_med = weightedMedian(evi_pred, area)) %>%
   ggplot(aes(x=factor(year),y=evi_pred)) +
   geom_boxplot(alpha=0.2) +
  labs(x=" ", y="EVI estimates", title="EVI") +  
   theme_bw()

p <- post_dt %>%
  group_by(year) %>%
  mutate(m_P = median(P),
         w_P = weightedMedian(P, area)) %>%
  ggplot(aes(x=factor(year),y=P)) +
  geom_boxplot(alpha=0.2) +
  labs(x=" ", y="Precipitation [mm]", title="Precipitation") + 
  theme_bw()

grid.arrange(e,p)

### STOPPED HERE ###
#evi vs p
post_dt %>%
  group_by(presence) %>%
  ggplot(aes(x=P, y=evi_pred, color=factor(presence), alpha=0.3)) +
  geom_ribbon(aes(ymin = evi_pred-2*sd, ymax=evi_pred+2*sd)) +
  facet_grid(rows=vars(season), cols=vars(presence))
  geom_ribbon(aes(ymin = lower, ymax=upper, x=P), fill="grey70",alpha =0.3) +
  facet_grid(rows=vars(season), cols=vars(presence))
  geom_line()
  geom_line()
  labs(x="Count", y="Precipitation [mm]", title="Precipitation") + #change title 
  theme_bw() +
  # geom_hline(aes(yintercept=m_P, group=year), colour="black") + #check differences between: w_med, m_med
  geom_hline(aes(yintercept=w_P, group=year), colour="black", linetype="dashed")
  geom_ribbon(aes(ymin=pred-2*SE2,ymax=pred+2*SE2),alpha=0.2,fill="red") +

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

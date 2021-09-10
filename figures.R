
posterior <- as.matrix(fit_GP_unp_int)
head(posterior)

fit_summary <- summary(fit_GP_unp_int)
print(names(fit_summary))
print(fit_summary$summary)

#plot: compare empirical distribution of the data (evi) 
#     to the posterior distribution (evi) EVI 

y <- standat$evi #not weigthed by area 

# #variante 1:
#    # selector for y_rep columns
#    sel <- grep("evi_pred", colnames(GP_int))
#    sel
#    
#    yrepo <- matrix(NA, nrow = length(sel), ncol = 6000)
#    for (i in 1:length(sel)) {
#       yrepo[i,] <- GP_int[,sel[i]]
#    }

#variante 2:
yrep <- posterior[,867:1146] 
   #in this case: evi_pred[1:280], iter=6000 <- should I only draw from the iterations for visualization ??
ppc_dens_overlay(y = y, #observed
                 yrep = yrep) #predicted

      ppc_stat(y, yrep, stat = "mean") + grid_lines() #mean or median? 
      ppc_hist(y, yrep[1:19, ])

#transpose matrix
yrep_t <- t(yrep)
yrep_t <- as.data.frame(yrep_t)
yrep_t$time <- standat$time
yrep_t$gp_id <- standat$gp_id
yrep_t$presence <- standat$presence
yrep_t$lc_class <- standat$lc_class
yrep_t$rn <- rownames(yrep_t)

df_try <- df_s[df_s$ID <= 5,]
yrep_t$season <- df_try$season
yrep_t$sd_id <- df_try$ID
yrep_t$lc_sd <- df_try$X #n=336
yrep_t$area <- df_try$area_m2

yrep_t_long <- gather(yrep_t, key=iter, value=evi_est, -time, -gp_id, -presence, -lc_class, -rn, -season, -sd_id, -lc_sd, -area)
head(yrep_t_long)


yrep_mean_w <- yrep_t_long %>%
   group_by(lc_sd) %>%
   summarise(weighted.mean(evi_est, area))

yrep_mean <- yrep_t_long %>%
   group_by(lc_sd) %>%
   summarise(mean(evi_est))
#comment: 
#Do I need to weigh it or does anyways every lc_sd have the same area? 




#VISUALIZE TIME SERIES 
      
      #one GP example (GP_id== )
      plot1 <- yrep_t_long %>%
         filter (gp_id==1) %>%
         group_by(time) %>%
         summarize("mean_evi" = mean(evi_est),
                    "evi_Q25" = quantile(evi_est, probs = 0.25),
                    "evi_Q75" = quantile(evi_est, probs = 0.75),
                   "evi_Q05" = quantile(evi_est, probs = 0.05),
                   "evi_Q95" = quantile(evi_est, probs = 0.95)) %>%
         ggplot(aes(x=time, y=mean_evi)) + 
         geom_ribbon(aes(ymin = evi_Q05, ymax=evi_Q95, fill="lightblue"), alpha =0.3) +
         geom_ribbon(aes(ymin = evi_Q25, ymax=evi_Q75, fill="yellow"), alpha = 0.4) +
         geom_line(aes(x=time, y=mean_evi, color="blue", linetype="dashed")) +
         scale_x_continuous("Time", 
                            breaks = seq(0, 92, by=12),
                            labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021")) 
      #wrong connection? I think this figure is wrong: GP are boiling down Credible intervals to zero where observational data is !?
      
      #all GPs -> can i draw this inference? 
      plot <- yrep_t_long %>%
         #filter (gp_id==1) %>%
         group_by(time) %>%
         summarize("mean_evi" = weighted.mean(evi_est, area),
                   "evi_Q25" = quantile(evi_est, probs = 0.25),
                   "evi_Q75" = quantile(evi_est, probs = 0.75),
                   "evi_Q05" = quantile(evi_est, probs = 0.05),
                   "evi_Q95" = quantile(evi_est, probs = 0.95)) %>%
         ggplot(aes(x=time, y=mean_evi)) + 
      #   geom_ribbon(aes(ymin = evi_Q05, ymax=evi_Q95, fill="lightblue"), alpha =0.3) +
      #   geom_ribbon(aes(ymin = evi_Q25, ymax=evi_Q75, fill="yellow"), alpha = 0.4) +
         geom_point(aes(x=time, y=mean_evi, color="blue", linetype="dashed")) +
         scale_x_continuous("Time", 
                            breaks = seq(0, 92, by=12),
                            labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021")) 
      #WRONG???
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

      eobs <- df_try %>%
         filter (X == 5) %>% #messed up numbers
         ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() +
         labs(y = "EVI", title="EVI observed (lc_sd == 5)") + 
         scale_x_continuous("Time", 
                            breaks = seq(0, 92, by=12),
                            labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))
      
      grid.arrange(epred, eobs)



      group_cols <- c("presence", "lc_sd", "time")  
      yrep_t_long %>% 
         group_by(!!!syms(group_cols)) %>% 
         summarize("mean_evi" = weighted.mean(evi_est, area)) %>%
         ggplot(aes(y=mean_evi, x=time, colour=factor(presence))) + geom_point() +
         scale_x_continuous("Time", 
                            breaks = seq(0, 92, by=12),
                            labels =c("2014","2015","2016","2017", "2018", "2019", "2020", "2021"))

# MODEL SLOPES

      #Precipitation 
            mcmc_areas(posterior, pars="b1",
                       prob = .80)   
      #Sand dam presence
            mcmc_areas(posterior, pars="b2",
                       prob=0.80)
      #comparison Precipitation and Sand dam presence (take care: presence = discrete 0,1)
            pairs(fit_GP_unp_int, pars = c("b1", "b2"), log = TRUE, las = 1)
      
            mcmc_areas(posterior, pars=c("b1", "b2", "b4"),
                              prob = .80)
      
      #LC class comparison
         mcmc_areas(posterior, pars = c("b3[1]", "b3[2]"), prob=0.8)
         #or:
         mcmc_areas(posterior, regex_pars = "b3", prob=0.8)
      
      # TIME 
         
         #???
         
# SEASON 
   #observed
   obs_seas <- df_s %>%
         ggplot(aes(y=evi, x=season, colour=factor(presence))) + 
         geom_boxplot() +
         labs(x="Seasons", y="Observed EVI", title="Observed data", colour = " ") +
         scale_color_manual(labels = c("No sand dam", "Sand dam"),
                            values = c("darkgrey", "brown"))
   pred_seas <- yrep_t_long %>%
         ggplot(aes(y=evi_est, x=season, colour=factor(presence))) +
         geom_boxplot() +
         labs(x="Seasons", y="Predicted EVI", title="Predicted data", colour = " ") +
         scale_color_manual(labels = c("No sand dam", "Sand dam"),
                            values = c("lightblue", "blue"))
   
   grid.arrange(obs_seas, pred_seas)
   

#######
mean_evi <- stats::weighted.mean(yrep_t_long$evi_est, yrep_t_long$area)
# Predicted mean EVI per GP 

   yrep_t_long %>%
     group_by(lc_sd) %>%
      summarize("GP_mean_evi" = stats::weighted.mean(evi_est, area)) %>%
      ggplot(aes(y=GP_mean_evi, x=lc_sd)) + geom_point() +
      labs(x="GP (LC/SD)", y="Predicted mean EVI", title="Predicted mean EVI per GP (n=336)") +
      geom_hline(yintercept=mean_evi, linetype = 2, size = 1/4)

# Histogram Predicted mean EVI
   yrep_t_long %>%
      group_by(lc_sd) %>%
      summarize("GP_mean_evi" = stats::weighted.mean(evi_est, area)) %>%
      ggplot(aes(x=GP_mean_evi)) + geom_histogram() +
      labs(x="Predicted mean EVI", y="counts", title="Distribution of predicted mean EVI (n=336 GPs)") +
      geom_vline(xintercept=mean_evi, linetype = 2, size = 1/4)#+

#comments: add observational data
#          add credible intervals 
  
#outer cross-validation 
cv_pred <- yrep_t_long %>%
         group_by(!!!syms(c("time", "lc_sd"))) %>%
         summarise(evi_pred=weighted.mean(evi_est, area))

cv_obs <- df_try %>%
   group_by(!!!syms(c("time_mean", "X"))) %>%
   summarise(evi_obs = weighted.mean(evi, area_m2))

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
   geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1) +
   geom_text(x = 0.22, y = 0.4, label = lm_eqn(cv), parse = TRUE) +
   #coord_cartesian(xlim= c(0,0.45),ylim=c(0,0.45))
   labs(x="Predicted EVI", y="Observed EVI", title="Outer Cross-Validation")
   
p


#points= 280 (10 GP * 28 time stemps)

nrow(cv)

head(cv)
library(ggpubr)
library(ggplot2)
ggplot(cv,aes(x = evi_pred, y = evi_obs)) + 
   geom_point() + 
   geom_smooth(method = "lm", se=FALSE) +
   stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
   stat_regline_equation(label.y = 350, aes(label = ..rr.label..))


ggplot(cv, aes(x=evi_pred, y=evi_obs)) + 
   geom_point() +
   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) 
#+
   stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
   stat_regline_equation(label.y = 350, aes(label = ..rr.label..))
library(ggpubr)

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
   labs(y = "Precipitation [unit]") + 
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


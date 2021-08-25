    #load("~/01Master/MasterThesis/Pius/R/sand dam/df_com_all.RData")

library(dplyr)
library(rstan)
library(lubridate)
library(zoo)
library(tidyr)
library(bayesplot)
library(loo)
library(shinystan)
library(purrr)
library(ggsci)
library(tictoc)

library(ggplot2)
library(gridExtra)

    # df_par <- df_com  
    # 
    # # aggregate over the 4 periods 
    # df_y_m <- df_par %>%
    #   mutate (year = year(year_month)) %>%
    #   mutate (month= month(year_month, label=T))
    # 
    # df_y_m$season <- factor(df_y_m$month)
    # levels(df_y_m$season) <- list(JF = month.abb[c(1,2)],
    #                               MAM = month.abb[c(3:5)],
    #                               JJAS = month.abb[c(6:9)],
    #                               OND = month.abb[c(10:12)])
    # #based on literature and expert knowledge 
    # 
    # df_y_m$seas_y <- paste(df_y_m$season, df_y_m$year, df_y_m$X, sep="_")
    # 
    # df_season <- df_y_m %>% 
    #   group_by(seas_y) %>% 
    #   summarize(EVI_mean = mean(EVI_mean),
    #             Precip_mean = mean(Precip_mean),
    #             time_mean = mean(timeorder)) %>% #check: okay?
    #   as.data.frame()
    # 
    # df_s <- merge(df_season, df_y_m, by="seas_y")
    # 
    # df_s <- df_s %>%
    #   mutate(evi = EVI_mean.x,
    #          P = Precip_mean.x) %>%
    #   select(-EVI_mean.y, -EVI_mean.x, -Precip_mean.y, -Precip_mean.x, -date.x, -date.y, -year_month, -month, -timeorder, -X.1)
    # df_s <- distinct(df_s)
    # 
    # head(df_s)
    # nrow(df_s)
    # 
    # 
    # save(df_s, file = "df_seas_all.RData")

load("~/01Master/MasterThesis/Pius/R/sand dam/df_seas_all.RData")

df_s$gp_id <- paste(df_s$X, df_s$class, sep="_")

#in presence column are NAs 
nrow(df_s[is.na(df_s$presence), ])  

#removed:
df_s <- df_s %>%
  drop_na("presence")
unique(df_s$presence)

# # #subset 
# df_kitui <- df_s %>%
#   filter(study == "Kitui West (Pius)")
# unique(df_tiva$ID)

# add/wrangle season column 
df_s <- df_s %>%
  transform(seas=as.numeric(factor(season)))
unique(df_s$seas)
#1 = JF
#3 = JJAS
#2 = MAM
#4 = OND

#save(df_s, file = "df_s.RData")
head(df_s)

# PLOTTING AROUND: DATA VISUALIZATION (EVI, Precipitation)
#für x == 1
e1 <- df_s %>%
  filter (X == 1) %>%
  ggplot(aes(x=time_mean, y=evi)) + geom_point() + geom_line() 

p1 <- df_s %>%
  filter (X == 1) %>%
  ggplot(aes(x=time_mean, y =P))+geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5)

grid.arrange(e1, p1)

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

# # # CREATE LIST OF DATA FOR STAN MODEL # # #
standat = with(df_s[df_s$ID <= 5,], list( #[df_s$ID <= 30,] #[df_s$ID <= 10,]
  evi = evi,
  P = P,
  time = time_mean,
  presence = presence, ## do not do the factor thing for dummy variables, only index variables
  lc_class = as.integer(factor(LC_proj)),
  gp_id = as.integer(factor(gp_id)),
  seas = as.integer(factor(seas))
))

standat$gp_sampsize = table(standat$gp_id)
standat$max_gp_sampsize = max(standat$gp_sampsize)
standat$ngp = max(standat$gp_id)
standat$N = length(standat$evi)
standat$n_lc = max(standat$lc_class)
standat$n_seas = max(standat$seas)

#rescale precipitation data
standat$P = standat$P * 0.001

# GAUSSIAN PROCESS MODEL #
### UNPOOLED GP ###
GP2_unpooled = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_2_unpool.stan")

          tic()
          fit_GP2_unpooled = sampling (GP2_unpooled, data=standat,
                                       chains=4,
                                       cores=7,
                                       iter=4000,
                                       warmup=1000)
          toc()
          
          #took 3.5 hours 
          #
          # control = list(adapt_delta = 0.99,
          #                max_treedepth = 15)) #,warmup = 1000
          
          # summary(fit_GP2_unpooled)
          # plot(fit_GP2_unpooled)
          
          
          ll_1 <- extract_log_lik(fit_GP2_unpooled, parameter_name = "loglik", merge_chains = TRUE)
          m1 <- loo(ll_1)
          
          
          
          rstan::check_divergences(fit_GP2_unpooled)
          
          n_chains <- 4
          warmups <- 1000
          max_treedepth <- 10
          mack_diagnostics <- rstan::get_sampler_params(fit_GP2_unpooled) %>% 
            set_names(1:n_chains) %>% 
            map_df(as_data_frame,.id = 'chain') %>% 
            group_by(chain) %>% 
            mutate(iteration = 1:length(chain)) %>% 
            mutate(warmup = iteration <= warmups)
          
          mack_diagnostics %>% 
            group_by(warmup, chain) %>% 
            summarise(percent_divergent = mean(divergent__ >0)) %>% 
            ggplot() +
            geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
            scale_y_continuous(labels = scales::percent, name = "% Divergent Runs")  + 
            scale_fill_npg()
          
          mack_diagnostics %>% 
            ggplot(aes(iteration, treedepth__, color = chain)) + 
            geom_line() + 
            geom_hline(aes(yintercept = max_treedepth), color = 'red') + 
            scale_color_locuszoom()
          
          #Convergence/ MCMC diagnostics 
          #Rhat
          rhat_GP2unp <- rhat(fit_GP2_unpooled)
          mcmc_rhat_hist(rhat_GP2unp)
          #Neff
          ratios_GP2unp <- neff_ratio(fit_GP2_unpooled)
          mcmc_neff(ratios_GP2unp)
          
          #traceplot
          #Which parameters are of interest?
          #, "sigma" = temporal space (VCV matrix of time steps), 
          # "b1" = slope for precipitation 
          # "b2" = slope for presence (=sand dam info)
          # "b3" = slope for land cover (categorical predictor: shrubs, cropland)
          # rho?
          # alpha?
          
          # traceplot(fit_GP2_unpooled, pars= c("rho[1]", "alpha[1]")) 
          # traceplot(fit_GP2_unpooled, pars = c("b1", "b2"))
          # traceplot(fit_GP2_unpooled, pars = c("b3[1]", "b3[2]"))
          # 
          # #or
          # mcmc_trace(fit_GP2_unpooled, pars= c("rho[2]", "rho[1]","rho[3]"))#, "b1", "b2", "b3")) #Which parameter are of interest?
          
          #Posterior draws: 
          #draw samples from posterior distribution 
          
          gc()
          memory.size()
          #memory.limit()
          
          #as.matrix
          GP2_unp <- as.matrix(fit_GP2_unpooled)
          #Error: cannot allocate vector of size 1.7 Gb
          #solution closed other tabs!
          
              # #extract
              # list_of_draws <- rstan::extract(fit_GP2_unpooled)
              # #Error: cannot allocate vector of size 863.4 Mb
              # 
              # #as.data.frame
              # list_of_draws <- as.data.frame(fit_GP2_unpooled)
              # #Error: cannot allocate vector of size 1.7 Gb
              # 
              # #as.array
              # list_of_draws <- as.array(fit_GP2_unpooled)
              # #Error: cannot allocate vector of size 1.7 Gb
          
          mcmc_areas(GP2_unp, 
                     pars='b1',
                     prob = .90
          )
          
          mcmc_areas(GP2_unp, 
                     pars='b2',
                     prob = .90
          )
          
          mcmc_areas(GP2_unp, 
                     pars=c("b3[1]", "b3[2]"),
                     prob = .90
          )
          
          mcmc_areas(GP2_unp, 
                     pars='gamma',
                     prob = .90
          )
          
          
          #first interpretation: 
          #b1 positive, evi increases with increasing precipitation 
          #b2 positive, evi increases with increasing sand dam presence 
          #b3 comparing shrubs and cropland - very similar, but slightly higher evi at 2 (cropland) than 1 (shrubs)
          #gamma

########################
GP_unpool_seas = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_unp_seas.stan")

          tic()
          fit_GP_unpool_seas = sampling (GP_unpool_seas, data=standat,
                                       chains=4,
                                       cores=7,
                                       iter=7000,
                                       warmup=3000,
                                       control = list(adapt_delta = 0.99, #Increase the target acceptance rate
                                                      max_treedepth = 15)) #Increase the maximum allowed treedepth
          toc()
          
          
          ll_m2 <- extract_log_lik(fit_GP_unpool_seas, parameter_name = "loglik", merge_chains = TRUE)
          m2 <- loo(ll_m2)
          
          loo_compare(m1, m2)

          rhat_GP2_seas <- rhat(fit_GP_unpool_seas)
          mcmc_rhat_hist(rhat_GP2_seas)
          
          GP2_seas <- as.matrix(fit_GP_unpool_seas)
#### PARTIALLY POOLED GP ####
GP2_ppool = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_2_ppool.stan")

tic()
fit_GP2_ppool = sampling (GP2_ppool, data=standat, 
                          chains=4,
                          cores=7, 
                          iter=4000,
                          warmup=1500,
                          control = list(adapt_delta = 0.99, #Increase the target acceptance rate
                                         max_treedepth = 15)) #Increase the maximum allowed treedepth)
toc()
#There were 986 divergent transitions after warmup (4000, 1000, no control)
#There were 100 divergent transitions after warmup (4000, 1000, control)
#There were 1989 divergent transitions after warmup (6000, 2000, control)
#changed priors: only few (5?) transitions

#MODEL COMPARISON : Can I even compare them directly? 
ll_3 <- extract_log_lik(fit_GP2_ppool, parameter_name = "loglik")
m3 <- loo(ll_3)

loo_compare(m1,m2,m3)

summary(fit_GP2_ppool)
GP2_pp <- as.matrix(fit_GP2_ppool)

pairs(fit_GP2_ppool, pars=c("rho[1]", "rho[2]"), las=1)
#Convergence/ MCMC diagnostics 
#Rhat
rhat_GP2pp <- rhat(fit_GP2_ppool)
mcmc_rhat_hist(rhat_GP2pp)
#Neff
ratios_GP2pp <- neff_ratio(fit_GP2_ppool)
mcmc_neff(ratios_GP2pp)

#traceplot
traceplot(fit_GP2_ppool, pars= c("rho[1]", "alpha[1]")) #Which parameter are of interest?
traceplot(fit_GP2_ppool, pars = c("b1", "b2"))
traceplot(fit_GP2_ppool, pars = c("b3[1]", "b3[2]"))
#, "sigma", "b1", "b2", "b3"
#or
mcmc_trace(fit_GP2_ppool, pars= c("rho[2]", "rho[1]","rho[3]"))#, "b1", "b2", "b3")) #Which parameter are of interest?

mcmc_areas(GP2_pp, 
           pars=c('b1', "b2", "b3[1]", "b3[2]"),
           prob = .90
)

mcmc_areas(GP2_pp, 
           pars="b2",
           prob = .90
)


#Error in check_pars(allpars, pars) : no parameter log_lik
m_unp <-loo(GP2_ppooled)
m_pp <-loo(GP2_unpooled)
loo(fit_GP2_ppool)
extract_log_lik(fit_GP2_ppool, )
extract_log_lik(fit_GP2_ppool, parameter_name = "lp__", merge_chains = TRUE) 'lp__ = log-posterior'
loo_compare(m_unp, m_pp)
#Error: Not all models have the same number of data points.

?extract_log_lik
?waic()
w

waic(fit_3)
waic(fit_2)

loo_compare(waic(fit_0), waic(fit_1), waic(fit_2), waic(fit_3))

loo(fit_2)
loo(fit_3)

loo_compare(loo(fit_2), loo(fit_3))

bridge_sampler
#variational bayes 
#BRMS

library(brms)
dat <- df_s %>%
  filter(X <= 3)

fit_b <- brm(data= dat, 
             family = gaussian, 
             evi~ 1 + gp(time_mean, by=X) + P,
             prior = c(prior(normal(0,19), class = Intercept),
                       prior(normal(0,1), class = b),
                       prior(cauchy(0,1), class = sdgp)),
             chains=1,
             cores=7, 
             warmup = 1000,
             iter=1100,
             seed=13,
             control = list(adapt_delta = 0.999,
                            max_treedepth = 12))

#Rhat
rhat_b <- rhat(fit_b)
mcmc_rhat(rhat_b)

pred_b <- predict(fit_b, probs = c(0.055,0.945))
pred_b <- as.data.frame(pred_b)

loo(fit_b)
waic(fit_b)

loo_compare()

fit_b2 <- brm(data= dat, 
              family = gaussian, 
              evi~ 1 + gp(time_mean, by=X) + P + presence,
              prior = c(prior(normal(0,10), class = Intercept),
                        prior(normal(0,10), class = b, coef="P"),
                        prior(normal(0,10), class = b, coef="presence"),
                        prior(cauchy(0,10), class = sdgp)),
              chains=1,
              cores=7, 
              warmup = 1000,
              iter=1100,
              seed=13,
              control = list(adapt_delta = 0.999,
                             max_treedepth = 12))



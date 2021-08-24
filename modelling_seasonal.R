    load("~/01Master/MasterThesis/Pius/R/sand dam/df_com.RData")

library(dplyr)
library(rstan)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)
library(bayesplot)
library(loo)
library(shinystan)
library(purrr)
library(ggsci)
library(tictoc)

    df_par <- df_com %>%
      filter (class != "veg_aqua")
    
    # aggregate over the 4 periods 
    
    df_y_m <- df_par %>%
      mutate (year = year(year_month)) %>%
      mutate (month= month(year_month, label=T))
    
    df_y_m$season <- factor(df_y_m$month)
    levels(df_y_m$season) <- list(JF = month.abb[c(1,2)],
                            MAM = month.abb[c(3:5)],
                            JJAS = month.abb[c(6:9)],
                            OND = month.abb[c(10:12)])
              #based on expert knowledge 
    
    df_y_m$seas_y <- paste(df_y_m$season, df_y_m$year, df_y_m$X, sep="_")
    
    df_season <- df_y_m %>% 
      group_by(seas_y) %>% 
      summarize(EVI_mean = mean(EVI_mean),
                Precip_mean = mean(Precip_mean),
                time_mean = mean(timeorder)) %>% #check: okay?
      as.data.frame()
    
    df_s <- merge(df_season, df_y_m, by="seas_y")
    
    df_s <- df_s %>%
      mutate(evi = EVI_mean.x,
             P = Precip_mean.x) %>%
      select(-EVI_mean.y, -EVI_mean.x, -Precip_mean.y, -Precip_mean.x, -EVI_sd, - Precip_sd, -date.x, -date.y, -year_month, -month, -timeorder)
    df_s <- distinct(df_s)
    
    head(df_s)
    nrow(df_s)
    
        # #old way of determining timeorder
        # split_dfs = strsplit(df_s$seas_y, split="_", fixed=TRUE)
        # split_s = unlist(lapply(split_dfs, "[[", 1))
        # split_y = unlist(lapply(split_dfs, "[[", 2))
        # df_s$sy <- paste(split_s, split_y, sep="_")
        # 
        #  
        # df_s <- df_s %>%
        #   transform(timeorder=as.numeric(factor(sy)))
        # head(df_s)
    
    save(df_s, file = "df_seas.RData")
    
load("~/01Master/MasterThesis/Pius/R/sand dam/df_seas.RData")

df_s$gp_id <- paste(df_s$X, df_s$class, sep="_")

#in presence column are NAs 
nrow(df_s[is.na(df_s$presence), ])  
#a. either remove them
#or b. try to work with "Year_Us column"

#for now: removed:
df_s <- df_s %>%
  drop_na("presence")
unique(df_s$presence)

# # # CREATE LIST OF DATA FOR STAN MODEL # # #
standat = with(df_s[df_s$ID <= 135,], list( #[df_s$ID <= 30,]
  evi = evi,
  P = P,
  time = time_mean,
  presence = presence, ## do not do the factor thing for dummy variables, only index variables
  lc_class = as.integer(factor(LC_proj)),
  gp_id = as.integer(factor(gp_id))
))

standat$gp_sampsize = table(standat$gp_id)
standat$max_gp_sampsize = max(standat$gp_sampsize)
standat$ngp = max(standat$gp_id)
standat$N = length(standat$evi)
standat$n_lc = max(standat$lc_class)
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

#,
                    # control = list(adapt_delta = 0.99,
                    #                max_treedepth = 15)) #,warmup = 1000
# tic()
# fit_GP2_unpooled = sampling (GP2_unpooled, data=standat, 
#                              chains=2,
#                              cores=7, 
#                              iter=2000,
#                              control = list(adapt_delta = 0.99,
#                                             max_treedepth = 15)) #,warmup = 1000
# toc()
summary(fit_GP2_unpooled)
plot(fit_GP2_unpooled)

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
  traceplot(fit_GP2_unpooled, pars= c("rho[1]", "alpha[1]")) #Which parameter are of interest?
  traceplot(fit_GP2_unpooled, pars = c("b1", "b2"))
  traceplot(fit_GP2_unpooled, pars = c("b3[1]", "b3[2]"))
  #, "sigma", "b1", "b2", "b3"
  #or
  mcmc_trace(fit_GP2_unpooled, pars= c("rho[2]", "rho[1]","rho[3]"))#, "b1", "b2", "b3")) #Which parameter are of interest?
 
GP2_unp <- as.matrix(fit_GP2_unpooled)

mcmc_areas(GP2_unp, 
           pars=c('b1', "b2", "b3[1]", "b3[2]"),
           prob = .90
)
#first interpretation: b2 positive, evi increases with increasing presence(1) 
# -> ~4.5% increase of EVI when sand dam is present?
mcmc_areas(GP2_unp, 
           pars=c("b3[1]", "b3[2]"),
           prob = .90
)                       


#### PARTIALLY POOLED GP ####
GP2_ppool = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_2_ppool.stan")

fit_GP2_ppool = sampling (GP2_ppool, data=standat, 
                    chains=2,
                    cores=7, 
                    iter=2000,
                    control = list(adapt_delta = 0.99, #Increase the target acceptance rate
                                   max_treedepth = 15)) #Increase the maximum allowed treedepth) 
                    #,
                    #iter=4000,
                    #
                    #, warmup = 1000)

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

#MODEL COMPARISON : Can I even compare them directly? 
log_lik_GPppool <- extract_log_lik(fit_GP2_ppool, parameter_name = "log_likelihood")
loo(fit_GP2_ppool)
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



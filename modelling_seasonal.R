load("~/01Master/MasterThesis/Pius/R/sand dam/df_com.RData")

library(dplyr)
library(rstan)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)
library(bayesplot)
library(loo)

df_par <- df_com %>%
  filter (class != "veg_aqua")

#aggregate over year 
df_y <- df_par %>%
  mutate (year = year(year_month))

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


df_s$gp_id <- paste(df_s$X, df_s$class, sep="_")

#in presence column are NAs 
nrow(df_s[is.na(df_s$presence), ])  
#a. either remove them
#or b. try to work with "Year_Us column"

#for now remove:
df_s <- df_s %>%
  drop_na("presence")
unique(df_s$presence)

standat = with(df_s[df_s$X <= 3,], list(
  evi = evi,
  P = P,
  time = time_mean,
  presence = presence,
  gp_id = as.integer(factor(gp_id))
))

standat$gp_sampsize = table(standat$gp_id)
standat$max_gp_sampsize = max(standat$gp_sampsize)
standat$ngp = max(standat$gp_id)
standat$N = length(standat$evi)

### UNPOOLED GP ###
GP2_unpooled = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_2.stan")

fit_GP2_unpooled = sampling (GP2_unpooled, data=standat, 
                    chains=1,
                    cores=7, 
                    iter=1100, warmup = 1000)
summary(fit_GP2_unpooled)
plot(fit_GP2_unpooled)

  #Rhat
  rhat_GP2unp <- rhat(fit_GP2_unpooled)
  mcmc_rhat_hist(rhat_GP2unp)
  #Neff
  ratios_GP2unp <- neff_ratio(fit_GP2_unpooled)
  mcmc_neff(ratios_GP2unp)
  #traceplot
  traceplot(fit_GP2_unpooled, pars= c("rho", "alpha", "sigma", "a", "b1", "b2")) #Which parameter are of interest?
  #or
  mcmc_trace(fit_GP2_unpooled, pars= c("rho", "alpha", "sigma", "a", "b1", "b2")) #Which parameter are of interest?
  #Error: Some 'pars' don't match parameter names: rho, alpha FALSE

GP2_unpooled <- as.matrix(fit_GP2_unpooled)

#### PARTIALLY POOLED GP ####
GP2_ppool = stan_model("~/01Master/MasterThesis/Pius/R/sand dam/GP_2_ppool.stan")

fit_GP2_ppool = sampling (GP2_ppool, data=standat, 
                    chains=1,
                    cores=7, 
                    iter=1100, warmup = 1000)

summary(fit_GP2_ppool)
GP2_ppooled <- as.matrix(fit_GP2_ppool)

#MODEL COMPARISON 

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


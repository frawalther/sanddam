#packages
library(rstan)
library(dplyr)
library(ggplot2)
library(rstanarm)
library(bayesplot)

#packages of interest 

library(loo)
library(brms)
library(atsar)


#load dataset
load("df_com.RData")
head(df_com)
nrow(df_com)
unique(df_com$X) #286

#exclude LC "veg_aqua"
df_par <- df_com %>%
  filter (class != "veg_aqua")

df_uno <- df_par %>%
  filter ( X <= "1")

#extract only specific months/aggregate over period
library(lubridate)
df_uno$year_month
df_com$
  
#Gaussian process


#Latent variable GP
#add something to ID individual time series 
df_par$gp_id <- paste(df_par$X, df_par$class, sep="_")

#try a subset of data first
standat = with(df_par[df_par$X <= 3,], list(
  evi = EVI_mean,
  P = Precip_mean,
  time = timeorder,
  gp_id = as.integer(factor(gp_id))
))


GP2 = stan_model("GP_2.stan")
    # Warning message:
    #   In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
    #   '-E' not found
#"This warning can safely be ignored and will be removed in the next release."
fit_GP2 = sampling (GP2, data=standat, 
                    chains=1,
                    cores=4, 
                    iter=1100, warmup = 1000)


summary(fit_GP2)
matrix_GP2<- as.matrix(fit_GP2)
print(colnames(matrix_GP2)) 
print(dim(matrix_GP2))

head(df_1)
### for 1 SD 
  df_1 <- df_par %>%
    filter( X == 1) %>%
    filter(class == "cropland")

  n <- nrow(df_1)
  #t = df_com$timeorder
  
  GP1 = stan_model("GP_1.stan")

  fit_GP1 = sampling (GP1, data=list(N = n,
                                     evi = df_1$EVI_mean,
                                     P = df_1$Precip_mean,
                                     time = df_1$timeorder), 
                      chains=1,
                      cores=7, 
                      iter=1100, warmup = 1000)

  
  # different code writing
  # fit_GP1 <- stan(file="GP_1.stan",
  #                 data = list(N = n,
  #                             evi = df_1$EVI_mean,
  #                             P = df_1$Precip_mean,
  #                             time = df_1$timeorder), 
  #                             #LC = df_1$LC_proj),
  #                 chains=1, 
  #                 cores=4,
  #                 iter=2000)

  #need to repeat that for all Sds/X

summary(fit_GP1)$summary %>% head()
glimpse(fit_GP1)

plot(fit_GP1)

  #Rhat
    rhat_GP1 <- rhat(fit_GP1)
    mcmc_rhat_hist(rhat_GP1)
  #Neff
    ratios_GP1 <- neff_ratio(fit_GP1)
    mcmc_neff(ratios_GP1)
  #traceplot
    traceplot(fit_GP1, pars= c("rho", "alpha", "sigma", "a", "b1")) #Which parameter are of interest?
    #or
    mcmc_trace(fit_GP1, pars= c("rho", "alpha", "sigma", "a", "b1")) #Which parameter are of interest?

  
#posterior predictive checks
list_of_draws <- extract(fit_GP1)
print(names(list_of_draws))  
    
matrix_of_draws<- as.matrix(fit_GP1)
print(colnames(matrix_of_draws)) 
print(dim(matrix_of_draws))
#   #posterior predictive checks: WHICH FUNCTION?
#     posterior_samples()
#     posterior_predict()
#     fitted()
#     samples()
#     
#   posterior <- as.matrix(fit_GP1)
#   #or?
#   post <- posterior_predict(fit_GP1, 100)
#   mcmc_areas(posterior) # no good visualization 
#   ?posterior_predict(posterior )
#   ?sampling()
#   pp_check(fit_GP1, nsamples = 100, type = "hist")
   bayesplot::pp_check(fit_GP1, fun="dens_overlay", nreps = 100)
#   #bayesplot() package
#   #Extract posterior draws from stanfit object
#   GP1_draws <- extract(fit_GP1)
#   # Traceplot 
#   mcmc_trace(GP1_draws) #pars = c("mean", "sd", "sigma")
#   ### Random draws from the full model compared to observed data
#   color_scheme_set("red")
#   ppc_dens_overlay(y = fit_GP1$y,
#                    yrep = posterior_predict(fit_GP1, draws = 50))
#   post_pred <- posterior_predict(fit, draws = 30) # Why is no transformation needed here?
#   
#   library(bayesplot)
#   
#   ppc_dens_overlay(y = fit$y, yrep = post_pred)
#   
#    
# #Latent variable GP 
#   #subsample  
# df_par = df_crop[sample(nrow(df_crop),200),]
#   n<- nrow(df_par)
#    
#   GP2 = stan_model("GP_2.stan")
#   fit_GP2 = sampling(GP2, data = list(N=n,
#                                       evi = df_par$EVI_mean,
#                                       P = df_par$Precip_mean,
#                                       time= df_par$timeorder),
#                      chains=1,
#                      cores=4,
#                      iter=2000)
# 
# #wrong model: need to explicitly calculate VCV matrix per site 
# 
#   summary(fit_GP2)
#   posterior_GP2 <- as.matrix(fit_GP2)
#   head(posterior_GP2)
#   

# BRMS   
#brms GP model 
   #subset
dat <- df_par %>%
  filter(X == 3)

fit_b <- brm(data= dat, 
             family = gaussian, 
             EVI_mean ~ 1 + gp(timeorder) + Precip_mean,
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



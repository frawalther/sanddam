#packages
library(rstan)
library(dplyr)

#packages of interest 
library(bayesplot)
library(loo)
library(brms)
library(rtsanarm)
library(atsar)

library(ggplot2)


#load dataset
load("df_com.RData")
head(df_com)
nrow(df_com)
unique(df_com$X) #286

#exclude LC "veg_aqua"
df_par <- df_com %>%
  filter (class != "veg_aqua")
df_par = subset(df_com, class != "veg_aqua") ## Matt doesn't have dplyr :-P

nrow(df_par)
unique(df_par$X) #269

#Gaussian process:
  # marginal likelihood GP 
  n <- nrow(df_com)
  #t = df_com$timeorder
  fit_GP1 <- stan(file="GP_1.stan",
                  data = list(N = n,
                              evi = df_com$EVI_mean,
                              P = df_com$Precip_mean),
                              #t = df_com$timeorder),
                  chains=1, 
                  cores=4,
                  iter=2000)

#need to include time steps - time order where? 

#Latent variable GP 
# add something to ID individual time series
df_par$gp_id = paste(df_par$X, df_par$class, sep="_")

# try a subset of data first
standat = with(df_par[df_par$X <= 3,], list(
    evi = EVI_mean,
    P = Precip_mean,
    time = timeorder,
    gp_id = as.integer(factor(gp_id))
))
standat$gp_sampsize = table(standat$gp_id)
standat$max_gp_sampsize = max(standat$gp_sampsize)
standat$ngp = max(standat$gp_id)
standat$N = length(standat$evi)

  GP2 = stan_model("GP_2.stan")
  fit_GP2 = sampling(GP2, data = standat,
                     chains=1,
                     cores=4,
                     iter=2000)



### TRASH ####
# #autoregressive model
# #Stan AR(1)
# #m1_AR
# n <- nrow(df_com)
# #or: 
# # unique(df_com$X)
# # n <- 286 #dont think so 
# 
# fit_AR1 <- stan(file="m1_AR.stan", 
#                 data = list(N = n,
#                             y = df_com$EVI_mean),
#                 chains=4, 
#                 cores=4,
#                 iter=2000)
# summary(fit_AR1)
# posterior1 <- as.matrix(fit_AR1)
# head(posterior1)
# 

# 
# #m2_AR
# n <- nrow(df_com)
# unique(df_com$LC_proj)
# n_j <- 3
# 
# fit_AR1_m2 <- stan(file="m2_AR.stan", 
#                 data = list(N = n,
#                             x = df_cc$Precip_mean,
#                             y = df_cc$EVI_mean,
#                             J = n_j,
#                             group = df_cc$LC_proj),
#                 chains=4, 
#                 cores=4,
#                 iter=2000)
# 
# summary(fit_AR1_m2)
# posterior2 <- as.matrix(fit_AR1_m2)


# #brms
# #AR(1)
# lm_ar<- brms::brm(data=df_cc,
#                   family=gaussian(),
#                   formula= log(EVI_mean) ~ arma(gr=LC_proj, p=1, q=1) + Precip_mean , #time= #cov=T 
#                   prior= prior(normal(0,1), class=b),
#                   sample_prior=T,
#                   cores=7,
#                   chains=2,
#                   iter=2000)
# 
# ?brm
# ?arma

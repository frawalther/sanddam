data {
  int<lower=1> N;
  vector[N] evi;//EVI, y
  real P[N]; //Precipitation(mean), predictor x
  vector [2] t[N];//time order
}

transformed data {
  real delta = 1e-9;

}

parameters {
  //hyperparameters
  real <lower=0> alpha; //cov kernel parameter , not intercept
  real <lower=0> rho; //length scale 
  
  //regression parameters
  real a; //intercept
  real b; // slope 

  // scaled latent GP effect 
 vector [N] eta; 
}

transformed parameters {
  vector [N] mu;
  { //compute variance-covariance matrix
    matrix[N,N] K; 
    matrix[N,N] L_K; //cholesky decomposition of VCV matrix (lower triangle)
    vector [N] gamma; //additive effect of GP
    
    K = cov_exp_quad(t, alpha, rho); //var-cov matrix
    
    //diagonal elements
    for (i in 1:N)
    K[i,i] = K[i,i] + delta;
    
    L_K = cholesky_decompose(K);
    gamma = L_K * eta;
    mu = inv_logit(gamma + a + P * b);
  }
}
model {    
    rho ~ inv_gamma(5,5);
    alpha~ std_normal();
    eta ~std_normal();
    a ~ normal(0,10)
    b ~ normal(0,10)
    
    evi ~ multi_normal_cholesky(mu,L_K);
}


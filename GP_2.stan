// latent variable GP 
data {
  int<lower=1> N; //nrows(df)
  vector<lower = -1, upper = 1>[N] evi;//EVI, y [N] or [t] ? 
  vector[N] P; //Precipitation(mean), predictor x
  vector[N] t;//time order
  // int<lower=1> LC [N];
  // int n_lc;
  //matrix [N,k] x;

}

transformed data {
  real delta = 1e-9;
}

parameters {
  //hyperparameters
  real <lower=0> rho; //length scale 
  real <lower=0> alpha; //cov kernel parameter , not intercept

  //regression parameters
  real a; //intercept
  real b; // slope 

  // scaled latent GP effect 
 vector [N] eta; 
}

transformed parameters {
  vector [N] mu;
  {    
    //compute variance-covariance matrix
    matrix[N,N] K; 
    matrix[N,N] L_K; //cholesky decomposition of VCV matrix (lower triangle)

    K = cov_exp_quad(t, alpha, rho) ; //var-cov matrix //error!
    
    //diagonal elements
    for (i in 1:N)
    K[i,i] = K[i,i] + delta;
    L_K = cholesky_decompose(K);
    
    vector [N] gamma; //additive effect of GP
    gamma = L_K * eta;
    mu = gamma + a + b * P; //error!  
  }
}

model { 
    matrix[N,N] L_K;

    rho ~ inv_gamma(5,5);
    alpha~ std_normal();
    eta ~std_normal();
    a ~ normal(0,10);
    b ~ normal(0,10);

    evi ~ multi_normal_cholesky(mu,L_K);

}


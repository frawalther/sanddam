// latent variable GP 
data {
  int<lower=1> N; //nrows(df)
  vector<lower = -1, upper = 1>[N] evi;//EVI, y [N] or [t] ? 
  vector [N] P; //Precipitation(mean)
  real time [N];//time order
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
  real <lower = 0> sigma; //variance

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
    matrix[N,N] K = cov_exp_quad(time, alpha, rho); 
    matrix[N,N] L_K; //cholesky decomposition of VCV matrix (lower triangle)
    vector [N] gamma; //additive effect of GP
    
    //diagonal elements
    for (i in 1:N)
      K[i,i] = K[i,i] + delta;
    L_K = cholesky_decompose(K);
    gamma = L_K * eta;
    mu = gamma + a + b * P;
  }
}

model { 

  rho ~ inv_gamma(5,5);
  alpha~ std_normal();
  eta ~ std_normal();
  a ~ normal(0,10);
  b ~ normal(0,10);

  evi ~ normal(mu, sigma);

}

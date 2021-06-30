//latent GP [for 1 SD]
data {
  int<lower=1> N;
  vector<lower=-1, upper=1> [N] evi;//EVI, y
  vector [N] P; //Precipitation(mean)
  real time [N];//time order

  //int LC [N];
}

transformed data {
//real delta = 1e-9;  
vector[N] jitter = rep_vector(1e-9, N);
}

parameters {
  real <lower=0> rho; //length scale 
  real <lower=0> alpha; //cov kernel parameter , not intercept
  real <lower=0> sigma; //noise standard deviation 

    //regression parameters
  real a; //intercept
  real b1; // slope 
  //real b2; // slope   
  
  vector[N] eta;
}

transformed parameters {
    vector [N] mu;
  {    
    //compute variance-covariance matrix
    matrix[N,N] K = cov_exp_quad(time, alpha, rho); 
    matrix[N,N] L_K = cholesky_decompose(add_diag(K, jitter)); //cholesky decomposition of VCV matrix (lower triangle)
    vector[N] gamma = L_K * eta; //additive effect of GP
    
    // //diagonal elements
    // for (i in 1:N) { 
    //   K[i,i] = K[i,i] + delta;
    // L_K = cholesky_decompose(K); 
    // gamma = L_K * eta;
    mu = gamma + a + b1 * P; // + b2 * LC
    //}
  }
}

model {
  rho ~ inv_gamma(5,5);
  alpha ~ std_normal();
  eta ~ std_normal();
  a ~ normal(0,10);
  b1 ~ normal(0,10);
  //b2 ~ normal(0,10);
  evi ~ normal(mu, sigma);

}




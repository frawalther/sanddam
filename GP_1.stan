data {
  int<lower=1> N;
  vector[N] evi;//EVI, y
  real P[N]; //Precipitation(mean), predictor x

}

transformed data {
  vector[N] mu = rep_vector(0,N);

}

parameters {
  real <lower=0> alpha; //cov kernel parameter , not intercept
  real <lower=0> rho; //

  real<lower=0> sigma;
}

model {
    matrix[N,N] L_K; //cholesky decomposition of VCV matrix (lower triangle)
    matrix[N,N] K = cov_exp_quad(P, alpha, rho); //var-cov matrix
    real sq_sigma = square(sigma);
    
    //diagonal elements
    for (n in 1:N)
    K[n,n] = K[n,n] + sq_sigma;
    
    L_K = cholesky_decompose(K);
    
    rho ~ inv_gamma(5,5);
    alpha~ std_normal();
    sigma ~std_normal();
    
    evi ~ multi_normal_cholesky(mu,L_K);
    
}



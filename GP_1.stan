//marginal likelihood GP 
data {
  int<lower=1> N;
  real P[N]; //Precipitation(mean), predictor x
  vector<lower=-1, upper=1> [N] evi;//EVI, y
  //int t[N]; //time order 
}

transformed data {
  vector[N] mu = rep_vector(0, N);    //mean function (vector of 0s)

}

parameters {
  real <lower=0> rho; //length scale 
  real <lower=0> alpha; //cov kernel parameter , not intercept
  real <lower=0> sigma; //noise standard deviation 
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



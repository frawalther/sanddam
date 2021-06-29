// latent variable GP 
data {
  int<lower=1> N; //nrows(df)
  vector<lower = -1, upper = 1>[N] evi;//EVI(mean)
  vector [N] P; //Precipitation(mean)
  real time [N];//time order
  
  // int<lower=1> LC [N];
  // int n_lc;

  int<lower=1> ngp; //Number of GPs to fit 
  int<lower=1, upper = ngp> gp_id [N]; //sampling unit ID, allowing for one GP per 
  int gp_sampsize [ngp]; //for each GP, how many data points are represented 
  int<lower=1, upper=N> max_gp_sampsize; // how many samples in the largest covariance kernel
}

transformed data {
  real delta = 1e-9;
  
  real gp_time [ngp, max_gp_sampsize];
  // we need a way to get back to the original index in order to fill in gamma later
  int gp_index[ngp, max_gp_sampsize];
  for(i in 1:ngp) {
    gp_time[i] = rep_array(0, max_gp_sampsize);
    gp_index[i] = rep_array(0, max_gp_sampsize);
  }
  
  {
    int last [N] = rep_array(0,N);
    for(i in 1:N) {
      gp_time[gp_id[i]][last[i] +1] = time[i];
      gp_index[gp_id[i]][last[i] +1] = i;
      //last[i] += 1; // stopped here 
    }
  }
}

parameters {
  //hyperparameters
  real <lower=0> rho; //length scale 
  real <lower=0> alpha; //cov kernel parameter , not intercept
  real <lower = 0> sigma; //variance

  //regression parameters
  real a; //intercept
  real b1; // slope predictor 1
  //real b2; //slope predictor 2

  // scaled latent GP effect 
 vector [N] eta; 
}

transformed parameters {
  vector [N] mu;
  {
    vector [N] gamma; //additive effect of GP
    // fit the GPs
    for(i in 1:ngp) {
      int ss = gp_sampsize[i];
      vector [ss] et_gp; // local copy of eta for just this GP
      vector [ss] gam_gp; //local copy of gamma for just this GP
      
          //compute variance-covariance matrix
      matrix[ss,ss] K = cov_exp_quad(gp_time[i, 1:ss], alpha, rho); 
      matrix[ss,ss] L_K; //cholesky decomposition of VCV matrix (lower triangle)

      //diagonals
      // find the right values for eta 
      for (j in 1:ss) {
        K[j,j] = K[j,j] + delta;
        et_gp[j] = eta[gp_index[i,j]];
        }
   
    L_K = cholesky_decompose(K);
    gam_gp = L_K * et_gp;
    for (j in 1:ss) {
    gamma[gp_index[i,j]] = gam_gp[j];
    }

    // add GP effect to the linear model
    mu = gamma + a + b1 * P;
  }
  } //right place? for(i in 1:ngp)
}
 


model { 

  rho ~ inv_gamma(5,5);
  alpha~ std_normal();
  eta ~ std_normal();
  a ~ normal(0,10);
  b1 ~ normal(0,10);

  evi ~ normal(mu, sigma);
}

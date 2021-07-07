// latent variable GP; unpooled 
data {
  int <lower=1> N; //nrows(df)
  vector<lower = -1, upper = 1>[N] evi;// response 
  vector [N] P; //Precipitation
  real time [N];//time order
  
  vector [N] presence; //wrong data type
  vector [N] lc; //wrong data type
  
  // int<lower=1> k_p; //Sand dam presence 
  // matrix[N, k_p] p;// as dummy/indicator variable (0,1)
  // 
  // int<lower=1> k_lc; // lc categories 
  // matrix[N, k_lc] lc;
  // int<lower=1, upper = k_lc> LC [N]; // LC as index variable
  // 
  // int<lower=1> D; //dimensionality of predictors?
  // matrix[N, D] X;

  int<lower=1> ngp; //Number of GPs to fit 
  int<lower=1, upper = ngp> gp_id [N]; //sampling unit ID, allowing for one GP per 
  int gp_sampsize [ngp]; //for each GP, how many data points are represented 
  int<lower=1, upper=N> max_gp_sampsize; // how many samples in the largest covariance kernel
}

transformed data {
  real delta = 1e-9;
  
  /*
    With gp_time, we transform the time variable, which is unsorted, into a matrix with the
    values arranged for each individual GP
    This allows us, in the GP loop later, to pull out only the times for each GP, without having
    To search all of the data for the right data points
  */
  real gp_time [ngp, max_gp_sampsize];

  /*
    gp_index is in a similar format to gp_time, but it keeps track of the index of the original values
    this way, when we finally compute a gamma for each GP, we can assign it back to the right
    data point
  */
  int gp_index[ngp, max_gp_sampsize];

  // start by filling them in with zeros
  for(i in 1:ngp) {
    gp_time[i] = rep_array(0, max_gp_sampsize);
    gp_index[i] = rep_array(0, max_gp_sampsize);
  }
  
  {
    // last keeps track of the index that was last used for each GP
    int last [ngp] = rep_array(0,ngp);
    for(i in 1:N) {
      int j = gp_id[i];
      gp_time[j, last[j] + 1] = time[i];
      gp_index[j, last[j] + 1] = i;
      last[j] += 1;
    }
  }
}

parameters {
  //hyperparameters
  vector <lower=0> [ngp] rho; //length scale 
  vector <lower=0> [ngp] alpha; //cov kernel parameter , not intercept
  real <lower = 0> sigma; //variance

  //regression parameters
  real a; //intercept
  real b1; // slope predictor 1 (P)
  real b2; //slope predictor 2 (presence)
  real b3; //slope predictor 3 (lc)

  // scaled latent GP effect 
 vector [N] eta; 

 // //parameter for presence
 // vector[k_p] b_p;
 // // 
 // // parameter for LC
 // vector[k_lc] b_lc;
}

transformed parameters {
  vector [N] mu;
  { // anonymous block to keep some variables hidden
    vector [N] gamma; //additive effect of GP
    // fit the GPs
    for(i in 1:ngp) {
      int ss = gp_sampsize[i];
      vector [ss] et_gp; // local copy of eta for just this GP
      vector [ss] gam_gp; //local copy of gamma for just this GP
      
          //compute variance-covariance matrix
      matrix[ss,ss] K = cov_exp_quad(gp_time[i, 1:ss], alpha[i], rho[i]); 
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

    } // end for loop
  // add GP effect to the linear model
  //mu = gamma + a + b1 * P + b_p * X + b_lc * LC;
  mu = gamma + a + b1 * P + b2 * presence + b3 * lc; 
  } // end anonymous block
}
 
model { 
//priors
  rho ~ inv_gamma(5,5);
  alpha ~ std_normal();
  eta ~ std_normal();
  sigma ~ cauchy(0, 10);
  a ~ normal(0,10);
  b1 ~ normal(0,10);
  b2 ~ normal(0,10);
  b3 ~ normal(0,10);
  // b_p ~ normal(0,10);
  // b_lc ~ normal(0,10);
//likelihood
  evi ~ normal(mu, sigma);
}


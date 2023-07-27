// Estimating probability of mudskippers' habitat
// We referred to the following page and refined codes below.
// https://atsa-es.github.io/atsa-labs/sec-marss-fitting-with-stan.html
// 
// We estimate our target variables while using existing data only 
// and skipping NA. For the purpose, we provide the data as follows.
data {
  int<lower=0> TT; // length of experiment period (TT = 43); Total Time
  int<lower=0> N; // N. of individual (N = 8)
  int<lower=0> n_position; // N. of non-NA values in y; Position of non-NA values
  int<lower=0> col_index_position[n_position]; // column index of non-NA vals
  int<lower=0> row_index_position[n_position]; // row index of non-NA vals
  vector[n_position] y; // all of the observed values excluding NAs 
}
parameters {
  vector<lower=0>[N] x0; // initial states of local level
  real v; // trend element
  vector[N] w[TT]; // Individual process deviation varies over time.
  real<lower=0> sd_q; // Process variance is fixed.
  real<lower=0> sd_r[N]; // Observation variance varies over time.
}
// 
transformed parameters {
  // process model
  vector[N] x[TT]; // refed as x[TT,N]
  for(i in 1:N){
    x[1,i] = x0[i] + v + w[1,i];
    for(t in 2:TT) {
      x[t,i] = x[t-1,i] + v + w[t,i];
    }
  }
}
// 
// A simple local level + trend model
model {
  // prior
  sd_q ~ cauchy(0,5);
  // model
  for(i in 1:N){
    // prior
    x0[i] ~ normal(y[i],100); // assume no missing y[1]
    sd_r[i] ~ cauchy(0,5);
    for(t in 1:TT){
    w[t,i] ~ normal(0, sd_q);
    }
  }
  // prior
  v ~ normal(0,10);
  // Observation space
  for(i in 1:n_position){
    y[i] ~ normal(x[col_index_position[i], row_index_position[i]], sd_r[row_index_position[i]]);
  }
}
// 
generated quantities {
  // 
  vector[n_position] log_likelihood;
  vector[N] yhat[TT]; // refed as yhat[TT,N]。
  // likelyhood
  for (n in 1:n_position) log_likelihood[n] = normal_lpdf(y[n] | x[col_index_position[n], row_index_position[n]], sd_r[row_index_position[n]]);
  // estimate Y using the estimated results above
  for(i in 1:N){
    for(t in 1:TT){
      yhat[t,i] = normal_rng(x[t,i], sd_r[i]);
      }
      }
      }
// END

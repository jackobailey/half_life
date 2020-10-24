// Exponential Decay Model
data {
  int<lower = 1> N; // Number of cases in the CMS data
  int<lower = 1> T; // Number of days in GDP data
  int<lower = 1> M; // Number of months in the CMS data
  int<lower = 1> C; // Max time comparisons in the CMS data
  vector[N] w8; // Respondent weights
  int<lower = 0> vote[N]; // DV: Incumbent voting
  int<lower = 1> month[N]; // Month survey occurred
  int<lower = 1> time[N]; // Number of time comparisons
  int<lower = 1> from[N]; // Survey date index
  int<lower = 1> to[N,C]; // Matrix of time comparisons,
  vector[T] gdp; // Vector of GDP values
}
parameters {
  vector[M] alpha; // Intercept
  real<lower = 0> sigma_alpha; // Intercept variability
  real beta0; // Initial economic voting effect
  real lambda; // Decay constant
}
model{
  
  // Initialise gdp change parameter and linear predictor
  vector[N] delta_gdp;
  vector[N] delta_t;
  vector[N] mu;
  
  // Compute change in GDP
  for(n in 1:N){
    for(t in 1:time[n]){
      delta_gdp[n] = ((gdp[from[n]] - gdp[to[n, t]])/gdp[to[n, t]])*100;
      delta_t[n] = from[n]-to[n, t];
      mu[n] = alpha[month[n]] + beta0 * ((exp(-lambda*delta_t[n]) * delta_gdp[n])/exp(-lambda*delta_t[n]));
    }
  }
  
  // Adaptive autoregressive prior on alpha
  target += normal_lpdf(alpha[1] | 0, 1.5);
  for(m in 2:M){
    target += normal_lpdf(alpha[m] | alpha[m-1], sigma_alpha);
  }
  target += exponential_lpdf(sigma_alpha | 2);
  
  // Priors
  target += normal_lpdf(beta0 | 0, 0.5);
  target += normal_lpdf(lambda | 0, 0.5);
  
  // Likelihood functions
  for(n in 1:N){
    target += w8[n] * (bernoulli_logit_lpmf(vote[n] | mu[n]));
  }
}

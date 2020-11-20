data {
  int<lower=1> N;// number of years 
  int<lower=0> M;// number of simulated years
  vector[N+M] C;// catch in tonnes per year
  vector[N+M] I;// CPUE
}

parameters {
  real<lower=1> K;// carrying capacity
  real<lower=0> r;// growth rate
  real<lower=0> q;// catchability
  real<lower=0> sigma2;// state process sd
  real<lower=0> tau2;// observation process sd
  vector<lower=0>[N+M] P;
}

transformed parameters {
  real<lower=0> Q;
  vector<lower=0>[N+M] B;

  Q = q*K;
  
  for (i in 1:(N+M)) { 
    B[i] = P[i]*K; 
    }
  
}

model {
r ~ lognormal(-1.38,0.51); // prior for growth rate
K ~ lognormal(5.04,0.5162); // prior for carrying capacity
q ~ inv_gamma(0.001,0.001); // prior for catchability
sigma2 ~ inv_gamma(3.79, 0.0102); // prior for sd of state process
tau2 ~ inv_gamma(1.71, 0.0086); // prior for sd of observation process
P[1] ~ lognormal(log(0.9),sqrt(sigma2))T[0.5,2.0]; //prior for initial population size

// state process
for (i in 2:(N+M)) {
P[i] ~ lognormal(fmax(log(P[i-1] + r*P[i-1]*(1-P[i-1]) - C[i-1]/K),log(0.01)),sqrt(sigma2));
}

// observation process
for (i in 1:(N+M)) { 
I[i] ~ lognormal(log(q*B[i]),sqrt(tau2));
}
}

generated quantities {
vector[N] log_lik;
real<lower=0> MSP;
real<lower=0> B_MSP;

  for (i in 1:N) {
    log_lik[i] = lognormal_lpdf(I[i] | log(q*B[i]) , sqrt(tau2));
}
  MSP = r*K/4;
  B_MSP = K/2;

}

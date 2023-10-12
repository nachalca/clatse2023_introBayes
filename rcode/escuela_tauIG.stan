data {
  int<lower=0> J; // cantidad de escuelas
  real y[J]; // efectos estimados
  real<lower=0> sigma[J]; // se de los efectos
  real<lower=0> a;
  real<lower=0> b;
}
parameters {
  real mu; // media poblacional del programa
  real<lower=0> tau; // variabilidad entre escuelas
  vector[J] eta; // error a nivel de escuela
}
transformed parameters {
  vector[J] muj; // school effects
  muj <- mu + tau*eta;
}
model {
  tau ~ inv_gamma(a, b);
  eta ~ normal(0, 1); // theta ~ normal(mu, tau)
  y ~ normal(muj, sigma);
}

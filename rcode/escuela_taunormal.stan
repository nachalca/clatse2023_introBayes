data {
  int<lower=0> J; // cantidad de escuelas
  real y[J]; // efectos individuales
  real<lower=0> sigma[J]; // se de los efectos
}
parameters {
  real mu; // media poblacional del programa
  real<lower=0> tau; // variabilidad entre escuelas
  vector[J] eta; // error a nivel de escuela
}
transformed parameters {
  vector[J] muj; // efectos estimados
  muj <- mu + tau*eta;
}
model {
  tau ~ normal(0, 15);
  eta ~ normal(0, 1); // muj ~ normal(mu, tau)
  y ~ normal(muj, sigma);
}

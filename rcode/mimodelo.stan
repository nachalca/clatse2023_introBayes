// Ejemplo inicial de STAN
// bloque de datos y/o parametros
data {
  int<lower=0> N; 
  int y[N];
}
// bloque para definir parametros y su espacio
parameters{
  real<lower=0, upper=1> theta;
}
// Bloque del modelo: previas y modelo para datos
model {
  theta ~ normal(.5, .2);
  y ~ binomial(30, theta);
}


data {
   int<lower=0> S;   // cantidad de secciones en Montevideo
            
   real p[S]; // estimaciones directas del muestreo
   real<lower=0> SE[S]; // error estandar de la estimación directa
   
   int<lower=0> k; // total de covariables (incluye el intercepto)
   matrix[S,k] X; // Variables explicativas 
}
parameters {
   vector[k] betas;
   real<lower=0> sigmas;
   real s[S];  // efecto aleatorio en los theta
}
transformed parameters {
   real thetas0[S];
   real P[S];
   for (i in 1:S){
          thetas0[i] = X[i,] * betas;
          P[i] = inv_logit( thetas0[i] + s[i]);
   }
}
model {
   for (i in 1:k){
        betas[i] ~ normal(0, 3);
   }
   
   sigmas ~ normal(0, 3);
   
   for (i in 1:S) {
      s[i] ~ normal(0,sigmas); // para que logit(theta) ~ normal() 
      p[i] ~ normal(P[i], SE[i]);
   } 
}

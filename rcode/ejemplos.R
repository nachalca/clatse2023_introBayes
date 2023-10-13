
library(tidyverse)

# Instalación STAN ----------------------------------------------------
# install.packages( rstan )
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

library(rstan)
rstan_options(auto_write = TRUE)
# =====================


# Ejemplo Inicial STAN ----------------------------------------------------
# ponemos los datos en una lista con nombres
dt.ls <- list( N=length(y), y = y)

# Obtenemos simulaciones de la posterior
res = stan(file = 'muestreo.stan', data = dt.ls )

plot(res, plotfun='trace')

plot(res, plotfun='rhat', bins=50)

plot(res, plotfun='ess', bins=50)

plot(res, pars ='theta', bins=50)

# =====================


# Modelo 8 escuelas ----------------------------------------------------
dt <- data.frame(row.names=c("A","B","C","D","E","F","G","H"),
                 effect = c(28.39,7.94,-2.75,6.82,-.64,.63,18.01,12.16),
                 see = c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6))


# Modelo con previa normal para tau
fit.normal <- stan(file="rcode/escuela_taunormal.stan",
                   data=list(J=nrow(dt), y = dt$effect, sigma=dt$see))


summary(fit.normal)


# Comparación con modelo de previa IG(1,1) para tau
dig <- function(x, a, b) {
  sale = a*log(b) - lgamma(a) - (a+1)*log(x) - b/x 
  exp(sale)
}

a1 <- 1
fit.ig <- stan(file="rcode/escuela_tauIG.stan",
               data=list(J=nrow(dt), y = dt$effect, sigma=dt$see, a=a1, b=a1), iter = 5000)

tauig.samples <- rstan::extract(fit.ig, pars='tau')$tau |> as.numeric()
taunormal.samples <- rstan::extract(fit.normal, pars='tau')$tau |> as.numeric()

pl.ig <- ggplot() + 
  geom_histogram(aes(x=tauig.samples, y=after_stat(density) ), bins=50, fill='grey70') + 
  geom_function(data=data.frame(x=seq(0.00001,30, .05)), 
                fun= function(x) dig(x, a1, a1),  color='red') + 
  labs(x=expression(tau), y='Muestras posterior', title='Previa IG(1,1)') +
  theme_bw() 

pl.nr <- ggplot() + 
  geom_histogram(aes(x=taunormal.samples, y=after_stat(density) ), fill='grey70') + 
  geom_function(data=data.frame(x=seq(0,30, .5)), fun=function(x) 2*dnorm(x, sd=15), color='red') + 
  labs(x=expression(tau), y='Muestras posterior', title='Previa Normal(0, 15)') +
  theme_bw()

library(patchwork)
pl.nr+pl.ig
# =====================


# Modelo Fay-Herriot ----------------------------------------------------

library(rstan)
rstan_options(auto_write = TRUE)

# Armar datos y estimar el modelo
dd <- list(
  S = 24, p=insAlim$iaMoG, SE=insAlim$se, k = 1, X=matrix(1, ncol = 1, nrow=24)
)

fit1 <- stan(file = here('rcode/modelo2.stan'), data=dd, verbose = FALSE)

# Verificar convergencia
plot(fit1, plotfun='rhat', bins=50)
plot(fit1, plotfun='ess', bins=50)

# Graficar los P estimados
plot(fit1, pars ='P', bins=50, ci_level = 0.5)

ss.fit1 <- summary(fit1, pars='P')$summary |> data.frame()
insAlim$Phat <- ss.fit1$mean
insAlim$sePhat <- ss.fit1$sd

# Comparamos estimaciones directas con SAE
ggplot(insAlim ) + 
  geom_point( aes(Phat, iaMoG) )

# Dibujar mapa de Montevideo
secciones <- merge(seccCen, insAlim, by = "SECCION") %>% filter(DEPTO==1)
ggplot(secciones) +
  geom_sf(aes(fill = Phat)) +
  labs(title = "Inseguridad Alimentaria por secciones") +
  scale_fill_viridis_c() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom")

# =====================


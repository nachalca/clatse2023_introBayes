library(rstan)
rstan_options(auto_write = TRUE)

dt <- data.frame(row.names=c("A","B","C","D","E","F","G","H"),
                 effect = c(28.39,7.94,-2.75,6.82,-.64,.63,18.01,12.16),
                 see = c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6))


fit.normal <- stan(file="rcode/escuela_taunormal.stan",
                    data=list(J=nrow(dt), y = dt$effect, sigma=dt$see))


muj.samples <- rstan::extract(fit.normal, pars='muj')$muj

res <- data.frame( 
  escuela = rownames(dt), 
  effect  = apply(muj.samples, 2, median), 
  low = apply(muj.samples, 2, quantile, prob=0.025), 
  upp = apply(muj.samples, 2, quantile, prob=.975) 
  ) 


see.all <- with(dt, 1/sum(1/see^2) )|> sqrt()
muall <- with(dt, sum(effect/see^2)/sum(1/see^2)) 

res.all <- data.frame(escuela=rownames(dt), effect=muall, 
                      low=muall-1.96*see.all, upp=muall+1.96*see.all)

dt.sep <- dt |> rownames_to_column(var = 'escuela') |> mutate(low=effect-1.96*see, upp=effect+1.96*see) |> select(-see) 

list( sep = dt.sep,  
      todos = res.all,  
      jerar = res) |> 
  bind_rows(.id = 'metodo') |>  
  mutate( escuela = reorder(escuela, effect)) |> 
  ggplot() + 
  geom_pointrange(
    aes(x=effect, y=escuela, xmin=low, xmax=upp, color=metodo), 
    position= position_dodge(.2)) +
  geom_vline( xintercept =muall, linetype='dashed') +
  labs(x="Efecto", y = '') + 
  theme_bw() +
  theme( axis.text.y = element_text(size=I(20)) )  + 
  scale_color_viridis_d()
  


plot(fit.normal, plotfun='rhat', bins=50)
plot(fit.normal, plotfun='ess', bins=50)
plot(fit.normal, pars ='theta', bins=50)
summary(fit.normal)




plot(fit.normal, pars ='tau', bins=50, plotfun='hist')



#================
dig <- function(x, a, b) {
  sale = a*log(b) - lgamma(a) - (a+1)*log(x) - b/x 
  exp(sale)
}

a1 <- 1
fit.ig <- stan(file="rcode/escuela_tauIG.stan",
                   data=list(J=nrow(dt), y = dt$effect, sigma=dt$see, a=a1, b=a1), iter = 5000)

# plot(fit.ig, pars ='tau', bins=50, plotfun='hist')
tauig.samples <- rstan::extract(fit.ig, pars='tau')$tau |> as.numeric()
pl.ig <- ggplot() + 
  geom_histogram(aes(x=tauig.samples, y=after_stat(density) ), bins=50, fill='grey70') + 
  #geom_density(aes(x=tauig.samples)) +
  geom_function(data=data.frame(x=seq(0.00001,30, .05)), 
                fun= function(x) dig(x, a1, a1), 
                color='red') + 
  labs(x=expression(tau), y='Muestras posterior', title='Previa IG(1,1)') +
  theme_bw() 


taunormal.samples <- rstan::extract(fit.normal, pars='tau')$tau |> as.numeric()

pl.nr <- ggplot() + 
  geom_histogram(aes(x=taunormal.samples, y=after_stat(density) ), fill='grey70') + 
  geom_function(data=data.frame(x=seq(0,30, .5)), fun=function(x) 2*dnorm(x, sd=15), color='red') + 
  labs(x=expression(tau), y='Muestras posterior', title='Previa Normal(0, 15)') +
  theme_bw()

library(patchwork)
pl.nr+pl.ig












# Funcion para generar los datos
rgamma_glm <- function(n, mu, phi) {
  rgamma(n=n, shape=1/phi, scale=mu*phi)
}

gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rgamma_glm(n=n, mu=mu, phi=phi)
  data.frame(y=y, x=x)
}

phi <- 2
n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 10
results <- NULL

for (i in n) {
  for (j in 1:nrep) {
    datos <- gen_dat(n=i, b0=-1, b1=1, phi=phi)
    mod <- glm(y ~ x, data=datos, family=Gamma(link=log))
    # Para obtener Mean Deviance Estimator se usa:
    e1 <- mod$deviance / mod$df.residual
    # Para obtener Pearson Estimator se usa:
    w <- weights(mod, type="working")
    e <- residuals(mod, type="working")
    e2 <- sum(w * e^2) / df.residual(mod)
    # Para obtener Modified Profile Log-Likelihood Estimator se usa:
    e3 <- mod_ll_prof_phi_glm_gamma(mod, verbose=TRUE)
    # The maximum likelihood estimate of the shape parameter.
    e4 <- MASS::gamma.dispersion(mod)
    results <- rbind(results, c(i, e1, e2, e3, e4))
  }
}

colnames(results) <- c('n', 'mean_devi', 'pearson', 'modified', 'mle')

library(dplyr)
results <- as.data.frame(results)
res <- results %>%
  group_by(n) %>%
  summarise(mean_devi = mean(mean_devi),
            mean_pear = mean(pearson),
            mean_modi = mean(modified),
            mean_mle  = mean(mle)) 

res




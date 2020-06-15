# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modeol

rgamma_glm <- function(n, mu, k) {
  x <- rgamma(n=n, shape=k, scale=mu/k)
  return(x)
}

gen_dat <- function(n, b0, b1, k) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rgamma_glm(n=n, mu=mu, k=k)
  data.frame(y=y, x=x)
}

n <- 100
datos <- gen_dat(n=n, b0=-1, b1=1, k=0.5)

mod <- glm(y ~ x, data=datos, family=Gamma(link='log'))
summary(mod)

MASS::gamma.dispersion(mod) # para obtener phi
MASS::gamma.shape(mod)      # para obtener k
MASS:::gamma.shape.glm(mod) # para obtener k

sum((datos$y - fitted(mod))^2 / fitted(mod)^2) / (n-2)

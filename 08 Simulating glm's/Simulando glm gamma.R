# -------------------------------------------------------------------------
# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo
# -------------------------------------------------------------------------

# Modelo: Y~Gamma(mu, phi=dipsersion param) con log(mu) = b0 + b1 * x
# con b0=-1 y b1=1
# la covariable x ~ U(0, 1)

# Se va a usar la funcion rgamma_glm() y no rgamma()
# porque la parametrizacion de la fdp de dgamma() no 
# coincide con la fdp de los glm.
rgamma_glm <- function(n, mu, phi) {
  x <- rgamma(n=n, shape=1/phi, scale=mu*phi)
  return(x)
}

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rgamma_glm(n=n, mu=mu, phi=phi)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 100
datos <- gen_dat(n=n, b0=-1, b1=1, phi=2)
head(datos)

# Exploremos los datos
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) + 
  geom_point()

# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=Gamma(link='log'))
summary(mod)

coef(mod) # para obtener los betas
MASS::gamma.dispersion(mod) # para obtener phi Maximum Likelihood
MASS::gamma.shape(mod)      # para obtener k automaticamente
MASS:::gamma.shape.glm(mod) # para obtener k automaticamente

# Para estimar phi manualmente
sum((datos$y - fitted(mod))^2 / fitted(mod)^2) / (n-2)

# Reto --------------------------------------------------------------------

# Explorar el efecto del tamano de muestra n en las estimaciones.
# Para cada n usted debe simular 100 conjuntos de datos, estimar
# los parametros y almacenarlos.

# Aproveche el codigo de abajo para iniciar.

n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 100
results <- matrix(NA, ncol=4, nrow=nrep*length(n))
colnames(results) <- c('b0', 'b1', 'phi_auto', 'phi_manual')

# Crear un diagrama de dispersion del promedio de b0 versus n.
# Crear un diagrama de dispersion del promedio de b1 versus n.
# Crear un diagrama de dispersion del promedio de k auto versus n.
# Crear un diagrama de dispersion del promedio de k manual versus n.

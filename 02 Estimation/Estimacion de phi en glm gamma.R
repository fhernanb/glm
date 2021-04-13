# Para estimar el parametro de dispesion phi en un glm gamma
# se puede usar los siguientes metodos:

# 1) Maximum likelihood estimator
# 2) Modified Profile Log-Likelihood Estimator
# 3) Mean Deviance Estimator
# 4) Pearson Estimator

# que estan en seccion 6.8 de Dunn & Smyth (2018) pagina 252


# Ejemplo con datos reales ------------------------------------------------
library(GLMsData)
data(trees)
mod <- glm(Volume ~ log(Height) + log(Girth), data=trees,
           family=Gamma(link=log))

# Maximum likelihood estimate
MASS::gamma.dispersion(mod)

# Modified Profile Log-Likelihood Estimator
source("https://tinyurl.com/ya2q6v7e")
mod_prof_ll_phi_glm(mod, verbose=TRUE)

# Mean Deviance Estimator
mod$deviance / mod$df.residual

# Pearson Estimator
w <- weights(mod, type="working")
e <- residuals(mod, type="working")
sum(w * e^2) / df.residual(mod)

# Otra forma para obtener Pearson Estimator es:
summary(mod)$dispersion

# Ejemplo con datos simulados ---------------------------------------------

# Funcion para generar obs de un glm con y ~ gamma(mu, phi)
# con la parametrizacion de un glm
rgamma_glm <- function(n, mu, phi) {
  rgamma(n=n, shape=1/phi, scale=mu*phi)
}

# Funcion para generar el dataframe con los datos simulados
# siguiendo el siguiente modelo:
# y ~ gamma(mu, phi)
# log(mu) = b0 + b1 * x
# phi = valor
# x ~ U(0, 1)

gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n, min=0, max=1)
  mu <- exp(b0 + b1 * x)
  y <- rgamma_glm(n=n, mu=mu, phi=phi)
  return(data.frame(y=y, x=x))
}

n <- 600 # Vamos a simular muchas observaciones
phi <- 2 # Valor de phi VERDADERO

# Creando el dataset
datos <- gen_dat(n=n, b0=-1, b1=1, phi=phi)
head(datos, n=8)

# Vamos a ajustar el modelo
mod <- glm(y ~ x, data=datos, family=Gamma(link=log))

# The maximum likelihood estimate of the shape parameter.
MASS::gamma.dispersion(mod)

# Para obtener Modified Profile Log-Likelihood Estimator se usa:
source("https://tinyurl.com/ya2q6v7e")
mod_prof_ll_phi_glm(mod, verbose=TRUE)

# Para obtener Mean Deviance Estimator se usa:
mod$deviance / mod$df.residual

# Para obtener Pearson Estimator se usa:
summary(mod)$dispersion


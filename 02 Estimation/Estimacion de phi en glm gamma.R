# Para estimar el parametro de dispesion phi en un glm gamma
# se puede usar los siguientes metodos:

# 1) Maximum likelihood estimator
# 2) Modified Profile Log-Likelihood Estimator
# 3) Mean Deviance Estimator
# 4) Pearson Estimator

# que estan en seccion 6.8 de Dunn & Smyth (2018) pagina 252


# Ejemplo -----------------------------------------------------------------
library(GLMsData)
data(trees)
mod <- glm(Volume ~ log(Height) + log(Girth), data=trees,
           family=Gamma(link=inverse))

# The maximum likelihood estimate of the shape parameter.
MASS::gamma.dispersion(mod)

# Para obtener Modified Profile Log-Likelihood Estimator se usa:
source("https://tinyurl.com/ydcf3a38")
mod_ll_prof_phi_glm_gamma(mod, verbose=TRUE)

# Para obtener Mean Deviance Estimator se usa:
mod$deviance / mod$df.residual

# Para obtener Pearson Estimator se usa:
w <- weights(mod, type="working")
e <- residuals(mod, type="working")
sum(w * e^2) / df.residual(mod)

# Otra forma para obtener Pearson Estimator es:
summary(mod)$dispersion

# Ejemplo con datos simulados ---------------------------------------------

# Funcion para generar obs de un glm gamma
rgamma_glm <- function(n, mu, phi) {
  rgamma(n=n, shape=1/phi, scale=mu*phi)
}

# Funcion para generar el dataframe
gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- rgamma_glm(n=n, mu=mu, phi=phi)
  data.frame(y=y, x=x)
}

n <- 1000
phi <- 2

datos <- gen_dat(n=n, b0=-1, b1=1, phi=phi)
mod <- glm(y ~ x, data=datos, family=Gamma(link=log))

# The maximum likelihood estimate of the shape parameter.
MASS::gamma.dispersion(mod)

# Para obtener Modified Profile Log-Likelihood Estimator se usa:
source("https://tinyurl.com/ydcf3a38")
mod_ll_prof_phi_glm_gamma(mod, verbose=TRUE)

# Para obtener Mean Deviance Estimator se usa:
mod$deviance / mod$df.residual

# Para obtener Pearson Estimator se usa:
summary(mod)$dispersion


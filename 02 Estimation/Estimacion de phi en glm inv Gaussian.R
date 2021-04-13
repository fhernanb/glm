# Para estimar el parametro de dispesion phi en un glm inv Gaussian
# se puede usar los siguientes metodos:

# 1) Maximum likelihood estimator
# 2) Modified Profile Log-Likelihood Estimator
# 3) Mean Deviance Estimator
# 4) Pearson Estimator

# que estan en seccion 6.8 de Dunn & Smyth (2018) pagina 252

# Ejemplo -----------------------------------------------------------------
library(GLMsData)
data(lime)
mod <- glm(Foliage ~ Origin * log(DBH),
           family=inverse.gaussian(link="log"), data=lime)

# Maximum likelihood estimate
deviance(mod)/length(lime$Foliage)

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

# Funcion para generar obs de un glm gamma

# Funcion para generar el dataframe con los datos simulados
# siguiendo el siguiente modelo:
# y ~ gamma(mu, phi)
# log(mu) = b0 + b1 * x
# phi = valor
# x ~ U(0, 1)

gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n, min=0, max=1)
  mu <- exp(b0 + b1 * x)
  y <- statmod::rinvgauss(n=n, mean=mu, disp=phi)
  return(data.frame(y=y, x=x))
}

n <- 50 # Vamos a simular muchas observaciones
phi <- 3 # Valor de phi VERDADERO

# Creando el dataset
datos <- gen_dat(n=n, b0=-1, b1=1, phi=phi)
head(datos, n=8)

# Vamos a ajustar el modelo
mod <- glm(y ~ x, data=datos, family=inverse.gaussian(link=log))

# Maximum likelihood estimate
deviance(mod)/length(datos$y)

# Modified Profile Log-Likelihood Estimator
source("https://tinyurl.com/ya2q6v7e")
mod_prof_ll_phi_glm(mod, verbose=TRUE)

# Mean Deviance Estimator
mod$deviance / mod$df.residual

# Otra forma para obtener Pearson Estimator es:
summary(mod)$dispersion

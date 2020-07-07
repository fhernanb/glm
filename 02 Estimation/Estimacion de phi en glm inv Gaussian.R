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

# Funcion para generar el dataframe
gen_dat <- function(n, b0, b1, phi) {
  x <- runif(n=n)
  mu <- exp(b0 + b1 * x)
  y <- statmod::rinvgauss(n=n, mean=mu, disp=phi)
  data.frame(y=y, x=x)
}

n <- 50
phi <- 3

datos <- gen_dat(n=n, b0=-1, b1=1, phi=phi)
mod <- glm(y ~ x, data=datos, 
           family=inverse.gaussian(link=log))

# Maximum likelihood estimate
deviance(mod)/length(datos$y)

# Modified Profile Log-Likelihood Estimator
source("https://tinyurl.com/ya2q6v7e")
mod_prof_ll_phi_glm(mod, verbose=TRUE)

# Mean Deviance Estimator
mod$deviance / mod$df.residual

# Otra forma para obtener Pearson Estimator es:
summary(mod)$dispersion

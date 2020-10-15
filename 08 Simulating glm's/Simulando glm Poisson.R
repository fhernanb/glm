# -------------------------------------------------------------------------
# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo
# -------------------------------------------------------------------------

# Modelo: Y~Poisson(lambda) con log(lambda) = b0 + b1 * x
# donde x ~ U(0, 1)
# con b0=-1 y b1=2
# la covariable x ~ U(0, 1)

# Funcion para generar los datos
gen_dat <- function(n, b0, b1) {
  x <- runif(n=n, min=0, max=1)
  lambda <- exp(b0 + b1 * x)
  y <- rpois(n=n, lambda=lambda)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 100
datos <- gen_dat(n=n, b0=-1, b1=2)
head(datos)

# Exploremos los datos
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) + 
  geom_point()

# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=poisson(link='log'))
summary(mod)
coef(mod)     # Los valores estimados son cercanos a los verdaderos

# Envelopes
fit.model <- mod
source("https://www.ime.usp.br/~giapaula/envel_pois")

# Reto --------------------------------------------------------------------

# Explorar el efecto del tamano de muestra n en las estimaciones.
# Para cada n usted debe simular 100 conjuntos de datos, estimar
# los parametros y almacenarlos.

# Aproveche el codigo de abajo y cambie xxx por la expresion correcta.

n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 100
results <- matrix(NA, ncol=2, nrow=nrep*length(n))
colnames(results) <- c('xxx', 'b1')
dim(results) # para ver la dimension de la matriz donde se almacenaran

for (i in n) {
  for (j in 1:nrep) {
    xxx <- gen_dat(n=xxx, b0=-1, b1=1)
    mod <- glm(xxx)
    results[xxx] <- coef(xxx)
  }
}

# Crear un diagrama de dispersion del promedio de bi versus n.
# Crear un diagrama de dispersion del ECM de bi versus n.


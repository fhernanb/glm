# -------------------------------------------------------------------------
# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo
# -------------------------------------------------------------------------

# Modelo: Y~Normal(mu, s2) con mu = 1 / (b0 + b1 * x)
# con b0=-1 y b1=1 y s2=9
# la covariable x ~ U(0, 1)

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, sd) {
  x <- runif(n=n, min=0, max=1)
  media <- 1 / (b0 + b1 * x)
  y <- rnorm(n=n, mean=media, sd=sd)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 100
datos <- gen_dat(n=n, b0=-1, b1=1, sd=3)
head(datos)

# Exploremos los datos
library(ggplot2)

ggplot(datos, aes(x=x, y=y)) + 
  geom_point()

# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=gaussian(link='inverse'))
summary(mod)
coef(mod)     # Los valores estimados son cercanos a los verdaderos

# Reto --------------------------------------------------------------------

# Explorar el efecto del tamano de muestra n en las estimaciones.
# Para cada n usted debe simular 100 conjuntos de datos, estimar
# los parametros y almacenarlos.

# Aproveche el codigo de abajo y cambie xxx por la expresion correcta.

n <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
nrep <- 100
results <- matrix(NA, ncol=3, nrow=nrep*length(n))
colnames(results) <- c('xxx', 'b1', 'sigma2')
dim(results) # para ver la dimension de la matriz donde se almacenaran

for (i in n) {
  for (j in 1:nrep) {
    xxx <- gen_dat(n=xxx, b0=-1, b1=1)
    mod <- glm(xxx)
    results[xxx] <- xxx ---> ahi se almacenan b0, b1 y sigma2
  }
}

# Crear un diagrama de dispersion del promedio de bi versus n.
# Crear un diagrama de dispersion del promedio de sigma2 versus n.
# Crear un diagrama de dispersion del ECM de bi versus n.
# Crear un diagrama de dispersion del ECM de sigma2 versus n.


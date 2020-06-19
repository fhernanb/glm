# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Modelo: Y~Binomial(n=10, pi) con logit(pi) = b0 + b1 * x
# con b0=-1 y b1=1

# Funcion para generar los datos
gen_dat <- function(n, b0, b1) {
  x <- runif(n=n)
  eta <- b0 + b1 * x
  pi <- exp(eta) / (1 + exp(eta))
  y <- rbinom(n=n, size=10, prob=pi)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 100
datos <- gen_dat(n=n, b0=-1, b1=1)
head(datos)


# Reto --------------------------------------------------------------------

# Ajustar el modelo para recuperar los parametros

mod <- glm(y ~ x, data=datos, family=binomial(link='logit'))
summary(mod)
coef(mod)

attach(datos)
mod <- glm(xxxxxxxxxxxxxxxxxxxxxxxx)
summary(mod)
coef(mod)

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


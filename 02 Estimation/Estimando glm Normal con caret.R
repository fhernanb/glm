# En este ejemplo se simulan datos de un glm y se 
# estiman los parametros del modelo

# Modelo: Y~Normal(mu, s2) con mu = 1 / (b0 + b1 * x)
# con b0=-1 y b1=1 y s2=9

# Funcion para generar los datos
gen_dat <- function(n, b0, b1, sd) {
  x <- runif(n=n)
  media <- b0 + b1 * x
  y <- rnorm(n=n, mean=media, sd=sd)
  data.frame(y=y, x=x)
}

# Generando los datos
n <- 1000
datos <- gen_dat(n=n, b0=-1, b1=1, sd=3)
head(datos)

# Ajustado el modelo
mod <- glm(y ~ x, data=datos, family=gaussian)
summary(mod)
coef(mod)     # Los valores estimados son cercanos a los verdaderos

# Ajustando el modelo con caret
library(caret)

tuned <- train(y ~ x, data = datos, method = "lm")
tuned
tuned$bestTune
tuned$finalModel


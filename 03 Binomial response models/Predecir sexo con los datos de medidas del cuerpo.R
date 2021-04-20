# Ejemplo de regresion logistica usando los datos medidas cuerpo

# Leyendo los datos -------------------------------------------------------
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=url, sep="\t", header=TRUE)
head(datos, n=5)

# Haciendo algunas modificaciones -----------------------------------------

# creando la variable Y usando ceros y unos.
# Y=1 para hombre yt Y=0 para mujer
library(dplyr)
datos <- datos %>% mutate(y=case_when(Sexo == "M" ~ 1,
                                      Sexo == "F" ~ 0))
datos %>% head()

# Explorando y versus otras variables -------------------------------------
library(ggplot2)

ggplot(data=datos) + 
  geom_point(mapping=aes(x=Peso, y=y, color=Sexo))


# Ajustando el modelo -----------------------------------------------------
mod <- glm(y ~ Peso, data=datos, family=binomial(link='logit'))
summary(mod)
coef(mod)

# agregando la curva al diagrama de dispersion
curva <- function(x) {
  eta <- -35.239067 + 0.552851 * x # predictor lineal
  probabilidad <- exp(eta) / (1 + exp(eta))
  return(probabilidad)
}

ggplot(data=datos) + 
  geom_point(mapping=aes(x=Peso, y=y, color=Sexo)) +
  geom_function(fun=curva, colour="black", lty="dashed")

# Explorando el efecto de beta1 en pi y en log(pi/(1-pi))
# supongamos dos personas, una de 63 y otra de 64 kilogramos
x <- 63
p1 <- predict(mod, data.frame(Peso=x), type='response')
p2 <- predict(mod, data.frame(Peso=x+1), type='response')
p1
p2

# Calculemos el odds para persona 1
p1 / (1 - p1)

# Calculemos el odds para persona 2 de dos formas diferentes
p2 / (1 - p2)
exp(0.552851) * p1 / (1 - p1) # esto es exp(beta) * odds persona 1

# Calculemos la razon de odds
(p1 / (1 - p1)) / (p2 / (1 - p2))

# Vamos a ilustrar los resultados anteriores con una figura
ggplot(data=datos) + 
  geom_point(mapping=aes(x=Peso, y=y, color=Sexo)) +
  geom_function(fun=curva, colour="black", lty="dashed") + 
  geom_segment(aes(x=x, y=0, xend=x, yend=p1), arrow=arrow(), col="blue") +
  geom_segment(aes(x=x, y=p1, xend=45, yend=p1), arrow=arrow(), col="blue") +
geom_segment(aes(x=x+1, y=0, xend=x+1, yend=p2), arrow=arrow(), col="blue") +
  geom_segment(aes(x=x+1, y=p2, xend=45, yend=p2), arrow=arrow(), col="blue")
  

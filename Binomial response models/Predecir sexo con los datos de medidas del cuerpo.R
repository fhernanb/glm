# Ejemplo de regresion logistica usando los datos medidas cuerpo

# leyendo los datos
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=url, sep="\t", header=TRUE)
head(datos, n=5)

# creando y
datos$y <- ifelse(datos$Sexo == 'M', 1, 0)

# dibujando los datos
with(datos, plot(y=jitter(y, amount=0.03), x=Peso, las=1, pch=20))

# modificando el sexo
datos$Sexo <- as.factor(datos$Sexo)
levels(datos$Sexo)

# ajustando el modelo
mod1 <- glm(y ~ Peso, data=datos, family=binomial(link='logit'))
coef(mod1)

# agregando la curva al diagrama de dispersion
curva <- function(x) {
  eta <- -35.239067 + 0.552851*x
  probabi <- exp(eta) / (1 + exp(eta))
  return(probabi)
}

with(datos, plot(y=y, x=Peso, pch=20, las=1))
grid()
curve(curva, add=TRUE, col='blue', lwd=3)

# Explorando el efecto de beta1 en pi y en log(pi/(1-pi))
x <- 63
p1 <- predict(mod1, data.frame(Peso=x), type='response')
p2 <- predict(mod1, data.frame(Peso=x+1), type='response')
p1
p2
p1 / (1 - p1)
exp(0.552851) * p1 / (1 - p1)
p2 / (1 - p2)
(p1 / (1 - p1)) / (p2 / (1 - p2))

0.57
0.57
0.57


# Ejemplo sobre cangrejas del libro de Foundations of linear and GLM
# de Agresti

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
datos <- read.table(url, header=T)
head(datos)

datos$color <- as.factor(datos$color)
datos$spine <- as.factor(datos$spine)

str(datos)

# Un poco de estadistica descriptiva
barplot(table(datos$y), las=1, col='lightgreen',
        xlab='Numero de satelites', ylab='Frecuencia')

mean(datos$y)
var(datos$y)

with(datos, plot(y ~ weight, pch=20, las=1))
with(datos, plot(y ~ width, pch=20, las=1))
with(datos, boxplot(y ~ color))
with(datos, boxplot(y ~ spine))

# Modelo de inicio
mod0 <- glm(y ~ color * spine + weight * width + 
              color * weight + spine * weight + 
              color * width + spine * width, 
            family=poisson, data=datos)
summary(mod0)

car::Anova(mod0)

# Nuevo modelo seleccionando manualmente
mod1 <- glm(y ~ weight + color * spine, 
            family=poisson, data=datos)
summary(mod1)

y1_hat <- fitted(mod1)
plot(x=datos$y, y=y1_hat, las=1)
abline(a=0, b=1, lty="dashed")
cor(y1_hat, datos$y)

# Aplicando seleccion de variables
library(MASS)
mod2 <- stepAIC(object=mod0, direction="backward", k=4)
summary(mod2)

y2_hat <- fitted(mod2)
plot(x=datos$y, y=y2_hat, las=1)
abline(a=0, b=1, lty="dashed")
cor(y2_hat, datos$y)

car::Anova(mod2)

mod3 <- glm(y ~ color + spine + weight + width, 
            family=poisson, data=datos)
summary(mod3)

y3_hat <- fitted(mod3)
plot(x=datos$y, y=y3_hat, las=1)
abline(a=0, b=1, lty="dashed")
cor(y3_hat, datos$y)

par(mfrow=c(2, 2))
plot(mod3)

# Envelopes
fit.model <- mod3
source("https://www.ime.usp.br/~giapaula/envel_pois")



# Ejemplo 1 ---------------------------------------------------------------

# Usando los datos de cangrejos
url <- "http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)

# Se desea modelar la variable Y
# Y=0 si la cangreja NO tiene satelites pegados
# Y=1 si la cangreja SI tiene satelites pegados

# H0: un modelo binomial sin covariables es apropiado
# H1: un modelo binomial con width es mejor

fit0 <- glm(y ~ 1, family=binomial, data=Crabs)
fit1 <- glm(y ~ width, family=binomial, data=Crabs)

# Prueba razon de verosimilitud manual
lrt <- -2*(logLik(fit0) - logLik(fit1))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=3-2, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="Chisq", dispersion=1)

# Conclusion: ambas pruebas generan los mismos resultados

# Ejemplo 2 ---------------------------------------------------------------

# Usando los datos de cangrejos
# color: 1, medium light; 2, medium; 3, medium dark; 4, dark 
# spine condition: 1, both good; 2, one worn or broken; 3, both worn or broken
# Width, carapace width (cm); 
# Weight, weight (kg).

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)
Crabs$color <- as.factor(Crabs$color)
Crabs$spine <- as.factor(Crabs$spine)

# Se desea modelar la variable Y que representa el numero de satelites

# H0: un modelo Poisson con weight y color esta bien
# H1: un modelo Poisson con weight, color, width y spine es mejor

fit1 <- glm(y ~ weight + color, family=poisson, data=Crabs)
fit2 <- glm(y ~ weight + color + width + spine, family=poisson, data=Crabs)

# Coeficientes
coef(fit1)
coef(fit2)

# Prueba razon de verosimilitud manual comparando fit1 y fit2
lrt <- -2*(logLik(fit1) - logLik(fit2))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-5, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit1, fit2, test="Chisq", dispersion=1)

# Interpretacion: width y spine no mejoran el modelo fit 1


# Ejemplo de comparaciones secuenciales -----------------------------------

# Usando los datos de cangrejos
# color: 1, medium light; 2, medium; 3, medium dark; 4, dark 
# spine condition: 1, both good; 2, one worn or broken; 3, both worn or broken
# Width, carapace width (cm); 
# Weight, weight (kg).

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)
Crabs$color <- as.factor(Crabs$color)
Crabs$spine <- as.factor(Crabs$spine)

# Se desea modelar la variable Y que representa el numero de satelites
# Queremos comparar los siguientes modelos:
# fit0: sin covariables
# fit1: con weight
# fit2: con weight y width
# fit3: con weight, width y color
# fit4: con todas las covariables

# H0: el modelo "menor" esta bien
# H1: el modelo "mayor" es mejor

fit0 <- glm(y ~ 1, family=poisson, data=Crabs)
fit1 <- glm(y ~ weight, family=poisson, data=Crabs)
fit2 <- glm(y ~ weight + width, family=poisson, data=Crabs)
fit3 <- glm(y ~ weight + width + color, family=poisson, data=Crabs)
fit4 <- glm(y ~ weight + width + color + spine, family=poisson, data=Crabs)

# Prueba razon de verosimilitud manual comparando fit0 y fit1
lrt <- -2 * (logLik(fit0) - logLik(fit1))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=2-1, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit1 y fit2
lrt <- -2 * (logLik(fit1) - logLik(fit2))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=3-2, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit2 y fit3
lrt <- -2 * (logLik(fit2) - logLik(fit3))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=6-3, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit3 y fit4
lrt <- -2 * (logLik(fit3) - logLik(fit4))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-6, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit4, test="Chisq", dispersion=1)

# Interpretacion: las variables weight y color son las que
# deberian estar en el modelo final

# Otro ejemplo ------------------------------------------------------------

# Usando los datos de cangrejos
# color: 1, medium light; 2, medium; 3, medium dark; 4, dark 
# spine condition: 1, both good; 2, one worn or broken; 3, both worn or broken
# Width, carapace width (cm); 
# Weight, weight (kg).

url <- "http://users.stat.ufl.edu/~aa/glm/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)
Crabs$color <- as.factor(Crabs$color)
Crabs$spine <- as.factor(Crabs$spine)

# Se desea modelar la variable Y que representa el numero de satelites
# Queremos comparar los siguientes modelos:
# fit1: con todas covariables - weight
# fit2: con todas covariables - width
# fit3: con todas covariables - color
# fit4: con todas covariables - spine
# fit5: con todas las covariables

# H0: el modelo "menor" esta bien
# H1: el modelo "mayor" es mejor

fit1 <- glm(y ~          width + color + spine, family=poisson, data=Crabs)
fit2 <- glm(y ~ weight +         color + spine, family=poisson, data=Crabs)
fit3 <- glm(y ~ weight + width +         spine, family=poisson, data=Crabs)
fit4 <- glm(y ~ weight + width + color        , family=poisson, data=Crabs)
fit5 <- glm(y ~ weight + width + color + spine, family=poisson, data=Crabs)

# Prueba razon de verosimilitud manual comparando fit1 y fit5
lrt <- -2 * (logLik(fit1) - logLik(fit5))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-7, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit2 y fit5
lrt <- -2 * (logLik(fit2) - logLik(fit5))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-7, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit3 y fit5
lrt <- -2 * (logLik(fit3) - logLik(fit5))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-5, lower.tail=FALSE)

# Prueba razon de verosimilitud manual comparando fit4 y fit5
lrt <- -2 * (logLik(fit4) - logLik(fit5))
lrt <- as.numeric(lrt)
lrt
pchisq(q=lrt, df=8-6, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
car::Anova(fit5, type="II", test.statistic="LR")

# NO USAR la siguiente instruccion en este ejemplo
anova(fit5, test="LRT") # NOOOOOOOOOOOOOO


# Diferencia entre anova y Anova ------------------------------------------

# En la url de abajo
# https://stats.stackexchange.com/questions/144837/difference-between-anova-and-anova-function
# Douglas Bates responde a la pregunta.




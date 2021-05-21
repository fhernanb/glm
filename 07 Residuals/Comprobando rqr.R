# En este script vamos a simular varios glm utilizando la funcion
# simglm del paquete rsq.
# Se van a ajustar tres modelos:
# Modelo bueno 1: con x1
# Modelo malo  2: con x1 y otras x's
# Modelo malo  3: solo con otras x's


# Poisson -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="poisson", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=poisson, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=poisson, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=poisson, data=sdata$yx)

# Analisis de residuales
library(car) # Para construir un qqPlot especial

par(mfrow=c(1, 3))

qr <- statmod::qresid(m1)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m1")

qr <- statmod::qresid(m2)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m2")

qr <- statmod::qresid(m3)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m3")

# Gaussian -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="gaussian", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=gaussian, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=gaussian, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=gaussian, data=sdata$yx)

# Analisis de residuales
library(car) # Para construir un qqPlot especial

par(mfrow=c(1, 3))

qr <- statmod::qresid(m1)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m1")

qr <- statmod::qresid(m2)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m2")

qr <- statmod::qresid(m3)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m3")

# Binomial -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="binomial", lambda=1, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=binomial, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=binomial, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=binomial, data=sdata$yx)

# Analisis de residuales
library(car) # Para construir un qqPlot especial

par(mfrow=c(1, 3))

qr <- statmod::qresid(m1)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m1")

qr <- statmod::qresid(m2)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m2")

qr <- statmod::qresid(m3)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m3")

# Gamma -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="Gamma", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=Gamma, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=Gamma, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=Gamma, data=sdata$yx)

# Analisis de residuales
library(car) # Para construir un qqPlot especial

par(mfrow=c(1, 3))

qr <- statmod::qresid(m1)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m1")

qr <- statmod::qresid(m2)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m2")

qr <- statmod::qresid(m3)    # quantile residual
qqPlot(x=qr, dist="norm", mean=0, sd=1, main="m3")

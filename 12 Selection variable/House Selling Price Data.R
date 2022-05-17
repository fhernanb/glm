# -----------------------------------------------------------------------------
# En este ejemplo vamos a utilizar procedimientos para seleccionar
# variables en un modelo glm.
#
# Los datos corresponden al ejemplo 4.7.3 de Agresti (2015)
#
# Nuestro modelo saturado sera: 
# precio ~ Gamma(mu_i, phi)
# 1 / mu_i ~ beds + baths + size + taxes + new + var_cuanti^2 + 
#            algunas inter dobles
# -----------------------------------------------------------------------------

# Lo primero es cargar los datos
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Vamos a explorar la relacion entre el precio con las otras variables explicativas
library(ggplot2)
library(patchwork)

p1 <- ggplot(datos) + geom_point(aes(size, price), alpha=0.2)
p2 <- ggplot(datos) + geom_point(aes(taxes, price), alpha=0.2)
p3 <- ggplot(datos, aes(x=as.factor(baths), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Baths")
p4 <- ggplot(datos, aes(x=as.factor(beds), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Beds")

(p1 + p2) / (p3 + p4)

# Vamos a ajustar un naive model
naive_mod <- glm(price ~ 1, data=datos, family=Gamma(link="inverse"))
summary(naive_mod)

# Comparemos el beta_0 estimado con mean(y)
mean(datos$price)
1 / mean(datos$price)

# Backward del paquete MASS
full_form <- formula(price ~ beds + baths + size + taxes + new +
                       I(beds^2) + I(baths^2) + I(size^2) + I(taxes^2) +
                       beds * new + baths * new + size * new)

full_mod <- glm(full_form, data=datos, family=Gamma(link="log"))

# Pregunta?????
# Es lo mismo family="Gamma" y family=Gamma(link="inverse") ?

# Aplicando backward
library(MASS)  # Para poder usar la funcion stepAIC
back_mod <- stepAIC(full_mod, trace=TRUE, k=2, direction="backward")
back_mod$anova
summary(back_mod)


# Envelope
library(glmtoolbox)
envelope(back_mod)

# Aplicando forward
forw_mod <- stepAIC(naive_mod, trace=TRUE, k=2, direction="forward",
                    scope=full_form)
forw_mod$anova
summary(forw_mod)

# Aplicando both
both_mod <- stepAIC(naive_mod, trace=TRUE, k=2, direction="both",
                    scope=full_form)
both_mod$anova
summary(both_mod)

# Resultados:
# De los tres modelos, back tiene el menor AIC
# Al comparar forward y both los modelos resultantes son iguales

# Comparemos dos de los modelos
anova(forw_mod, back_mod)

# Envelopes
library(glmtoolbox)
envelope(back_mod)
envelope(forw_mod)

# Vamos a calcular back_media y forw_media
# Lo que se muestra a continuacion se pudo haber obtenido 
# la funcion fitted(back_mod) pero se uso predict()
back_media <- predict(back_mod, newdata=datos, type="response")
forw_media <- predict(forw_mod, newdata=datos, type="response")

par(mfrow=c(1, 2))
plot(x=datos$price, y=back_media, las=1)
title("Backward")
abline(a=0, b=1, col="tomato") # linea recta y = a + b * x

plot(x=datos$price, y=back_media, las=1)
title("Forward")
abline(a=0, b=1, col="blue2") # linea recta y = a + b * x

# Correlacion entre las medias estimadas y precio
cor(back_media, datos$price)
cor(forw_media, datos$price)

# ECM
mean((back_media - datos$price)^2)
mean((forw_media - datos$price)^2)

# La corrrelacion puede ser enganosa. Vamos a crear un crazy_mod
crazy_media <- datos$price / 4
cor(crazy_media, datos$price)

plot(x=datos$price, y=crazy_media, las=1)
title("Crazy")
abline(a=0, b=1, col="purple")



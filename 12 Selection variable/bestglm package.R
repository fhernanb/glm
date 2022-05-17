# -------------------------------------------------------------------------
# En este ejemplo vamos a utilizar procedimientos para seleccionar
# variables en un modelo.
#
# Los datos corresponden al ejemplo 4.7 de Agresti (2015)
#
# Nuestro modelo saturado sera: 
# precio ~ Gamma(mu_i, phi)
# 1 / mu_i ~ beds + baths + size + taxes + new + var_cuanti^2 + 
# -------------------------------------------------------------------------

# Lo primero es cargar los datos
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# La libreria a usar 
# https://cran.r-project.org/web/packages/bestglm/
library(bestglm)

# Creando la formula con el modelo saturado
full_form <- formula(price ~ -1 + beds + baths + size + taxes + 
                       new + I(beds^2) + I(baths^2) + 
                       I(size^2) + I(taxes^2))

# Creando la matriz X
X <- model.matrix(full_form, data=datos)
head(X)

# vector y
y <- datos$price

# El data frame Xy
Xy <- cbind(X, y)
Xy <- as.data.frame(Xy)
head(Xy) # para explorar el data frame

# Esta es la forma de usar bestglm
best_mods <- bestglm(Xy, 
                     IC="AIC", 
                     method="exhaustive",
                     family=Gamma, 
                     TopModels=2)

# Usando las funciones S3 print y summary
best_mods
summary(best_mods)

# Para explorar otros modelos
best_mods$Subsets

# Extrayendo el mejor modelo
mod <- best_mods$BestModel
summary(mod)
AIC(mod)

# Vamos a calcular media
# Lo que se muestra a continuacion se pudo haber obtenido 
media <- predict(mod, newdata=Xy, type="response")

plot(x=datos$price, y=media, las=1)
abline(a=0, b=1, col="tomato") # linea recta y = a + b * x

# Correlacion entre media estimada y el valor real de y
cor(media, datos$price)


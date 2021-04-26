# 6.2.2 Example: Political Ideology
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

# Vamos a crear los datos desagrupados, pero lo vamos a hacer manualmente

ideolo <- c("Muy lib", "Algo lib", "Mod", "Algo conser", "Muy conser")
ideologia <- rep(rep(ideolo, times=4), 
                 times=c(25, 105, 86, 28, 4, 0, 5, 15, 83, 32, 
                         20, 73, 43, 20, 3, 0, 1, 14, 72, 32))
ideologia <- factor(ideologia, 
                    ordered = TRUE,
                    levels=ideolo)

genero <- rep(c("Female", "Male"), times=c(383, 278))

partido <- rep(c("Democ", "Repub", "Democ", "Repub"), times=c(248, 135, 159, 119)) 

datos <- data.frame(genero, partido, ideologia)

head(datos)

str(datos)

# Ajustando el modelo con vglm
library(VGAM)

fit1 <- vglm(ideologia ~ partido + genero,
             family=cumulative(link=logitlink, parallel=TRUE), data=datos)

summary(fit1)

class(fit1)

# Ajustando el modelo con polr
library(MASS)

fit2 <- polr(ideologia ~ partido + genero, data=datos) 

summary(fit2)

class(fit2)

# Vamos a hacer unas predicciones con la funcion predict

help("predictvglm")

# 1) Cual podria ser la ideologia de una mujer republicana?
nuevos_datos <- data.frame(genero="Female", partido="Repub")

# Usando fit1
predict(object=fit1, newdata=nuevos_datos, type="response")
# Usando fit2
predict(object=fit2, newdata=nuevos_datos, type="probs")

# 2) Cual podria ser la ideologia de un hombre democrata?
nuevos_datos <- data.frame(genero="Male", partido="Democ")

# Usando fit1
predict(object=fit1, newdata=nuevos_datos, type="response")
# Usando fit2
predict(object=fit2, newdata=nuevos_datos, type="probs")

# Vamos a ver que tal fueron las predicciones
probs <- predict(object=fit2, new_data=datos, type="probs")
head(probs)
# Vamos a obtener la Posicion de las Maximas Probabilidades
pos_max_probs <- apply(X=probs, MARGIN=1, FUN=which.max)
y_hat <- levels(ideologia)[pos_max_probs]
y_hat <- factor(y_hat, ordered = TRUE, levels=ideolo)

head(y_hat)

# Matriz de confusion
tabla <- table(clasificacion=y_hat, real=datos$ideologia)
tabla

sum(diag(tabla)) / sum(tabla)

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=datos$ideologia)


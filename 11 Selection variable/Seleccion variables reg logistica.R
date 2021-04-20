# Ejemplo de seleccion de variables para un modelo de
# regresion logistica

# Objetivo: elegir las variables que mejor explican P(Y=heart attack)
# usando los datos medidas cuerpo

# Una figura ilustrativa aqui
# https://fhernanb.github.io/libro_mod_pred/arb-de-clasif.html#%C3%A1rbol-de-clasificaci%C3%B3n

# Leyendo los datos -------------------------------------------------------

library(readr)
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/cleveland.csv'
datos <- read_csv(url, col_names = FALSE)

colnames(datos) <- c('age', 'sex', 'cp', 'trestbps', 'chol',
                     'fbs', 'restecg', 'thalach', 'exang', 
                     'oldpeak', 'slope', 'ca', 'thal', 'target')

# Exploremos los datos
str(datos)

# Vamos a convertir a factores las variables CUALI que estan como numeros
library(dplyr)
datos <- datos %>% mutate(sex = as.factor(sex),
                          cp = as.factor(cp),
                          fbs = as.factor(fbs),
                          restecg = as.factor(restecg),
                          exang=as.factor(exang),
                          slope=as.factor(slope),
                          thal=as.factor(thal))

datos

# Haciendo algunas modificaciones -----------------------------------------

# creando la variable Y a partir de la variable target.
# Y=1=presence y Y=0=absence
library(dplyr)
datos <- datos %>% mutate(y=case_when(target == 0  ~ "absence",
                                      target != 0  ~ "presence"))

# Como y son palabras debemos volverlo factor
datos <- datos %>% mutate(y = as.factor(y))

# Para ver una parte de los datos
datos %>% head()

# Seleccion FORWARD -------------------------------------------------------
empty_model <- glm(y ~ 1, data=datos, family=binomial(link='logit'))

# Vamos a crear una formula para el modelo full
horizonte <- formula(y ~ age + sex + cp + trestbps + chol + fbs + 
                       restecg + thalach + exang + oldpeak + slope)

library(MASS)
mod_forw <- stepAIC(empty_model, trace=TRUE, direction="forward", scope=horizonte)
mod_forw$anova
summary(mod_forw)

# Seleccion BACKWARD ------------------------------------------------------
full_model <- glm(horizonte, data=datos, family=binomial(link='logit'))

mod_back <- stepAIC(full_model, trace=TRUE, direction="backward")
mod_back$anova
summary(mod_back)

# Seleccion BOTH
mod_both <- stepAIC(full_model, trace=TRUE, direction="both")
mod_both$anova
summary(mod_both)

# Nota: los modelos finales son el mismo

# Matriz de confusion y medidas de desempeno ------------------------------
# Ver: https://en.wikipedia.org/wiki/Confusion_matrix

y_hat <- predict(mod_back, newdata=datos, type="response")
cut_off <- 0.5 # Lo decido yo
y_hat <- ifelse(y_hat >= cut_off, "presence", "absence")
y_hat <- as.factor(y_hat)

# Manual
conf_mat <- table(Prediccion=y_hat, Real=datos$y)
addmargins(conf_mat)

# Con el paquete caret
caret::confusionMatrix(data=y_hat, reference=datos$y)


# Calculando otras medidas ------------------------------------------------

# McFadden’s R2 ranges from 0 to just under 1. 
# Values close to 0 indicate that the model has no predictive power.
# values of 0.2 to 0.4 for McFadden’s R2 represent EXCELLENT fit

pscl::pR2(mod_back)["McFadden"]

# The importance of each predictor variable in the model by using the 
# varImp function from the caret package
caret::varImp(mod_back)

# Tarea: consultar https://www.statology.org/logistic-regression-in-r/


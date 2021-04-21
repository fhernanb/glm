# 6.1.2 Example: What Do Alligators Eat?
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

url <- 'http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat'
Gators <- read.table(url, header=TRUE)
Gators$y <- factor(Gators$y, levels=c('O', 'F', 'I')) 

# Exploring the data
table(Gators$y)

library(ggplot2)

ggplot(Gators) +
  geom_bar(aes(y), alpha=0.3, fill="lightblue", colour="black") +
  labs(title="Diagrama de barras para el tipo de comida",
       x="Tipo de comida", y="Frecuencia absoluta")

ggplot(Gators) +
  geom_boxplot(aes(x=y, y=x), alpha=0.3, fill="lightblue", colour="black") +
  labs(title="Boxplot para la Longitud del cocodrilo por Tipo de comida",
       x="Tipo de comida", y="Longitud (metros)")

# Fitting the model
library(VGAM)
mod <- vglm(y ~ x, family=multinomial(refLevel="O"), data=Gators) 
summary(mod)

# Exploring the object mod
class(mod)

# Las probabilidades
mod@fitted.values

# Predicting the class for alligator with x=3.89
predict(mod, newdata=data.frame(x=3.89), type='response')

predict(mod, newdata=data.frame(x=3.89), type="link")

predict(mod, newdata=data.frame(x=3.89), type="terms")

# para predecir la clase
w <- predict(mod, newdata=data.frame(x=3.89), type='response')
clase <- nnet::which.is.max(w)
levels(Gators$y)[clase]

# My confusion matrix
w <- predict(mod, type='response')
clase <- apply(w, 1, nnet::which.is.max)
y_hat <- levels(Gators$y)[clase]
y_hat <- factor(y_hat, levels=c('O', 'F', 'I'))
tabla <- table(clasificacion=y_hat, real=Gators$y)
tabla

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=Gators$y)


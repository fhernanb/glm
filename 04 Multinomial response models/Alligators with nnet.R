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
library(nnet)
mod <- multinom(y ~ x, data=Gators)
summary(mod)

# Exploring the object mod
class(mod)
names(mod)

# Las probabilidades
mod$fitted.values

# Predicting the class for alligator with x=3.89
predict(mod, newdata=data.frame(x=3.89), type="probs")

predict(mod, newdata=data.frame(x=3.89), type="class")

# My confusion table
y_hat <- predict(mod)
tabla <- table(clasificacion=y_hat, real=Gators$y)
tabla

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=Gators$y)

# Vamos a crear un clasificador adivinando
# El argumento n es el numero de "adivinaciones"
# pero usando la proporcion del tipo de comida

adivinar <- function(n) {
  rtas <- sample(x=c('O', 'F', 'I'), size=n,
                 replace=TRUE, prob=c(8, 31, 20)/59)
  return(factor(rtas, levels=c('O', 'F', 'I')))
}

adivinar(n=5)

confusionMatrix(data=adivinar(n=59), reference=Gators$y)

# Vamos a repetir el proceso de adivinacion para conocer
# como se distribuye el ACCURACY

nrep <- 500
accu <- NULL
for (i in 1:nrep) {
  res <- confusionMatrix(data=adivinar(n=59), reference=Gators$y)
  accu[i] <- res$overall[["Accuracy"]]
}

# Vamos a crear un dataframe con las adivinaciones
datos_adiv <- data.frame(accu)
head(datos_adiv)

library(ggplot2)

ggplot(data=datos_adiv) +
  geom_histogram(aes(accu), colour="tomato", fill="lemonchiffon") +
  geom_vline(xintercept=mean(accu), colour="blue", lwd=1.3, lty="dashed") +
  labs(title="Histograma con los Accuracy obtenidos adivinando")

# 6.1.2 Example: What Do Alligators Eat?
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

url <- 'http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat'
Gators <- read.table(url, header=TRUE)
Gators$y <- factor(Gators$y, levels=c('O', 'F', 'I')) 

# Exploring the data
table(Gators$y)
barplot(table(Gators$y))
with(Gators, boxplot(x ~ y, las=1, ylab='Length (mt)'))

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
predict(mod, newdata=data.frame(x=3.89))

# My confusion table
y_hat <- predict(mod)
tabla <- table(clasificacion=y_hat, real=Gators$y)
tabla

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=Gators$y)

# Vamos a crear un clasificador adivinando
adivinar <- function(n) {
  rtas <- sample(x=c('O', 'F', 'I'), size=n,
                 replace=TRUE, prob=c(8, 31, 20)/59)
  return(factor(rtas, levels=c('O', 'F', 'I')))
}

adivinar(n=5)

confusionMatrix(data=adivinar(n=59), reference=Gators$y)

m <- 200
accu <- NULL
for (i in 1:m) {
  res <- confusionMatrix(data=adivinar(n=59), reference=Gators$y)
  accu[i] <- res$overall[["Accuracy"]]
}

plot(density(accu))
abline(v=mean(accu), col='red', lty='dashed')

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
library(caret)
cntrl <- trainControl(method= "repeatedcv",
                      number=10, repeats=10,
                      classProbs=TRUE,
                      summaryFunction=multiClassSummary)

tuned <- train(y ~ x, data=Gators, 
               method="multinom", 
               trControl=cntrl,
               tuneLength=15)

tuned$bestTune
tuned$finalModel

# Las probabilidades
predict(tuned$finalModel, type="class")

# Predicting the class for alligator with x=3.89
predict(tuned$finalModel, 
        newdata=data.frame(x=3.89), type='class')

# My confusion table
y_hat <- predict(tuned$finalModel, type="class")
tabla <- table(clasificacion=y_hat, real=Gators$y)
tabla

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=Gators$y)


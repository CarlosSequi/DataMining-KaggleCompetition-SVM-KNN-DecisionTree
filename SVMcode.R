# Cargamos la librería con las funciones para SVM
library(e1071)
library(caret)
library(mRMRe)
library(NoiseFiltersR)
library(mice)
library(DMwR2)
library(unbalanced)


set.seed(42)

train = as.data.frame(read.csv("data/train.csv", na.strings = c("?")))
test = as.data.frame(read.csv("data/test.csv"), na.strings = c("?"))

train.preprocessed <- train

train.preprocessed <- train.preprocessed[!duplicated(train.preprocessed),]

for (colidx in 1:50){
  x <- train.preprocessed[,colidx]
  # Quitamos outliers
  x.out.low_err <- which(x < -50000)
  train.preprocessed[c(x.out.low_err), colidx] <- NA
}

train.clean <- na.omit(train.preprocessed)

sel_vars <- 1:50
train.x <- train.clean[,sel_vars]
train.y <- factor(train.clean$C)

# Ya tendríamos los datos listos para el clasificador
system.time(model <- svm(x=train.x, y=train.y, kernel = "radial", cost = 1, cross = 5))

summary(model)
# Calculamos la precision
train.x.pred <- predict(model, newdata = train.full.imputed.x)
cm <- confusionMatrix(train.x.pred, train.y)
cm

# Para el dataset completo
train.pred <- predict(model, newdata = train.full.imputed[,1:50])
cm <- confusionMatrix(train.pred, factor(train.full.imputed[,51]))
cm

test.pred <- as.numeric(predict(model, newdata = test))-1
test.preprocessed.pred <- as.numeric(predict(model, newdata = test.preprocessed))-1
output.test.pred <- data.frame("Id"=seq.int(length(test.pred)), "Prediction"=test.pred)
head(test.pred)

# Comparamos...
length(test.pred)
length(which(test.pred != test.preprocessed.pred))
# Si son iguales, no merece la pena subirlas

write.csv(output.test.pred, file = "data/predictionSubmission.csv", quote = FALSE, row.names = FALSE)



library(lubridate)
library(tidyverse)
library(caTools)
library(ROCR)
library(purrr)
library(reshape2)
#install.packages("dummies")
library(dummies)
library(caret)
library(FactoMineR)

banco <- read.csv("Banco_base.csv", sep = ";")
banco_resp <- banco
banco_resp$operation_status <- as.character(banco_resp$operation_status)
banco_resp$operation_status <- ifelse(banco_resp$operation_status=="","open", banco_resp$operation_status)
banco_resp <-dummy.data.frame(banco_resp, names=c("operation_status"), sep="_")


#padronizando a variavel genero
banco_resp$gender <- replace(banco_resp$gender,banco_resp$gender=="MALE","male")
#transformando em formato data para usar essa variavel como numerica
banco_resp$birth_date <- format(as.Date(banco_resp$birth_date,"%d/%m/%Y"), "%d/%m/%Y")
banco_resp$birth_date <- as.Date(banco_resp$birth_date,"%d/%m/%Y")
banco_resp$age <- 2018 - year(banco_resp$birth_date)
#repetindo o processedimento acima com a data de criacao
banco_resp$created_at <- format(as.Date(banco_resp$created_at,"%d/%m/%Y"), "%d/%m/%Y")
banco_resp$created_at <- as.Date(banco_resp$created_at,"%d/%m/%Y")
banco_resp$tempo_criado <- 2018 - year(banco_resp$created_at)
#retirando as duas variaveis data da base por ser ruim de trabalhar
banco_resp[ ,c('birth_date', 'created_at')] <- list(NULL)

summary(banco[banco$operation_status=="closed" &is.na(banco$declares_income_tax),])
summary(banco[banco$operation_status=="no_closed" &is.na(banco$declares_income_tax),])

banco_resp$declares_income_tax <- ifelse(is.na(banco_resp$declares_income_tax),2,banco_resp$declares_income_tax)
banco_resp$cpf_restriction <- ifelse(is.na(banco_resp$cpf_restriction),2,banco_resp$cpf_restriction)
banco_resp$model_year <- as.numeric(banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="2201",2001,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="20013",2013,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="20055",2005,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="20011",2011,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="1192",1992,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="19888",1988,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="20005",2005,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="1112",1992,banco_resp$model_year)

banco_resp$model_year <- ifelse(banco_resp$model_year<2002,1,
                                ifelse(banco_resp$model_year>=2002 & banco_resp$model_year<2008,2,
                                       ifelse(banco_resp$model_year>=2008 & banco_resp$model_year<2011,3,
                                              ifelse(banco_resp>=2011,4,0))))

banco_resp$model_year <- ifelse(is.na(banco_resp$model_year),0,banco_resp$model_year)

banco_resp$age <- ifelse(is.na(banco_resp$age),0,banco_resp$age)

banco_resp <-  subset(banco_resp, select = c("gender","monthly_income",
                                             "phone_code","cpf_restriction","registration_form_closed",
                                             "model_year","auto_value","auto_debt","loan_amount",
                                             "declares_income_tax","operation_status_closed","operation_status_no_closed","operation_status_ongoing",
                                              "operation_status_open","age","tempo_criado"))

sapply(banco_resp, class)

banco_resp$monthly_income <- as.numeric(banco_resp$monthly_income)
banco_resp$registration_form_closed <- as.factor(banco_resp$registration_form_closed)
banco_resp$model_year <- as.factor(banco_resp$model_year)
banco_resp$auto_value <- as.numeric(banco_resp$auto_value)
banco_resp$auto_debt <- as.numeric(banco_resp$auto_debt)
banco_resp$loan_amount <- as.numeric(banco_resp$loan_amount)
banco_resp$declares_income_tax <- as.factor(banco_resp$declares_income_tax)
banco_resp$operation_status_closed <- as.factor(banco_resp$operation_status_closed)
banco_resp$operation_status_open <- as.factor(banco_resp$operation_status_open)
banco_resp$operation_status_no_closed <- as.factor(banco_resp$operation_status_no_closed)
banco_resp$operation_status_ongoing <- as.factor(banco_resp$operation_status_ongoing)


banco_resp_open <- banco_resp[banco_resp$operation_status_open==1,]
banco_resp <- banco_resp[banco_resp$operation_status_open!=1,]
banco_resp_ongoing <- banco_resp[banco_resp$operation_status_ongoing==1,]
banco_resp <- banco_resp[banco_resp$operation_status_ongoing!=1,]


banco_resp$operation_status_no_closed <- NULL
banco_resp$operation_status_ongoing <- NULL
banco_resp$operation_status_open <- NULL

sapply(banco_resp, function(x){
  table(is.na(x)) 
})

banco_resp[is.na(banco_resp)] <- 0 

#MOVER A COLUNA operation_status_closed PARA ULTIMA POSICAO
col_idx <- grep("operation_status_closed", names(banco_resp))
banco_resp <- banco_resp[, c((1:ncol(banco_resp))[-col_idx],col_idx)]

#REDUZIR DIMENSIONALIDADE
numerico <- c("monthly_income","phone_code","cpf_restriction","auto_value","auto_debt","loan_amount","age","tempo_criado") 
fatores <- c("gender","registration_form_closed","model_year","declares_income_tax","operation_status_closed")
banco_PCA <-  banco_resp[,numerico]
pca <- PCA(banco_PCA)

Correlation_Matrix=as.data.frame(round(cor(Data_for_PCA,pca$ind$coord)^2*100,0))
Correlation_Matrix[with(Correlation_Matrix, order(-Correlation_Matrix[,1])),]


split = sample.split(banco_resp$operation_status_closed, SplitRatio = 0.7)

train = subset(banco_resp, split == TRUE)

test = subset(banco_resp, split == FALSE)

regre <- glm(operation_status_closed~., data = train, family= "binomial")
summary(regre)

predictTest = predict(regre, type="response", newdata=test)


threshold <- 0.20

tapply(predictTest, test$operation_status_closed, mean)
table(test$operation_status_closed, predictTest > threshold)

x1 <- table(test$operation_status, predictTest > threshold)[1]
x2 <- table(test$operation_status, predictTest > threshold)[2]
x3 <- table(test$operation_status, predictTest > threshold)[3]
x4 <- table(test$operation_status, predictTest > threshold)[4]

i <- x1 + x3
j <- x2 + x4


if(i<j){
  u <- j
}else{
  u <- i
}

#ACURACIA
acuracia <- (x1+x4)/(x1+x2+x3+x4)
#MODELO BASE
baseline <- (u)/(x1+x2+x3+x4)
#SENSIBILIDADE
sensibilidade <- (x4)/(x4+x2)
#especificidade
especificidade <- (x1)/(x1+x3)
#ERRO MEDIO
erro <- (x2+x3)/(x1+x2+x3+x4)
#FALSO NEGATIVO ERRO
FNerro <- (x2)/(x4+x2)
#FALSO POSITIVO ERRO
FPerro <- (x3)/(x3+x1)
#VERDADEIRO POSITIVO ERRO
TPerro <- (x4)/(x4+x2)

ROCRpredTest = prediction(predictTest, test$operation_status_closed)
ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf, colorize=TRUE)
points(FPerro,TPerro,col = "red", cex = 1.5)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


#########################3
#RANDOM FOREST#
install.packages('randomForest')
library(randomForest)

bacno_tree <- randomForest(operation_status_closed ~ gender + monthly_income + phone_code + cpf_restriction + model_year + auto_value + auto_debt + loan_amount + declares_income_tax + registration_form_closed + age + tempo_criado,
                           data = train, nodesize=20, ntree=500)
prever_forest <- predict(bacno_tree, newdata = test)

table(test$operation_status_closed, prever_forest)

x1 <- table(test$operation_status, prever_forest)[1]
x2 <- table(test$operation_status, prever_forest)[2]
x3 <- table(test$operation_status, prever_forest)[3]
x4 <- table(test$operation_status, prever_forest)[4]

i <- x1 + x3
j <- x2 + x4


if(i<j){
  u <- j
}else{
  u <- i
}

#ACURACIA
acuracia_tree <- (x1+x4)/(x1+x2+x3+x4)
#MODELO BASE
baseline_tree <- (u)/(x1+x2+x3+x4)
#SENSIBILIDADE
sensibilidade_tree <- (x4)/(x4+x2)
#especificidade
especificidade_tree <- (x1)/(x1+x3)
#ERRO MEDIO
erro_tree <- (x2+x3)/(x1+x2+x3+x4)
#FALSO NEGATIVO ERRO
FNerro_tree <- (x2)/(x4+x2)
#FALSO POSITIVO ERRO
FPerro_tree <- (x3)/(x3+x1)
#VERDADEIRO POSITIVO ERRO
TPerro_tree <- (x4)/(x4+x2)

ROCRpredTest = prediction(predictTest, test$operation_status_closed)
ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf, colorize=TRUE)
points(FPerro,TPerro,col = "red", cex = 1.5)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

#########################3
#CROSS VALIDATION#
library(caret)
install.packages('e1071')
library(e1071)

numfolds = trainControl(method = "cv", number = 10)
cp_ideal = expand.grid(.cp=seq(0.01,100,0.01))

train(operation_status_closed ~ gender + monthly_income + phone_code + cpf_restriction + model_year + auto_value + auto_debt + loan_amount + declares_income_tax + registration_form_closed + age + tempo_criado,
      data = train, method = "rpart", trControl = numfolds, tuneGrid =  cp_ideal)

banco_treecv <- rpart::rpart(operation_status_closed ~ gender + monthly_income + phone_code + cpf_restriction + model_year + auto_value + auto_debt + loan_amount + declares_income_tax + registration_form_closed + age + tempo_criado,
                             data = train, method = "class", cp = 100)
preve_cv <- predict(banco_treecv, newdata = test, type = "class")
table(test$operation_status_closed, preve_cv)

#O MODELO DEU OVERFITTING

#https://stats.stackexchange.com/questions/81576/how-to-judge-if-a-supervised-machine-learning-model-is-overfitting-or-not

install.packages('bestglm')
library(leaps)
library(bestglm)
args(bestglm)
# No matter how my variables are named, it always just seems to grab the last column 
# to use as the outcome (y) variable. In the first case, that produces the error because 
# that column is not strictly 0/1 so it doesn't think you're doing a logistic regression.
bestglm(train, IC="BIC", family = binomial)

#MODELO ESCOLHIDO FOI O RANDOM FOREST
prever_forest_open <- predict(bacno_tree, newdata = banco_resp_open)
banco_resp_open$status_prev <- prever_forest_open
table(banco_resp_open$status_prev)



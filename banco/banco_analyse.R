library(lubridate)
library(tidyverse)
library(caTools)
library(ROCR)

banco <- read.csv("Banco_base.csv", sep = ";")
banco_resp <- subset(banco, banco$operation_status!="")
banco_resp$gender <- replace(banco_resp$gender,banco_resp$gender=="MALE","male")
banco_resp$birth_date <- format(as.Date(banco_resp$birth_date,"%d/%m/%Y"), "%d/%m/%Y")
banco_resp$birth_date <- as.Date(banco_resp$birth_date,"%d/%m/%Y")
banco_resp$age <- 2018 - year(banco_resp$birth_date)
banco_resp$created_at <- format(as.Date(banco_resp$created_at,"%d/%m/%Y"), "%d/%m/%Y")
banco_resp$created_at <- as.Date(banco_resp$created_at,"%d/%m/%Y")
banco_resp$tempo_criado <- 2018 - year(banco_resp$created_at)
banco_resp[ ,c('birth_date', 'created_at')] <- list(NULL)

summary(banco_resp[banco_resp$operation_status=="closed" &is.na(banco_resp$declares_income_tax),])
summary(banco_resp[banco_resp$operation_status=="no_closed" &is.na(banco_resp$declares_income_tax),])

banco_resp$declares_income_tax <- ifelse(is.na(banco_resp$declares_income_tax),2,banco_resp$declares_income_tax)
banco_resp$cpf_restriction <- ifelse(is.na(banco_resp$cpf_restriction),2,banco_resp$cpf_restriction)
banco_resp$model_year <- ifelse(banco_resp$model_year=="2201",2001,banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year=="20013",2013,banco_resp$model_year)
banco_resp$model_year <- as.numeric(banco_resp$model_year)
banco_resp$model_year <- ifelse(banco_resp$model_year<2002,1,
                                ifelse(banco_resp$model_year>=2002 & banco_resp$model_year<2008,2,
                                       ifelse(banco_resp$model_year>=2008 & banco_resp$model_year<2011,3,
                                              ifelse(banco_resp>=2011,4,0))))
banco_resp$age <- ifelse(is.na(banco_resp$age),0,banco_resp$age)

#IDENTIFICAR OS EMPRESTIMOS NEGADOS
banco_resp$operation_status <- ifelse(banco_resp$operation_status=="closed" |  banco_resp$operation_status=="ongoing",1,0)


banco_resp <-  subset(banco_resp, select = c("gender","monthly_income",
                                             "phone_code","cpf_restriction","registration_form_closed",
                                             "model_year","auto_value","auto_debt","loan_amount",
                                             "declares_income_tax","operation_status", "age","tempo_criado"))

sapply(banco_resp, class)

banco_resp$monthly_income <- as.numeric(banco_resp$monthly_income)
banco_resp$registration_form_closed <- as.factor(banco_resp$registration_form_closed)
banco_resp$model_year <- as.factor(banco_resp$model_year)
banco_resp$auto_value <- as.numeric(banco_resp$auto_value)
banco_resp$auto_debt <- as.numeric(banco_resp$auto_debt)
banco_resp$loan_amount <- as.numeric(banco_resp$loan_amount)
banco_resp$declares_income_tax <- as.factor(banco_resp$declares_income_tax)
banco_resp$operation_status <- as.factor(banco_resp$operation_status)

set.seed(144)

split = sample.split(banco_resp$operation_status, SplitRatio = 0.7)

train = subset(banco_resp, split == TRUE)

test = subset(banco_resp, split == FALSE)

regre <- glm(operation_status~., data = train, family= "binomial")
summary(regre)

predictTest = predict(regre, type="response", newdata=test)

threshold <- 0.15

tapply(predictTest, test$operation_status, mean)
table(test$operation_status, predictTest > threshold)

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

ROCRpredTest = prediction(predictTest, test$operation_status)
ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf, colorize=TRUE)
points(FPerro,TPerro,col = "red", cex = 1.5)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

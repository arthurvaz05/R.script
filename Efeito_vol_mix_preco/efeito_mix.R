library(tidyverse)
library(dplyr)
  
resultados <- data.frame("SKU" = c("A","B","C","A","B","C"),
                         "Preco" = round(runif(6, min=13.80, max=15.30),2),
                         "Qtd" = round(rnorm(6, mean=100, sd=20),2),
                         "Periodo" = c(1,1,1,2,2,2))

resultados$Faturamento <- resultados$Preco*resultados$Qtd %>%
  round(2)

resultados <- left_join(resultados,
                        aggregate(resultados[,"Qtd"], 
                                  by = list(Periodo = resultados$Periodo), 
                                  FUN = sum), by = 'Periodo') 
resultados$delta <- round(resultados$Qtd/resultados$x,5)
resultados$x <- NULL


faturamento_p1 <- aggregate(resultados[,"Faturamento"], 
                                  by = list(Periodo = resultados$Periodo), 
                                  FUN = sum)[1,2]
faturamento_p2 <- aggregate(resultados[,"Faturamento"], 
                                        by = list(Periodo = resultados$Periodo), 
                                        FUN = sum)[2,2]

variacao_faturamento <- round(aggregate(resultados[,"Faturamento"], 
                by = list(Periodo = resultados$Periodo), 
                FUN = sum)[2,2]-aggregate(resultados[,"Faturamento"], 
                                          by = list(Periodo = resultados$Periodo), 
                                          FUN = sum)[1,2],2)


#Delta qtd
variacao_vol <- sum(resultados[resultados$Periodo==2,"Qtd"]-resultados[resultados$Periodo==1,"Qtd"])*
  (sum(resultados[resultados$Periodo==1,"Faturamento"])/sum(resultados[resultados$Periodo==1,"Qtd"]))

#Selling price
variacao_preco <- sum((resultados[resultados$Periodo==2,"Preco"]-resultados[resultados$Periodo==1,"Preco"])*
           resultados[resultados$Periodo==2,"Qtd"])

#Mix price
#Preco medio do primeiro periodo menos o preco de cada SKU no primeiro periodo
variacao_mix <- sum((resultados[resultados$Periodo==2,"Qtd"]-(resultados[resultados$Periodo==1,"delta"]*sum(resultados[resultados$Periodo==2,"Qtd"])))*
(resultados[resultados$Periodo==1,"Preco"]-(sum(resultados[resultados$Periodo==1,"Faturamento"])/sum(resultados[resultados$Periodo==1,"Qtd"]))))

variacao_faturamento
round(variacao_vol + variacao_preco + variacao_mix,2)

library(plotly)

x <- c('a_faturamento_p1','b_variacao_vol', 'c_variacao_preco', 'd_variacao_mix','e_faturamento_p2')
y <- c(faturamento_p1,variacao_vol, variacao_preco, variacao_mix,faturamento_p2)
data <- data.frame(x, y)

data$x <- as.factor(data$x)
data$id <- seq_along(data$y)
data$variacao <- ifelse(data$y>0,"positivo","negativo")
data[data$x %in% c("faturamento_p1","faturamento_p2"),"variacao"] <- "fat"
data$acum <- cumsum(data$y)
data$end <- c(head(data$acum, -1),0)
data$start <- c(0,head(data$end, -1))
data$variacao <- as.factor(data$variacao)

# #install.packages("devtools")
# devtools::install_github("cardiomoon/ggplotAssist")
# remove.packages(c("ggplot2", "data.table"))
# install.packages('Rcpp', dependencies = TRUE)
# install.packages('ggplot2', dependencies = TRUE)
# install.packages('data.table', dependencies = TRUE)
#https://rstudio-pubs-static.s3.amazonaws.com/329677_8f579b9e46284caeb9d3a72b7fdb7ac3.html


library(ggplot2)
ggplot(data, aes(x, fill=variacao)) + 
  geom_rect(aes(x = x, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) + 
  scale_fill_manual(values=c("#D55E00", "#009E73"))



#########PERCENTUAL
var_fat <- (faturamento_p2 - faturamento_p1)/faturamento_p1
#EFEITO PRECO
efeito_preco <- faturamento_p2/(sum(resultados[resultados$Periodo==2,"Qtd"]*resultados[resultados$Periodo==1,"Preco"]))-1
#EFEITO VOLUME
efeito_vol <-sum(resultados[resultados$Periodo==2,"Qtd"])/sum(resultados[resultados$Periodo==1,"Qtd"])-1
#EFEITO MIX
efeito_mix <-var_fat - efeito_preco - efeito_vol


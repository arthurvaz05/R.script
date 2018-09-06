#BIBLIOTECAS
{
  library(readr)
  library(tidyverse)  
  library(lubridate)
  library(plotly)
}

#CARREGAR TABELA
MBI <- read_delim("~/Downloads/Mix_Bev_Industry.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

#ADICIONAR A VARIAVEL MES
MBI$Month <- with(MBI,lubridate::month(Date))
#ADICIONAR A VARIAVEL PRECO
MBI$Volume <- as.numeric(MBI$Volume)
MBI$Value <- as.numeric(MBI$Value)
MBI$PRICE <- with(MBI, Value/Volume)
#CATEGORIZAR AVG PACK SIZE POR 1Q/MEDIANA/3Q
summary(MBI$`Avg. Pack.Size`)
boxplot(MBI$`Avg. Pack.Size`)
MBI$AVG_PS <- with(MBI, ifelse(`Avg. Pack.Size` <= 61, "MUITO PEQUENO",
                               ifelse(`Avg. Pack.Size`>61 & `Avg. Pack.Size`<=67,"PEQUENO",
                                      ifelse(`Avg. Pack.Size`>67 & `Avg. Pack.Size`<=80,"GRANDE","MUITO GRANDE"))))
#ONE HOT ENCONDING
MBI2 <-   cbind(with(MBI, model.matrix(~ AVG_PS + 0)), with(MBI, model.matrix(~ `Pack Material` + 0)),
              with(MBI, model.matrix(~ Flavor + 0)),
              with(MBI, model.matrix(~ `Caloric Content` + 0)),
              with(MBI, model.matrix(~ `Pack Material` + 0)),MBI)
MBI2 <- subset(MBI2, select = -c(`Avg. Pack.Size`,`Pack Material`,Flavor,`Caloric Content`,`Pack Material`,AVG_PS))
#ANALISE DO MERCADO
aggregate(MBI[,c("Date","Volume","Value","Price")])
MBI$

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
MBI$Volume <- round(as.numeric(gsub(",", ".", gsub("\\.", "", MBI$Volume))),2)
MBI$Value <- round(as.numeric(gsub(",", ".", gsub("\\.", "", MBI$Value))),2)
MBI$Price <- round(with(MBI, Value/Volume),2)
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
#VERIFICAR A CLASSE DAS VARIAVEIS
sapply(MBI[,c("Date","Volume","Value","Price")],class)
MBI$Date <- mdy(MBI$Date)
MBI$Date <- as.Date(MBI$Date,origin="1970-01-01")

mercado <- function(){
    MBI_mercado <- MBI[,c("Value","Volume","Date")]
    MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
    MBI_mercado <- as.data.frame(MBI_mercado)
    MBI_mercado%>%
    ggplot() +
    geom_line(aes(x=Date, y = Value, colour = "blue")) +
    geom_line(aes(x=Date, y = Volume, colour = "green")) + 
    scale_color_discrete(name = "Valores no tempo", labels = c("Valor", "Volume"))  
    MBI$Year <- year(MBI$Date)
    MBI$Month <- month(MBI$Date)
    aggregate(MBI[,c("Value","Volume")], by = list(MBI$Year), FUN = sum)%>%
      mutate(Price = round(Value / Volume,2))
    aggregate(MBI[,c("Value","Volume")], by = list(MBI$Month), FUN = sum)%>%
      mutate(Price = round(Value / Volume,2))
    }
mercado()

mercado2 <- function(){
  par(mfrow=c(1,2))
  cbind(MBI[,c("Date","Price")],scale(MBI[,c("Value","Volume")]))%>%
    ggplot() +
    geom_point(aes(x=Volume, y = Value, colour = "blue", size = Price)) + 
    ggtitle("Volume x Valor")
}

mercado2()

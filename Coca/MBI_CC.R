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
MBI$Date <- as.Date(MBI$Date)

mercado <- function(){
  par(2,1)
  cbind(MBI$Date,scale(MBI[,c("Value","Volume")]))%>%
  aggregate(by = list(Group.date = MBI$Date), FUN=sum, na.rm=TRUE)%>%
    ggplot() +
    geom_line(aes(x=Group.date, y = Value, colour = "blue")) +
    geom_line(aes(x=Group.date, y = Volume, colour = "green")) + 
    scale_color_discrete(name = "Valores no tempo", labels = c("Valor", "Volume"))  
    mutate(MBI, Price = Value/Volume)%>%  
    ggplot() + 
    geom_point(aes(x=Date, y = Price, colour = "red"))
    
    mutate(MBI[,c("Date","Value","Volume")], Price = Value/Volume)%>%
    aggregate(by = list(Group.date = MBI$Date), FUN=sum, na.rm=TRUE)%>%
    print()
}
mercado()


library(tidyverse)
library(readr)
library(plotly)
crimerj <- read_delim("BaseDPEvolucaoMensalCisp.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

delegacias <- read_delim("delegacias.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

popmes <- read_delim("PopulacaoEvolucaoMensalCisp.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)

popmes$mes_ano <- paste(popmes$vano,"m",popmes$mes, sep = "")

x <- aggregate(crimerj[,c('total_roubos','total_furtos','registro_ocorrencias')], by = list(mes_ano = crimerj$mes_ano), FUN = sum)
x1 <- aggregate(popmes[,c('pop_circ')], by = list(mes_ano = popmes$mes_ano), FUN = sum)

x <- dplyr::left_join(x, x1, by = "mes_ano")


x[,c("total_roubos","total_furtos","registro_ocorrencias","pop_circ")] <- scale(x[,c("total_roubos","total_furtos","registro_ocorrencias","pop_circ")])

plot_ly(x, x = ~mes_ano, y = ~total_roubos, type = 'scatter',mode = 'lines', name = 'Roubos')%>%
  add_trace(y = ~total_furtos, name = 'Furtos') %>%
  add_trace(y = ~registro_ocorrencias, name = 'Registro ocorrencias') %>%
  add_trace(y = ~pop_circ, name = 'pop_circ') %>%
  layout(yaxis = list(title = 'Quantidade'), barmode = 'stack')

cor(x[,c("total_roubos","total_furtos","registro_ocorrencias")])

y <- na.omit(x[,c("total_roubos","total_furtos","registro_ocorrencias","pop_circ")] )
cor(y)

#O PACOTE BETS NAO ESTA INSTALADO, PACOTE TIR FALTANDO
install.packages("devtools")
library(devtools)
devtools::install_github("pedrocostaferreira/BETS")

cor(crimerj$total_roubos,crimerj$total_furtos)


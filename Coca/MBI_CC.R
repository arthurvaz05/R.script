#BIBLIOTECAS
{
  #install.packages("rmarkdown")
  library(rmarkdown)
  library(readr)
  library(tidyverse)  
  library(lubridate)
  library(plotly)
  library(readxl)
  #install.packages("prophet")
  library(prophet)
  #install.packages('Metrics')
  library('Metrics')
  require(gridExtra)
  install.packages("devtools")
  library(devtools)
  #devtools::install_github('hadley/ggplot2', force = TRUE)
}


#CARREGAR TABELA EXCEL
MBI <- read_excel("~/Desktop/Mix_Bev_Industry.xlsx", 
                  col_types = c("date", "text", "text", 
                                "text", "text", "numeric", "text", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))

#CRIACAO DE VALORES, MUDANCA DE CLASSES SERAM FEITAS DENTRO DESSA FUNCAO
manipulacao <- function(){
  #ADICIONAR A VARIAVEL MES
  MBI$Month <- month(MBI$Date)
  #ADICIONAR A VARIAVEL ANO
  MBI$Year <- year(MBI$Date)
  #VERIFICAR A CLASSE DAS VARIAVEIS
  lapply(MBI[,c("Date","Volume","Value")],class)
  #ADICIONAR A VARIAVEL PRECO
  MBI$Volume <- round(as.numeric(gsub(",", ".", gsub("\\.", "", MBI$Volume))),2)
  MBI$Value <- round(as.numeric(gsub(",", ".", gsub("\\.", "", MBI$Value))),2)
  MBI$Price <- round(with(MBI, Value/Volume),2)
  #CATEGORIZAR AVG PACK SIZE POR 1Q/MEDIANA/3Q
  summary(MBI$`Avg. Pack.Size`)
  boxplot(MBI$`Avg. Pack.Size`)
  text(y = boxplot.stats(MBI$`Avg. Pack.Size`)$stats, labels = boxplot.stats(MBI$`Avg. Pack.Size`)$stats, x = 1.25)
  MBI$AVG_PS <- with(MBI, ifelse(`Avg. Pack.Size` <= 61 & `Avg. Pack.Size` >=48, "MUITO PEQUENO",
                                 ifelse(`Avg. Pack.Size`>61 & `Avg. Pack.Size`<=67,"PEQUENO",
                                        ifelse(`Avg. Pack.Size`>67 & `Avg. Pack.Size`<=80,"GRANDE",
                                               ifelse(`Avg. Pack.Size`> 80 & `Avg. Pack.Size`<=84,"MUITO GRANDE","OUTLIER")))))  
  return(MBI)
}
MBI <- manipulacao()
#CRIACAO DE UM NOVO DATASET PARA O CALCULO DE FORECAST
manipulacao2 <- function(){
  #ONE HOT ENCONDING
  MBI2 <-   cbind(with(MBI, model.matrix(~ AVG_PS + 0)), with(MBI, model.matrix(~ `Pack Material` + 0)),
                  with(MBI, model.matrix(~ Flavor + 0)),
                  with(MBI, model.matrix(~ `Caloric Content` + 0)),
                  with(MBI, model.matrix(~ `Pack Material` + 0)),MBI)
  MBI2 <- subset(MBI2, select = -c(`Avg. Pack.Size`,`Pack Material`,Flavor,`Caloric Content`,`Pack Material`,AVG_PS))  
  return(MBI2)
}
MBI2 <- manipulacao2()

#ANALISE DO MERCADO
{
  #CRIACAO DAS FUNCOES DOS GRAFICOS E TABELAS NECESSARIAS PARA ENTENDER O MERCADO
  {
    #GRAFICO NORMALIZADO DO VOLUME E VALOR NO TEMPO
    mercado_grafico1 <- function(){
      MBI_mercado1 <- MBI[,c("Value","Volume","Date")]
      #E PRECISO NORMALIZAR PARA QUE A GRANDEZA FICA NA MESMA ESCALA CASO AO CONTRARIO NAO DA PARA COMPARAR
      MBI_mercado1[,c("Value","Volume")] <- scale(MBI_mercado1[,c("Value","Volume")])
      MBI_mercado1 <- as.data.frame(MBI_mercado1)
      aggregate(MBI_mercado1[,c("Value","Volume")],by = list(MBI_mercado1$Date), FUN = sum)  %>%
        ggplot() +
        geom_smooth(aes(x=Group.1, y = Value, colour = "blue"),show.legend = FALSE) +
        geom_smooth(aes(x=Group.1, y = Volume, colour = "green"),show.legend = FALSE) + 
        scale_color_discrete(name = "Valores no tempo", labels = c("Valor", "Volume"))
    }
    #GRAFICOS PARA MOSTRAR QUE O PRECO SE MANTEM O MESMO AO LONGO DO TEMPO
    mercado_grafico2 <- function(){
      MBI_mercado <- MBI[,c("Value","Volume","Date")]
      MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
      MBI_mercado <- as.data.frame(MBI_mercado)
      MBI_mercado <- aggregate(MBI_mercado[,c("Value","Volume")],by = list(MBI_mercado$Date), FUN = sum)
      MBI_mercado$Price <- MBI_mercado$Value/MBI_mercado$Volume
      MBI_mercado %>%
        ggplot() +
        geom_point(aes(x=Group.1, y = Volume, size = Price)) + ylim(min(MBI_mercado$Volume),max(MBI_mercado$Volume))
    }
    mercado_grafico3 <- function(){
      MBI_mercado <- MBI[,c("Value","Volume","Date")]
      MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
      MBI_mercado <- as.data.frame(MBI_mercado)
      MBI_mercado <- aggregate(MBI_mercado[,c("Value","Volume")],by = list(MBI_mercado$Date), FUN = sum)
      MBI_mercado$Price <- MBI_mercado$Value/MBI_mercado$Volume
      MBI_mercado %>%
        ggplot() +
        geom_point(aes(x=Group.1, y = Value, size = Price)) + ylim(min(MBI_mercado$Volume),max(MBI_mercado$Volume))
    }
    #NORMALIZAR OS DADOS PARA VER A REAL INFLUENCIA E RELACAO ENTRE VOLUME, VALOR E PRECO
    mercado_grafico4 <- function(){
      MBI_mercado <- MBI[,c("Value","Volume","Date","Flavor")]
      MBI_mercado <- aggregate(MBI_mercado[,c("Value","Volume")],by = list(MBI_mercado$Date,MBI_mercado$Flavor), FUN = sum)
      MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
      MBI_mercado$Price <- MBI_mercado$Value/MBI_mercado$Volume
      MBI_mercado <- as.data.frame(MBI_mercado)
      MBI_mercado$Cluster <- ifelse(MBI_mercado$Group.2=="Milk Chocolate",1,2)
      
      t <- MBI_mercado %>%
        ggplot() +
        geom_point(aes(x=Volume, y = Value, size = Price, fill = Group.2), shape = 21) + 
        ylim(min(MBI_mercado$Volume),max(MBI_mercado$Volume))
      ggplotly(t)
    }
    mercado_grafico5 <- function(){
      MBI_mercado <- MBI[MBI$Flavor!="Milk Chocolate",c("Value","Volume","Date","Flavor")]
      MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
      MBI_mercado <- as.data.frame(MBI_mercado)
      MBI_mercado <- aggregate(MBI_mercado[,c("Value","Volume")],by = list(MBI_mercado$Date,MBI_mercado$Flavor), FUN = sum)
      MBI_mercado$Price <- MBI_mercado$Value/MBI_mercado$Volume
      t <- MBI_mercado %>%
        ggplot() +
        geom_point(aes(x=Volume, y = Value, size = Price, fill = Group.2), shape = 21) + 
        ylim(min(MBI_mercado$Volume),max(MBI_mercado$Volume)) +
        scale_size(range = c(1, 10))
      ggplotly(t)
    }
    #A MESMA INFORMACAO DE CIMA DOS SABORES EM 3 GRAFICOS, POREM SEM ESTAR NORMALIZADO
    mercado_grafico6 <- function(){
      MBI_tb <- MBI[,c("Value","Volume","Date","Flavor")]
      MBI_tb$Month <- month(MBI_tb$Date)
      MBI_tb <- aggregate(MBI_tb[,c("Value","Volume")], by = list(MBI_tb$Month, MBI_tb$Flavor), FUN = sum)
      MBI_tb$Price <- MBI_tb$Value/MBI_tb$Volume
      colnames(MBI_tb) <- c("Month","Flavor","Value","Volume","Price")
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
      t <- MBI_tb%>%
        ggplot(aes(y= Valores, x =Flavor, fill = Flavor)) + 
        geom_bar(stat = "identity") + 
        facet_wrap(Atributos ~ ., scales = "free") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_text(size=15))
      ggplotly(t)
    }
    #GRAFICO DO TIPO DE EMABALGEM MERCADO
    mercado_grafico7 <- function(){
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(Material = MBI$`Pack Material`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Month, y=Value,colour=Material)) + 
        geom_line(stat='identity') + 
        facet_grid(Year ~ .)  +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_blank(),
              legend.position="none")
      ggplotly(g)
    }
    mercado_grafico8 <- function(){
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(Material = MBI$`Pack Material`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Month, y=Volume,colour=Material)) + 
        geom_line(stat='identity') + 
        facet_grid(Year ~ .) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_text(size=10),
              legend.title = element_blank()) +
              guides(fill=FALSE)
      ggplotly(g)  %>%
        layout(legend = list(
          orientation = "v", x = 1.1, y =0.5
        )
        )
    }
    #GRAFICO DE TEOR CALORICO
    mercado_grafico9 <- function(){
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list('Caloric Content' = MBI$`Caloric Content`, Year = MBI$Year, Month = MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Month, y=Value,colour=`Caloric Content`)) + 
        geom_line(stat='identity') + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_text(size=10),
              legend.title = element_blank()) +
        facet_grid(Year ~ .) 
      ggplotly(g)%>%
        layout(legend = list(
          orientation = "v", x = 1.1, y =0.5
        )
        )
    }
    #GRAFICO POR TAMANHO DO PACOTE
    mercado_grafico10 <- function(){
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(MBI$AVG_PS, MBI$Year, MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Group.3, y=Value,colour=Group.1)) + 
        geom_line(stat='identity') + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_text(size=10),
              legend.title = element_blank()) +
        facet_grid(Group.2 ~ .)
      ggplotly(g)%>%
        layout(legend = list(
          orientation = "v", x = 1.1, y =0.5
        )
        )
    }
    #TABELA AGRUPADA DO VALOR, VOLUME E PRECO POR ANO E MES
    mercado_month <- function(){
      MBI$Month <- month(MBI$Date)
      aggregate(MBI[,c("Value","Volume")], by = list(Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
    }
    mercado_grafico11 <- function(){
      MBI$Month <- month(MBI$Date)
      MBI_tb <- aggregate(MBI[,c("Value","Volume")], by = list(Month = MBI$Year), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
      t <- MBI_tb%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        facet_wrap(Atributos~ ., scales = "free") + 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text=element_text(hjust=-100))
      ggplotly(t)
    }
    mercado_year <- function(){
      MBI$Year <- year(MBI$Date)
      aggregate(MBI[,c("Value","Volume")], by = list(Year =MBI$Year), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
    } 
  }
  #ATIVAR AS FUNCOES DOS GRAFICOS E TABLEAS
  {
    mercado_grafico1()
    grid.arrange(mercado_grafico2(), mercado_grafico3(), ncol=2)
    mercado_grafico4()
    mercado_grafico5()
    mercado_grafico6()
    subplot(mercado_grafico7(),mercado_grafico8())
    mercado_grafico9()
    mercado_grafico10()
    grid.arrange(tableGrob(mercado_year(), rows = NULL),
                 tableGrob(mercado_month(), rows = NULL), nrow = 1)
    subplot(mercado_grafico11(),mercado_grafico12())
  }
  
}


#MODELO PREDITIVO GLOBAL
{
  MBI2 <- MBI2[!is.na(MBI2$Value),]
  
  MBI_MERCADO <- MBI2[!is.na(MBI2$Shelf_Availability),]
  M <- MBI_MERCADO[,c("Date","Volume")]
  ## 75% DO TAMANHO DA AMOSTRA
  tamanho_amostra <- floor(0.75 * nrow(M))
  
  train <- M[1:tamanho_amostra, ]
  test <- M[tamanho_amostra:NROW(M), ]
  colnames(train) <- c('ds', 'y')
  train <- na.omit(train)
  M1 <- prophet::prophet(train)
  
  future <- make_future_dataframe(M1, periods = 365)
  forecast <- predict(M1, future)
  plot(M1, forecast)
  
  tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  tail(forecast)
  
  
  prophet_plot_components(M1, forecast)
  
  forecast %>%
    mutate(resid = trend - yhat)%>%
    ggplot(aes(x = ds, y = resid)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point(alpha = 0.5) +
    geom_smooth()
  
  forecast %>%
    gather(x, y, trend, yhat) %>%
    ggplot(aes(x = ds, y = y, color = x)) +
    geom_point(alpha = 0.5) +
    geom_line(alpha = 0.5)
}
#https://towardsdatascience.com/using-open-source-prophet-package-to-make-future-predictions-in-r-ece585b73687
#https://peerj.com/preprints/3190.pdf

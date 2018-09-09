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
  #install.packages("corrplot")
  library(corrplot)
  library("Hmisc")
  #install.packages("PerformanceAnalytics")
  library("PerformanceAnalytics")
  
}

#DATA MINING
{
  #CRIACAO DE VALORES, MUDANCA DE CLASSES SERAM FEITAS DENTRO DESSA FUNCAO
  manipulacao <- function(){
    #CARREGAR TABELA EXCEL
    MBI <- read_excel("~/Desktop/Mix_Bev_Industry.xlsx", 
                      col_types = c("date", "text", "text", 
                                    "text", "text", "numeric", "text", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))
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
    MBI$AVG_PS <- with(MBI, ifelse(`Avg. Pack.Size` <= 61 & `Avg. Pack.Size` >=48, "VERY SMALL",
                                   ifelse(`Avg. Pack.Size`>61 & `Avg. Pack.Size`<=67,"SMALL",
                                          ifelse(`Avg. Pack.Size`>67 & `Avg. Pack.Size`<=80,"BIG",
                                                 ifelse(`Avg. Pack.Size`> 80 & `Avg. Pack.Size`<=84,"VERY BIG","OUTLIER")))))
    return(MBI)
  }
  boxplot_grafico <- function(){
    MBI <- manipulacao()
    #CATEGORIZAR AVG PACK SIZE POR 1Q/MEDIANA/3Q
    summary(MBI$`Avg. Pack.Size`)
    boxplot(MBI$`Avg. Pack.Size`)
    text(y = boxplot.stats(MBI$`Avg. Pack.Size`)$stats, labels = boxplot.stats(MBI$`Avg. Pack.Size`)$stats, x = 1.25)
  }
  #PARETO E PARETO CHART
  pareto <- function(x,y){
    #PRIMEIRO ARGUMENTO E O CONJUNTO DE DADOS
    #SEGUNDO A VARIAVEL PARA FAZER O PARETO
    d <- arrange(x, desc(y)) %>%
      mutate(
        cumsum = cumsum(y),
        freq = round(y / sum(y), 3),
        cum_freq = cumsum(freq)
      )
    return(d)
  }
  pareto_grafico <- function(x,y,z){
    d <- pareto(x,y)
    ## Saving Parameters 
    def_par <- par() 
    
    # New margins
    par(mar=c(5,5,4,5)) 
    
    ## plot bars, pc will hold x values for bars
    pc = barplot(d$y,
                 width = 1, space = 0.2, border = NA, axes = F,
                 ylim = c(0, 1.05 * max(d$y, na.rm = T)), 
                 ylab = "Counts" , cex.names = 0.7, 
                 names.arg = d$z,
                 main = "Pareto Chart (version 2)")
    
    ## anotate left axis
    axis(side = 2, at = c(0, d$y), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
    
    ## frame plot
    box( col = "grey62")
    
    ## Cumulative Frequency Lines 
    px <- d$cum_freq * max(d$y, na.rm = T)
    lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
    
    ## Annotate Right Axis
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(d$cum_freq * 100)) ,"%",sep=""), 
         las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
    
    ## restoring default paramenter
    par(def_par) 
    
  }
  #CRIACAO DE UM NOVO DATASET PARA O CALCULO DE FORECAST
  manipulacao2 <- function(){
    MBI <- manipulacao()
    MBI$AVG_PS <- with(MBI, ifelse(`Avg. Pack.Size` <= 61 & `Avg. Pack.Size` >=48, "VERY SMALL",
                                   ifelse(`Avg. Pack.Size`>61 & `Avg. Pack.Size`<=67,"SMALL",
                                          ifelse(`Avg. Pack.Size`>67 & `Avg. Pack.Size`<=80,"BIG",
                                                 ifelse(`Avg. Pack.Size`> 80 & `Avg. Pack.Size`<=84,"VERY BIG","OUTLIER")))))
    #ONE HOT ENCONDING
    MBI2 <-   cbind(with(MBI, model.matrix(~ AVG_PS + 0)), with(MBI, model.matrix(~ `Pack Material` + 0)),
                    with(MBI, model.matrix(~ Flavor + 0)),
                    with(MBI, model.matrix(~ `Caloric Content` + 0)),
                    with(MBI, model.matrix(~ `Pack Material` + 0)),MBI)
    MBI2 <- subset(MBI2, select = -c(`Avg. Pack.Size`,`Pack Material`,Flavor,`Caloric Content`,`Pack Material`,AVG_PS))  
    return(MBI2)
  }
  #VERIFICAR A REPRESENTATIVIDADE DO NOVO CLUSTER E E O TAMANHO DO NOVO DATASET
  manipulacao3 <- function(){
    MBI <- manipulacao()
    MBI$AVG_PS <- with(MBI, ifelse(`Avg. Pack.Size` <= 61 & `Avg. Pack.Size` >=48, "VERY SMALL",
                                   ifelse(`Avg. Pack.Size`>61 & `Avg. Pack.Size`<=67,"SMALL",
                                          ifelse(`Avg. Pack.Size`>67 & `Avg. Pack.Size`<=80,"BIG",
                                                 ifelse(`Avg. Pack.Size`> 80 & `Avg. Pack.Size`<=84,"VERY BIG","OUTLIER")))))
    MBI2 <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") | MBI$`Caloric Content`=="Sugar" | MBI$AVG_PS==c("BIG","VERY SMALL"),]
    round(apply(MBI2[,c("Volume","Value")],2,sum)/apply(MBI[,c("Volume","Value")],2,sum),2)
    print(object.size(MBI), units = "auto", standard = "SI")
    print(object.size(MBI2), units = "auto", standard = "SI")} 
}

#ANALISE DO MERCADO
{
  #CRIACAO DAS FUNCOES DOS GRAFICOS E TABELAS NECESSARIAS PARA ENTENDER O MERCADO
  {
    #GRAFICO NORMALIZADO DO VOLUME E VALOR NO TEMPO
    mercado_grafico1 <- function(){
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
      MBI <- manipulacao()
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(Material = MBI$`Pack Material`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Month, y=Value,colour=Material)) + 
        geom_line(stat='identity') + 
        ggtitle("Value") +
        facet_grid(Year ~ .)  +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_blank())+
        guides(fill=FALSE)
      ggplotly(g)%>%
        layout(legend = list(
          orientation = "v", x = 1.1, y =0.5
        )
        )
    }
    mercado_grafico8 <- function(){
      MBI <- manipulacao()
      MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(Material = MBI$`Pack Material`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
      MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
      
      g <- ggplot(MBI_grafico, aes(x=Month, y=Volume,colour=Material)) + 
        geom_line(stat='identity') + 
        ggtitle("Volume") +
        facet_grid(Year ~ .) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text = element_text(size=10)) +
        guides(fill=FALSE)
      ggplotly(g)  %>%
        layout(legend = list(
          orientation = "v", x = 1.1, y =0.5
        )
        )
    }
    #GRAFICO DE TEOR CALORICO
    mercado_grafico9 <- function(){
      MBI <- manipulacao()
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
      MBI <- manipulacao()
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
    #GRAFICO E TABELA AGRUPADA DO VALOR, VOLUME, PRECO
    mercado_month <- function(){
      MBI <- manipulacao()
      aggregate(MBI[,c("Value","Volume")], by = list(Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
    }
    mercado_year <- function(){
      MBI <- manipulacao()
      aggregate(MBI[,c("Value","Volume")], by = list(Year =MBI$Year), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
    } 
    mercado_grafico11 <- function(){
      MBI <- manipulacao()
      MBI_tb <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
      t <- MBI_tb[MBI_tb$Atributos=="Volume",]%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        ggtitle("Volume") +
        facet_grid(Year ~ ., scales = "free")+ 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(labels = scales::comma,breaks = seq(1, 12, by = 1)) +
        scale_y_continuous(labels = scales::comma)
      t
    }
    mercado_grafico12 <- function(){
      MBI <- manipulacao()
      MBI_tb <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
      t <- MBI_tb[MBI_tb$Atributos=="Value",]%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        ggtitle("Value") +
        facet_grid(Year ~ ., scales = "free")+ 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(labels = scales::comma,breaks = seq(1, 12, by = 1)) +
        scale_y_continuous(labels = scales::comma)
      t
    }
    mercado_grafico13 <- function(){
      MBI <- manipulacao()
      MBI_tb <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
      t <- MBI_tb[MBI_tb$Atributos=="Price",]%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        ggtitle("Price") +
        facet_grid(Year ~ ., scales = "free")+ 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust = 0.5))  +
        scale_x_continuous(labels = scales::comma,breaks = seq(1, 12, by = 1)) +
        scale_y_continuous(labels = scales::comma)
      t
    }
    #GRAFICO COBERTURA E SHELFLIFE POR ANO E MES
    mercado_grafico14 <- function(){
      MBI <- manipulacao()
      MBI_tb <- aggregate(MBI[,"Coverage"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Coverage)
      t <- MBI_tb[MBI_tb$Atributos=="Coverage",]%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        ggtitle("Coverage") +
        facet_grid(Year ~ ., scales = "free")+ 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(labels = scales::comma,breaks = seq(1, 12, by = 1)) 
      t
    }
    mercado_grafico15 <- function(){
      MBI <- manipulacao()
      x <- density(MBI$Coverage, na.rm = TRUE)
      hist(MBI$Coverage, main = "Coverage Histogram")
      plot(x, main = "Coverage Kernel Distribution")
    }
    mercado_grafico16 <- function(){
      MBI <- manipulacao()
      MBI_tb <- aggregate(MBI[,"Shelf_Availability"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Shelf_Availability)
      t <- MBI_tb[MBI_tb$Atributos=="Shelf_Availability",]%>%
        ggplot(aes(y= Valores, x =Month)) + 
        geom_line(stat = "identity") +
        ggtitle("Shelf_Availability") +
        facet_grid(Year ~ ., scales = "free")+ 
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle = 90, hjust = 1),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(labels = scales::comma,breaks = seq(1, 12, by = 1)) 
      t
    }
    mercado_grafico17 <- function(){
      MBI <- manipulacao()
      x <- density(MBI$Shelf_Availability, na.rm = TRUE)
      par(mfrow=c(2,1))
      y <- hist(MBI$Coverage, main = "Shelf Availability Histogram")
      x1 <- plot(x, main = "Shelf Availability Kernel Distribution")
    }
    #GRAFICO DE CORRELACAO DO VALOR, VOLUME, PRECO, COBERTURA E SHELFLIFE
    mercado_grafico18 <- function(){
      MBI <- manipulacao()
      MBI_tb1 <- aggregate(MBI[,"Coverage"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb2 <- aggregate(MBI[,"Shelf_Availability"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb3 <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- dplyr::left_join(MBI_tb3,MBI_tb2, by = c('Month','Year'))
      MBI_tb <- dplyr::left_join(MBI_tb,MBI_tb1, by = c('Month','Year'))
      res <- round(cor(MBI_tb[,c("Coverage","Shelf_Availability","Value","Volume","Price")]),2)
      par(mfrow=c(1,1))
      corrplot(res, type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 45)
    }
    mercado_grafico19 <- function(){
      MBI <- manipulacao()
      MBI_tb1 <- aggregate(MBI[,"Coverage"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb2 <- aggregate(MBI[,"Shelf_Availability"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb3 <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- dplyr::left_join(MBI_tb3,MBI_tb2, by = c('Month','Year'))
      MBI_tb <- dplyr::left_join(MBI_tb,MBI_tb1, by = c('Month','Year'))
      res2 <- rcorr(as.matrix(MBI_tb[,c("Coverage","Shelf_Availability","Value","Volume","Price")]))
      par(mfrow=c(1,1))
      corrplot(res2$r, type="upper", order="hclust", 
               p.mat = res2$P, sig.level = 0.05, insig = "blank")  
    }
    mercado_grafico20 <- function(){
      MBI <- manipulacao()
      MBI_tb1 <- aggregate(MBI[,"Coverage"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb2 <- aggregate(MBI[,"Shelf_Availability"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
      MBI_tb3 <- aggregate(MBI[,c("Value","Volume")], by = list(Year = MBI$Year,Month = MBI$Month), FUN = sum)%>%
        mutate(Price = round(Value / Volume,2))
      MBI_tb <- dplyr::left_join(MBI_tb3,MBI_tb2, by = c('Month','Year'))
      MBI_tb <- dplyr::left_join(MBI_tb,MBI_tb1, by = c('Month','Year'))
      chart.Correlation(MBI_tb[,c("Coverage","Shelf_Availability","Value","Volume","Price")], histogram=TRUE, pch=19)
    }
  }
}

#ANALISE DE MARCA E PRODUTOR
{
  #MARCA
  {
    #MILK CHOCOLATE ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico21 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
  
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Brand, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico22 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Brand <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico23 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico24 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }
    #CHERRY ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico25 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Brand, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico26 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Brand <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico27 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico28 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }
    #COFFE ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico29 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Brand, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico30 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Brand <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico31 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico32 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Brand <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Brand)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }  
  }
  #PRODUTOR
  {
    #MILK CHOCOLATE ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico33 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Producer, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico34 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Producer <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico35 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico36 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }
    #CHERRY ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico37 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Producer, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico38 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Producer <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico39 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico40 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }
    #COFFE ANALYSE
    {
      #MARCAS EIXO VOLUME E VALOR NO TEMPO
      mercado_grafico41 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Volume, fill = Producer, size = Price)) + 
          geom_point() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE)
        ggplotly(t)
      }
      #PRICE INDEX
      mercado_grafico42 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        #MBI2 <- pareto(MBI,MBI$Volume)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        
        price_market <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        price_market$Price <- round(price_market$Value/price_market$Volume,2)
        price_market$Producer <- "Market"
        MBI1 <- rbind(MBI1,price_market)
        
        MBI1$Price_index <- round(as.numeric(MBI1$Price)/price_market[match(paste(MBI1$Month,MBI1$Year,"Market", sep = " - "),paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"],3)
        
        t <- MBI1%>%
          ggplot(aes(x = Month, y= Price_index, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) + 
          geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VOLUME
      mercado_grafico43 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_volume)
        
        MBI2$Share_volume <- round(as.numeric(MBI2$Volume)/total_mercado_volume[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_volume$Month,total_mercado_volume$Year,"Market", sep = " - ")),"Volume"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_volume, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
      #SHARE VALOR
      mercado_grafico44 <- function(){
        MBI <- manipulacao()
        #MBI <- MBI[MBI$Flavor==c("Milk Chocolate","Cherry","Coffe") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        #MBI1 <- pareto(MBI,MBI$Value)
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        
        total_mercado_valor <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_valor$Price <- round(total_mercado_valor$Value/total_mercado_valor$Volume,2)
        total_mercado_valor$Producer <- "Market"
        MBI2 <- rbind(MBI2,total_mercado_valor)
        
        MBI2$Share_value <- round(as.numeric(MBI2$Value)/total_mercado_valor[match(paste(MBI2$Month,MBI2$Year,"Market", sep = " - "),paste(total_mercado_valor$Month,total_mercado_valor$Year,"Market", sep = " - ")),"Value"],3)
        
        t <- MBI2%>%
          ggplot(aes(x = Month, y= Share_value, colour = Producer)) + 
          geom_line() + 
          facet_grid(Year ~ .) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text = element_text(size=10)) +
          guides(fill=FALSE) 
        #+ geom_line(linetype = "dashed",aes(x = Month, y= 1),color="black", size=1)
        ggplotly(t)
      }
    }
  }
}

#MODELO PREDITIVO MARCA - MARCA
{
  #MILK CHOCOLATE - LILI BRAND
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Lili",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Lili",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      test <- dplyr::left_join(test,forc, by = "Date")
      #test <- plyr::join(test,forc, by = "Date", type = "left")
      test <- na.omit(test)
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #CHERRY - HARLEY
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Harley",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Harley",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      test <- dplyr::left_join(test,forc, by = "Date")
      #test <- plyr::join(test,forc, by = "Date", type = "left")
      test <- na.omit(test)
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #COFFEE - ESHAN
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Eshan",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Harley",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      #test <- dplyr::left_join(test,forc, by = "Date")
      test <- plyr::join(test,forc, by = "Date", type = "left")
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #MODELO DE FORECAST 
  {
    forecast_grafico1 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      x <- plot(M1, forecast)
      ggplotly(x)  
    }
    forecast_grafico2 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
      #tail(forecast)
      
    }
    forecast_grafico3 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      prophet_plot_components(M1, forecast)
    }
    forecast_grafico4 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      y <- forecast %>%
        mutate(resid = trend - yhat)%>%
        ggplot(aes(x = ds, y = resid)) +
        geom_hline(yintercept = 0, color = "red") +
        geom_point(alpha = 0.5) +
        geom_smooth()
      ggplotly(y)
    }
    forecast_grafico5 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      z <- forecast %>%
        gather(x, y, trend, yhat) %>%
        ggplot(aes(x = ds, y = y, color = x)) +
        geom_point(alpha = 0.5) +
        geom_line(alpha = 0.5)
      ggplotly(z)
    }
  }
  #https://towardsdatascience.com/using-open-source-prophet-package-to-make-future-predictions-in-r-ece585b73687
  #https://peerj.com/preprints/3190.pdf
}

#MODELO PREDITIVO MARCA - PRODUTO
{
  #MILK CHOCOLATE - CHANDLER
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Chandler",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Chandler",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      test <- dplyr::left_join(test,forc, by = "Date")
      #test <- plyr::join(test,forc, by = "Date", type = "left")
      test <- na.omit(test)
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #CHERRY - ALI - CHANDLER - GOODMAN
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Ali",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Ali",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      test <- dplyr::left_join(test,forc, by = "Date")
      #test <- plyr::join(test,forc, by = "Date", type = "left")
      test <- na.omit(test)
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #COFFEE - CHANDLER
  {
    forecast1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Chandler",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      colnames(train) <- c('ds', 'y')
      train <- na.omit(train)
      M1 <- prophet::prophet(train)
      return(M1)
    }
    forecast_erro1 <- function(){
      MBI <- manipulacao()
      MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL") & MBI$Brand=="Chandler",]
      MBI2 <- pareto(MBI,MBI$Volume)
      MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
      MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Date = MBI2[MBI2$Brand=="OTHERS",]$Date, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Date = MBI2[MBI2$cum_freq <= 0.2,]$Date, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
        mutate(Price = Value/Volume)
      MBI2 <- rbind(MBI2,MBI2_1)
      MBI2$Price <- round(MBI2$Price,2)
      MBI_MERCADO <- MBI2
      
      M <- MBI_MERCADO[,c("Date","Volume")]
      ## 75% DO TAMANHO DA AMOSTRA
      tamanho_amostra <- floor(0.75 * nrow(M))
      
      train <- M[1:tamanho_amostra, ]
      test <- M[tamanho_amostra:NROW(M), ]
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)  
      forc <- forecast[,c("ds","yhat")]
      colnames(forc) <- c("Date","Volume")
      #test <- dplyr::left_join(test,forc, by = "Date")
      test <- plyr::join(test,forc, by = "Date", type = "left")
      d <-   list(
        "rmsle" = round(rmsle(test$Volume.x,test$Volume.y),2),
        "mape" = round(mape(test$Volume.x,test$Volume.y),2),
        "mae" = mae(test$Volume.x,test$Volume.y),
        "rmse" = rmse(test$Volume.x,test$Volume.y)
      )
      d <- as.data.frame(d)
      grid.table(d,rows=NULL)
    } 
  }
  #MODELO DE FORECAST 
  {
    forecast_grafico1 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      x <- plot(M1, forecast)
      ggplotly(x)  
    }
    forecast_grafico2 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
      #tail(forecast)
      
    }
    forecast_grafico3 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      prophet_plot_components(M1, forecast)
    }
    forecast_grafico4 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      y <- forecast %>%
        mutate(resid = trend - yhat)%>%
        ggplot(aes(x = ds, y = resid)) +
        geom_hline(yintercept = 0, color = "red") +
        geom_point(alpha = 0.5) +
        geom_smooth()
      ggplotly(y)
    }
    forecast_grafico5 <- function(){
      M1 <- forecast1()
      future <- make_future_dataframe(M1, periods = 365)
      forecast <- predict(M1, future)
      z <- forecast %>%
        gather(x, y, trend, yhat) %>%
        ggplot(aes(x = ds, y = y, color = x)) +
        geom_point(alpha = 0.5) +
        geom_line(alpha = 0.5)
      ggplotly(z)
    }
  }
  #https://towardsdatascience.com/using-open-source-prophet-package-to-make-future-predictions-in-r-ece585b73687
  #https://peerj.com/preprints/3190.pdf
}
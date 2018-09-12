#PACOTES DE INSTALACAO
{
  install.packages("rmarkdown") 
  install.packages("prophet")
  install.packages('Metrics')
  install.packages("devtools")
  devtools::install_github('hadley/ggplot2', force = TRUE)
  install.packages("corrplot")
  install.packages("PerformanceAnalytics")
}

#BIBLIOTECAS
{
  library(Rcpp)
  library(rmarkdown)
  library(readr)
  library(tidyverse)  
  library(lubridate)
  library(plotly)
  library(readxl)
  library(prophet)
  library('Metrics')
  require(gridExtra)
  library(devtools)
  library(corrplot)
  library("Hmisc")
  library("PerformanceAnalytics")
  library(shiny)
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
      MBI_mercado1 <- aggregate(MBI_mercado1[,c("Value","Volume")],by = list(Date = MBI_mercado1$Date), FUN = sum)
      
      x <- MBI_mercado1%>%
        ggplot() +
        geom_smooth(aes(x=Date, y = Value, colour = "blue"),show.legend = FALSE) +
        geom_smooth(aes(x=Date, y = Volume, colour = "green"),show.legend = FALSE) + 
        scale_color_discrete(name = "Valores no tempo", labels = c("Valor", "Volume"))
      
      x1 <- ggplotly(x)
      x2 <- div( x1, align="center" )
      return(x2)
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
        MBI_mercado <- aggregate(MBI_mercado[,c("Value","Volume")],by = list(Date = MBI_mercado$Date, Flavor = MBI_mercado$Flavor), FUN = sum)
        MBI_mercado[,c("Value","Volume")] <- scale(MBI_mercado[,c("Value","Volume")])
        MBI_mercado$Price <- MBI_mercado$Value/MBI_mercado$Volume
        MBI_mercado <- as.data.frame(MBI_mercado)
        MBI_mercado$Cluster <- ifelse(MBI_mercado$Flavor=="Milk Chocolate",1,2)
        
        h <- MBI_mercado %>%
          plot_ly(x= ~Volume, y = ~Value,type = 'scatter', mode = 'markers',color = ~Flavor,
                  marker = (list(size = ~Price*10, sizeref = 1.5)))%>%
          layout(title = 'Price Bubble Chart ',
                 yaxis = list(zeroline = FALSE),
                 xaxis = list(zeroline = FALSE))
        h <- div(h, align = "center")
        
        return(suppressWarnings(print(h)))
        
      }
    #A MESMA INFORMACAO DE CIMA DOS SABORES EM 3 GRAFICOS, POREM SEM ESTAR NORMALIZADO
      mercado_grafico6 <- function(){
        MBI <- manipulacao()
        MBI_tb <- MBI[,c("Value","Volume","Date","Flavor")]
        MBI_tb$Month <- month(MBI_tb$Date)
        MBI_tb <- aggregate(MBI_tb[,c("Value","Volume")], by = list(MBI_tb$Month, MBI_tb$Flavor), FUN = sum)
        MBI_tb$Price <- round(MBI_tb$Value/MBI_tb$Volume,2)
        colnames(MBI_tb) <- c("Month","Flavor","Value","Volume","Price")
        MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Value:Price)
        
        ax <- list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        
        h <- MBI_tb[MBI_tb$Atributos=="Price",] %>%
          plot_ly(type = "bar")%>%
          add_trace(x= ~Flavor, y = ~Valores,  
                    color = ~Flavor,width = 0.9 )%>%
          layout(title = '',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        h1 <- MBI_tb[MBI_tb$Atributos=="Value",] %>%
          plot_ly(type = "bar")%>%
          add_trace(x= ~Flavor, y = ~Valores,  
                    color = ~Flavor,width = 0.9 )%>%
          layout(title = '',
                 yaxis = list(zeroline = FALSE),
                 bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        h2 <- MBI_tb[MBI_tb$Atributos=="Volume",] %>%
          plot_ly(type = "bar")%>%
          add_trace(x= ~Flavor, y = ~Valores,  
                    color = ~Flavor,width = 0.9 )%>%
          layout(title = '',
                 yaxis = list(zeroline = FALSE),
                 bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        p  <- plotly::subplot(h,h1,h2,shareX=F,shareY=F,titleX=F,titleY=F) %>% layout(showlegend=T,annotations = list(
          list(x = 0.15 , y = 1.05, text = "Value", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.9 , y = 1.05, text = "Volume", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.5 , y = 1.05, text = "Price", showarrow = F, xref='paper', yref='paper')))
        
        
        p1 <- div(p, align = "center")
        
        return(suppressWarnings(print(p1)))
        
        
      }
    #GRAFICO DO TIPO DE EMABALGEM MERCADO
      mercado_grafico7 <- function(){
        MBI <- manipulacao()
        MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(Material = MBI$`Pack Material`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
        MBI_grafico$Price <- round(MBI_grafico$Value/MBI_grafico$Volume,2)
        
        
        h <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Value,  
                    color = ~Material,width = 0.9 )%>%
          layout(title = 'Pack Material Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        h1 <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Volume,  
                    color = ~Material,width = 0.9 )%>%
          layout(title = 'Pack Material Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        
        p  <- plotly::subplot(h,h1, nrows = 2,shareX=F,shareY=F,titleX=T,titleY=T) %>% layout(showlegend=T)
        
        
        p1 <- div(p, align = "center")
        
        return(suppressWarnings(print(p1)))
        
        
      }
    #GRAFICO DE TEOR CALORICO
      mercado_grafico9 <- function(){
        MBI <- manipulacao()
        MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list('Caloric Content' = MBI$`Caloric Content`,  Year = MBI$Year, Month = MBI$Month), FUN = sum)
        MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
        
        
        h <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Value,  
                    color = ~`Caloric Content`)%>%
          layout(title = 'Caloric Content Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""),
                 showlegend = TRUE)
        
        h1 <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Volume,  
                    color = ~`Caloric Content`)%>%
          layout(title = 'Caloric Content Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""),
                 showlegend = FALSE)
        
        
        p  <- plotly::subplot(h,h1, nrows = 2,shareX=F,shareY=F,titleX=T,titleY=T) %>% layout(showlegend=T)
        
        
        p1 <- div(p, align = "center")
        
        return(suppressWarnings(print(p1)))
        
        
      }
    #GRAFICO POR TAMANHO DO PACOTE
      mercado_grafico10 <- function(){
        MBI <- manipulacao()
        MBI_grafico <- aggregate(MBI[,c("Value","Volume")], by = list(AVG_PS = MBI$AVG_PS, Year = MBI$Year, Month = MBI$Month), FUN = sum)
        MBI_grafico$Price <- MBI_grafico$Value/MBI_grafico$Volume  
        
        h <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Value,  
                    color = ~AVG_PS)%>%
          layout(title = 'Average Package Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        h1 <- MBI_grafico%>%
          plot_ly(type = "scatter", mode = 'lines')%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Volume,  
                    color = ~AVG_PS)%>%
          layout(title = 'Average Package Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""))
        
        
        p  <- plotly::subplot(h,h1, nrows = 2,shareX=F,shareY=F,titleX=T,titleY=T) %>% layout(showlegend=T)
        
        
        p1 <- div(p, align = "center")
        
        return(suppressWarnings(print(p1)))
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
    #GRAFICO COBERTURA E SHELFLIFE POR ANO E MES
      mercado_grafico14 <- function(){
        MBI <- manipulacao()
        MBI_tb <- aggregate(MBI[,"Coverage"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
        MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Coverage)
        
        h <- MBI_tb%>%
          plot_ly(type = "scatter", mode = 'lines', name = "Coverage")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Valores)%>%
          layout(title = 'Coverage Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
        
      }
      mercado_grafico15 <- function(){
        MBI <- manipulacao()
        
        #probability that an event will fall into the corresponding bin 
        h <- MBI%>%
          plot_ly(type = "histogram", name = "Coverage", histnorm = "probability density")%>%
          add_trace(x= ~Coverage)%>%
          layout(title = 'Probability Density Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      mercado_grafico16 <- function(){
        MBI <- manipulacao()
        MBI_tb <- aggregate(MBI[,"Shelf_Availability"], by = list(Year = MBI$Year,Month = MBI$Month), FUN = mean, na.rm = TRUE)
        MBI_tb <- MBI_tb%>%gather(Atributos, Valores, Shelf_Availability)
        
        h <- MBI_tb%>%
          plot_ly(type = "scatter", mode = 'lines', name = "Shelf_Availability")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Valores)%>%
          layout(title = 'Shelf Availability Chart',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
        
      }
      mercado_grafico17 <- function(){
      MBI <- manipulacao()
      
      #probability that an event will fall into the corresponding bin 
      h <- MBI%>%
        plot_ly(type = "histogram", name = "Shelf_Availability")%>%
        add_trace(x= ~Shelf_Availability)%>%
        layout(title = 'Styled Line',
               yaxis = list(zeroline = FALSE),bargap = 5,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE, title = ""),
               showlegend = F)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
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
      #PRICE INDEX
        mercado_grafico21 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Brand)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      #SHARE VOLUME
        mercado_grafico22 <- function(){
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
          MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
          
          
          total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
          total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
          total_mercado_volume$Brand <- "Market"
          colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
          total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
          
          MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
          MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
          
          #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
          {
            pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
          }
          
          h <- MBI2%>%
            plot_ly(type = "scatter", mode = "lines")%>%
            add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Brand)%>%
            layout(title = 'Milk Chocolate Brand Market',
                   yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                   showlegend = T)
          
          h1 <- div(h, align = "center")
          
          return(suppressWarnings(print(h1)))
        }
      #SHARE VALOR
        mercado_grafico23 <- function(){
          MBI <- manipulacao()
          MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
          MBI2 <- pareto(MBI,MBI$Volume)
          MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
          MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
            mutate(Price = Value/Volume)
          MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
            mutate(Price = Value/Volume)
          MBI2 <- rbind(MBI2,MBI2_1)
          MBI2$Price <- round(MBI2$Price,2)
          MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
          
          
          total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
          total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
          total_mercado_volume$Brand <- "Market"
          colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
          total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
          
          MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
          MBI2$Share_value <- round(MBI2$Value/MBI2$Value_M,2)
          
          #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
          {
            pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
          }
          
          h <- MBI2%>%
            plot_ly(type = "scatter", mode = "lines")%>%
            add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Brand)%>%
            layout(yaxis = list(zeroline = FALSE, title = "Share Volume"),bargap = 5,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                   showlegend = T)
          
          h1 <- div(h, align = "center")
          
          return(suppressWarnings(print(h1)))
        }
    }
    #CHERRY ANALYSE
    {
      #PRICE INDEX
        mercado_grafico24 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Brand)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      #SHARE VOLUME
      mercado_grafico25 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
        MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
        
        #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Brand)%>%
          layout(title = "Cherry Brand Market",
                 yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      #SHARE VALOR
        mercado_grafico26 <- function(){
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
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
        MBI2$Share_value <- MBI2$Value/MBI2$Value_M
        
        #PROVA REAL DO SHARE VALOR FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Brand)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
    }
    #COFFE ANALYSE
    {
      #PRICE INDEX
        mercado_grafico27 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Brand <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Brand)
        MBI1_1 <-  aggregate(MBI1[MBI1$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Brand=="OTHERS",]$Month, Year = MBI1[MBI1$Brand=="OTHERS",]$Year, Brand = MBI1[MBI1$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Brand = MBI1[MBI1$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Brand)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      #SHARE VOLUME
        mercado_grafico28 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Brand <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Brand)
        MBI2_1 <-  aggregate(MBI2[MBI2$Brand=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Brand=="OTHERS",]$Month, Year = MBI2[MBI2$Brand=="OTHERS",]$Year, Brand = MBI2[MBI2$Brand=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Brand = MBI2[MBI2$cum_freq <= 0.2,]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
        MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
        
        #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Brand)%>%
          layout(title = 'Coffee Brand Market',
                 yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                 showlegend = T)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
      #SHARE VALOR
        mercado_grafico29 <- function(){
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
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
        MBI2$Share_value <- MBI2$Value/MBI2$Value_M
        
        #PROVA REAL DO SHARE VALOR FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Brand)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
    }
  }
  #PRODUTOR
  {
    #MILK CHOCOLATE ANALYSE
    {
      #PRICE INDEX
        mercado_grafico30 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
        
        
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
        mercado_grafico31 <- function(){
          MBI <- manipulacao()
          MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
          MBI2 <- pareto(MBI,MBI$Volume)
          MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
          MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
            mutate(Price = Value/Volume)
          MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
            mutate(Price = Value/Volume)
          MBI2 <- rbind(MBI2,MBI2_1)
          MBI2$Price <- round(MBI2$Price,2)
          MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
          
          
          total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
          total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
          total_mercado_volume$Brand <- "Market"
          colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
          total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
          
          MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
          MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
          
          #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
          {
            pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
          }
          
          h <- MBI2%>%
            plot_ly(type = "scatter", mode = "lines")%>%
            add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Producer)%>%
            layout(title = 'Milk Chocolate Producer Market',
                   yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                   showlegend = T)
          
          h1 <- div(h, align = "center")
          
        }
      #SHARE VALOR
        mercado_grafico32 <- function(){
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
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
        MBI2$Share_value <- MBI2$Value/MBI2$Value_M
        
        #PROVA REAL DO SHARE VALOR FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
    }
    #CHERRY ANALYSE
    {
      #PRICE INDEX
        mercado_grafico33 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
        
        
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
      mercado_grafico34 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
        MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
        
        #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Producer)%>%
          layout(title = 'Cherry Producer Market',
                 yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                 showlegend = T)
        
        h1 <- div(h, align = "center")
        
      }
      #SHARE VALOR
        mercado_grafico35 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
        MBI2$Share_value <- MBI2$Value/MBI2$Value_M
        
        #PROVA REAL DO SHARE VALOR FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
    }
    #COFFE ANALYSE
    {
      #PRICE INDEX
        mercado_grafico36 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI1 <- pareto(MBI,MBI$Value)
        MBI1$Producer <- ifelse(MBI1$cum_freq>0.2,"OTHERS",MBI1$Producer)
        MBI1_1 <-  aggregate(MBI1[MBI1$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI1[MBI1$Producer=="OTHERS",]$Month, Year = MBI1[MBI1$Producer=="OTHERS",]$Year, Producer = MBI1[MBI1$Producer=="OTHERS",]$Brand), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- aggregate(MBI1[MBI1$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI1[MBI1$cum_freq <= 0.2,]$Month, Year = MBI1[MBI1$cum_freq <= 0.2,]$Year, Producer = MBI1[MBI1$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI1 <- rbind(MBI1,MBI1_1)
        MBI1$Price <- round(MBI1$Price,2)
        MBI1$Concat <- paste(MBI1$Month,MBI1$Year,sep = " - ")
        
        
        total_mercado_preco <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_preco$Price <- round(total_mercado_preco$Value/total_mercado_preco$Volume,2)
        total_mercado_preco$Brand <- "Market"
        colnames(total_mercado_preco) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_preco$Concat <- paste(total_mercado_preco$Month,total_mercado_preco$Year, sep = " - ")
        
        MBI1 <- dplyr::left_join(MBI1,total_mercado_preco[,c("Concat","Price_M")], by = "Concat")
        MBI1$Price_index <- round(MBI1$Price/MBI1$Price_M,2)
        
        
        h <- MBI1%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Price_index,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
        
        
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
      mercado_grafico37 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Volume_M")], by = "Concat")
        MBI2$Share_volume <- MBI2$Volume/MBI2$Volume_M
        
        #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_volume, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Share_volume,  color = ~Producer)%>%
          layout(title = 'Coffee Producer Market',
                 yaxis = list(zeroline = FALSE, title = "Share Volume",range = c(0,1)),bargap = 5,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
                 showlegend = T)
        
        h1 <- div(h, align = "center")
        
      }
      #SHARE VALOR
        mercado_grafico38 <- function(){
        MBI <- manipulacao()
        MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$`Caloric Content`=="Sugar" & MBI$AVG_PS==c("BIG","VERY SMALL"),]
        MBI2 <- pareto(MBI,MBI$Volume)
        MBI2$Producer <- ifelse(MBI2$cum_freq>0.2,"OTHERS",MBI2$Producer)
        MBI2_1 <-  aggregate(MBI2[MBI2$Producer=="OTHERS",c("Volume","Value")], by = list(Month = MBI2[MBI2$Producer=="OTHERS",]$Month, Year = MBI2[MBI2$Producer=="OTHERS",]$Year, Producer = MBI2[MBI2$Producer=="OTHERS",]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- aggregate(MBI2[MBI2$cum_freq <= 0.2,c("Volume","Value")], by = list(Month = MBI2[MBI2$cum_freq <= 0.2,]$Month, Year = MBI2[MBI2$cum_freq <= 0.2,]$Year, Producer = MBI2[MBI2$cum_freq <= 0.2,]$Producer), FUN = sum)%>%
          mutate(Price = Value/Volume)
        MBI2 <- rbind(MBI2,MBI2_1)
        MBI2$Price <- round(MBI2$Price,2)
        MBI2$Concat <- paste(MBI2$Month,MBI2$Year,sep = " - ")
        
        
        total_mercado_volume <- aggregate(MBI[,c("Volume","Value")], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
        total_mercado_volume$Price <- round(total_mercado_volume$Value/total_mercado_volume$Volume,2)
        total_mercado_volume$Brand <- "Market"
        colnames(total_mercado_volume) <- c("Month","Year","Volume_M","Value_M","Price_M","Brand")
        total_mercado_volume$Concat <- paste(total_mercado_volume$Month,total_mercado_volume$Year, sep = " - ")
        
        MBI2 <- dplyr::left_join(MBI2,total_mercado_volume[,c("Concat","Value_M")], by = "Concat")
        MBI2$Share_value <- MBI2$Value/MBI2$Value_M
        
        #PROVA REAL DO SHARE VALOR FECHANDO 1 NO MES E ANO
        {
          pr <- aggregate(MBI2$Share_value, by = list(MBI2$Month,MBI2$Year), sum)
        }
        
        h <- MBI2%>%
          plot_ly(type = "scatter", mode = "lines")%>%
          add_trace(x= ~list(Year = MBI$Year,Month = MBI$Month), y = ~Share_value,  color = ~Producer)%>%
          layout(title = 'Styled Line',
                 yaxis = list(zeroline = FALSE),bargap = 5,
                 xaxis = list(zeroline = FALSE),
                 showlegend = F)
        
        h1 <- div(h, align = "center")
        
        return(suppressWarnings(print(h1)))
      }
    }
  }
}

#CONTRIBUTION DAS MARCAS
{
  #MILK CHOCOLATE ANALYSE
  {
    #SHARE VOLUME
    mercado_grafico39 <- function(){
      MBI <- manipulacao()
      #CONSIDERANDO TODAS OS ATRIBUTOS DO PRODUTO DENTRO DO UNIVERSO DO SABOR
      MBI <- MBI[MBI$Flavor==c("Milk Chocolate") & MBI$Producer=="Chandler",]
      MBI2 <- aggregate(MBI[,"Volume"], by = list(Producer = MBI$Producer, Brand = MBI$Brand, Month = MBI$Month, Year = MBI$Year), FUN = sum)
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year, sep = " - ")
      
      total_produtor_volume <- aggregate(MBI[,"Volume"], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
      colnames(total_produtor_volume) <- c("Month","Year","Volume_M")
      total_produtor_volume$Concat <- paste(total_produtor_volume$Month,total_produtor_volume$Year, sep = " - ")
      
      MBI2 <- dplyr::left_join(MBI2,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
      MBI2$Contribution_volume1 <- round(MBI2$Volume/MBI2$Volume_M,6)
      MBI2 <- MBI2[MBI2$Volume!=0,]
      MBI2$Contribution_Month <- ifelse(MBI2$Month==1,12,MBI2$Month-1)
      MBI2$Contribution_Year <-  ifelse(MBI2$Contribution_Month==12,MBI2$Year-1,MBI2$Year) 
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year,MBI2$Brand, sep = " - ")  
      MBI12 <- MBI2[,c("Contribution_volume1","Concat")]
      MBI2$Concat <- paste(MBI2$Contribution_Month,MBI2$Contribution_Year,MBI2$Brand, sep = " - ")  
      MBI2 <- dplyr::left_join(MBI2, MBI12, by = "Concat")
      MBI2$Contribution_Rate <- ifelse(is.na(MBI2$Contribution_volume1.y),0,MBI2$Contribution_volume1.x-MBI2$Contribution_volume1.y)
      #MBI2 <- na.omit(MBI12)
      
      #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
      {
        pr <- aggregate(MBI2[,c("Volume","Contribution_Rate")], by = list(Month = MBI2$Month,Year = MBI2$Year,Producer = MBI2$Producer), sum)
        pr$Concat <- paste(pr$Month,pr$Year, sep = " - ")
        pr <- dplyr::left_join(pr,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
        
        pr$Share_Volume <- round(pr$Volume/pr$Volume_M,4)
        pr$pr_Month <- ifelse(pr$Month==1,12,pr$Month-1)
        pr$pr_Year <-  ifelse(pr$pr_Month==12,pr$Year-1,pr$Year)
        pr$Concat <- paste(pr$Month,pr$Year,pr$Producer, sep = " - ")  
        pr2 <- pr[,c("Share_Volume","Concat")]
        pr$Concat <- paste(pr$pr_Month,pr$pr_Year,pr$Producer, sep = " - ")
        pr <- dplyr::left_join(pr, pr2, by = "Concat")
        pr$Delta_Share_Volume <- ifelse(is.na(pr$Share_Volume.y),0,pr$Share_Volume.x-pr$Share_Volume.y)
        pr$Contribution_Rate <- round(pr$Contribution_Rate,4)
        pr$validation <- pr$Contribution_Rate-pr$Delta_Share_Volume
      }
      
      h <- MBI2%>%
        plot_ly(type = "scatter", mode = "lines")%>%
        add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Contribution_Rate,  color = ~Brand)%>%
        layout(title = 'Styled Line',
               yaxis = list(zeroline = FALSE),bargap = 5,
               xaxis = list(zeroline = FALSE),
               showlegend = F)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
    }
  } 
  
  #CHERRY ANALYSE
  {
    #SHARE VOLUME
    mercado_grafico40 <- function(){
      MBI <- manipulacao()
      #CONSIDERANDO TODAS OS ATRIBUTOS DO PRODUTO DENTRO DO UNIVERSO DO SABOR
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$Producer=="Ali",]
      MBI2 <- aggregate(MBI[,"Volume"], by = list(Producer = MBI$Producer, Brand = MBI$Brand, Month = MBI$Month, Year = MBI$Year), FUN = sum)
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year, sep = " - ")
      
      total_produtor_volume <- aggregate(MBI[,"Volume"], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
      colnames(total_produtor_volume) <- c("Month","Year","Volume_M")
      total_produtor_volume$Concat <- paste(total_produtor_volume$Month,total_produtor_volume$Year, sep = " - ")
      
      MBI2 <- dplyr::left_join(MBI2,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
      MBI2$Contribution_volume1 <- round(MBI2$Volume/MBI2$Volume_M,6)
      MBI2 <- MBI2[MBI2$Volume!=0,]
      MBI2$Contribution_Month <- ifelse(MBI2$Month==1,12,MBI2$Month-1)
      MBI2$Contribution_Year <-  ifelse(MBI2$Contribution_Month==12,MBI2$Year-1,MBI2$Year) 
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year,MBI2$Brand, sep = " - ")  
      MBI12 <- MBI2[,c("Contribution_volume1","Concat")]
      MBI2$Concat <- paste(MBI2$Contribution_Month,MBI2$Contribution_Year,MBI2$Brand, sep = " - ")  
      MBI2 <- dplyr::left_join(MBI2, MBI12, by = "Concat")
      MBI2$Contribution_Rate <- ifelse(is.na(MBI2$Contribution_volume1.y),0,MBI2$Contribution_volume1.x-MBI2$Contribution_volume1.y)
      #MBI2 <- na.omit(MBI12)
      
      #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
      {
        pr <- aggregate(MBI2[,c("Volume","Contribution_Rate")], by = list(Month = MBI2$Month,Year = MBI2$Year,Producer = MBI2$Producer), sum)
        pr$Concat <- paste(pr$Month,pr$Year, sep = " - ")
        pr <- dplyr::left_join(pr,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
        
        pr$Share_Volume <- round(pr$Volume/pr$Volume_M,4)
        pr$pr_Month <- ifelse(pr$Month==1,12,pr$Month-1)
        pr$pr_Year <-  ifelse(pr$pr_Month==12,pr$Year-1,pr$Year)
        pr$Concat <- paste(pr$Month,pr$Year,pr$Producer, sep = " - ")  
        pr2 <- pr[,c("Share_Volume","Concat")]
        pr$Concat <- paste(pr$pr_Month,pr$pr_Year,pr$Producer, sep = " - ")
        pr <- dplyr::left_join(pr, pr2, by = "Concat")
        pr$Delta_Share_Volume <- ifelse(is.na(pr$Share_Volume.y),0,pr$Share_Volume.x-pr$Share_Volume.y)
        pr$Contribution_Rate <- round(pr$Contribution_Rate,4)
        pr$validation <- pr$Contribution_Rate-pr$Delta_Share_Volume
      }
      
      h <- MBI2%>%
        plot_ly(type = "scatter", mode = "lines")%>%
        add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Contribution_Rate,  color = ~Brand)%>%
        layout(title = 'Contribution Ali Rate',
               yaxis = list(zeroline = FALSE, title = "Cherry Contribution Share Volume"),bargap = 5,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
               showlegend = T)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
    }
    #SHARE VOLUME
    mercado_grafico41 <- function(){
      MBI <- manipulacao()
      #CONSIDERANDO TODAS OS ATRIBUTOS DO PRODUTO DENTRO DO UNIVERSO DO SABOR
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$Producer=="Goodman",]
      MBI2 <- aggregate(MBI[,"Volume"], by = list(Producer = MBI$Producer, Brand = MBI$Brand, Month = MBI$Month, Year = MBI$Year), FUN = sum)
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year, sep = " - ")
      
      total_produtor_volume <- aggregate(MBI[,"Volume"], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
      colnames(total_produtor_volume) <- c("Month","Year","Volume_M")
      total_produtor_volume$Concat <- paste(total_produtor_volume$Month,total_produtor_volume$Year, sep = " - ")
      
      MBI2 <- dplyr::left_join(MBI2,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
      MBI2$Contribution_volume1 <- round(MBI2$Volume/MBI2$Volume_M,6)
      MBI2 <- MBI2[MBI2$Volume!=0,]
      MBI2$Contribution_Month <- ifelse(MBI2$Month==1,12,MBI2$Month-1)
      MBI2$Contribution_Year <-  ifelse(MBI2$Contribution_Month==12,MBI2$Year-1,MBI2$Year) 
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year,MBI2$Brand, sep = " - ")  
      MBI12 <- MBI2[,c("Contribution_volume1","Concat")]
      MBI2$Concat <- paste(MBI2$Contribution_Month,MBI2$Contribution_Year,MBI2$Brand, sep = " - ")  
      MBI2 <- dplyr::left_join(MBI2, MBI12, by = "Concat")
      MBI2$Contribution_Rate <- ifelse(is.na(MBI2$Contribution_volume1.y),0,MBI2$Contribution_volume1.x-MBI2$Contribution_volume1.y)
      #MBI2 <- na.omit(MBI12)
      
      #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
      {
        pr <- aggregate(MBI2[,c("Volume","Contribution_Rate")], by = list(Month = MBI2$Month,Year = MBI2$Year,Producer = MBI2$Producer), sum)
        pr$Concat <- paste(pr$Month,pr$Year, sep = " - ")
        pr <- dplyr::left_join(pr,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
        
        pr$Share_Volume <- round(pr$Volume/pr$Volume_M,4)
        pr$pr_Month <- ifelse(pr$Month==1,12,pr$Month-1)
        pr$pr_Year <-  ifelse(pr$pr_Month==12,pr$Year-1,pr$Year)
        pr$Concat <- paste(pr$Month,pr$Year,pr$Producer, sep = " - ")  
        pr2 <- pr[,c("Share_Volume","Concat")]
        pr$Concat <- paste(pr$pr_Month,pr$pr_Year,pr$Producer, sep = " - ")
        pr <- dplyr::left_join(pr, pr2, by = "Concat")
        pr$Delta_Share_Volume <- ifelse(is.na(pr$Share_Volume.y),0,pr$Share_Volume.x-pr$Share_Volume.y)
        pr$Contribution_Rate <- round(pr$Contribution_Rate,4)
        pr$validation <- pr$Contribution_Rate-pr$Delta_Share_Volume
      }
      
      h <- MBI2%>%
        plot_ly(type = "scatter", mode = "lines")%>%
        add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Contribution_Rate,  color = ~Brand)%>%
        layout(title = 'Contribution Goodman Rate',
               yaxis = list(zeroline = FALSE, title = "Cherry Contribution Share Volume"),bargap = 5,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
               showlegend = T)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
    }
    #SHARE VOLUME
    mercado_grafico42 <- function(){
      MBI <- manipulacao()
      #CONSIDERANDO TODAS OS ATRIBUTOS DO PRODUTO DENTRO DO UNIVERSO DO SABOR
      MBI <- MBI[MBI$Flavor==c("Cherry") & MBI$Producer=="Chandler",]
      MBI2 <- aggregate(MBI[,"Volume"], by = list(Producer = MBI$Producer, Brand = MBI$Brand, Month = MBI$Month, Year = MBI$Year), FUN = sum)
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year, sep = " - ")
      
      total_produtor_volume <- aggregate(MBI[,"Volume"], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
      colnames(total_produtor_volume) <- c("Month","Year","Volume_M")
      total_produtor_volume$Concat <- paste(total_produtor_volume$Month,total_produtor_volume$Year, sep = " - ")
      
      MBI2 <- dplyr::left_join(MBI2,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
      MBI2$Contribution_volume1 <- round(MBI2$Volume/MBI2$Volume_M,6)
      MBI2 <- MBI2[MBI2$Volume!=0,]
      MBI2$Contribution_Month <- ifelse(MBI2$Month==1,12,MBI2$Month-1)
      MBI2$Contribution_Year <-  ifelse(MBI2$Contribution_Month==12,MBI2$Year-1,MBI2$Year) 
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year,MBI2$Brand, sep = " - ")  
      MBI12 <- MBI2[,c("Contribution_volume1","Concat")]
      MBI2$Concat <- paste(MBI2$Contribution_Month,MBI2$Contribution_Year,MBI2$Brand, sep = " - ")  
      MBI2 <- dplyr::left_join(MBI2, MBI12, by = "Concat")
      MBI2$Contribution_Rate <- ifelse(is.na(MBI2$Contribution_volume1.y),0,MBI2$Contribution_volume1.x-MBI2$Contribution_volume1.y)
      #MBI2 <- na.omit(MBI12)
      
      #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
      {
        pr <- aggregate(MBI2[,c("Volume","Contribution_Rate")], by = list(Month = MBI2$Month,Year = MBI2$Year,Producer = MBI2$Producer), sum)
        pr$Concat <- paste(pr$Month,pr$Year, sep = " - ")
        pr <- dplyr::left_join(pr,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
        
        pr$Share_Volume <- round(pr$Volume/pr$Volume_M,4)
        pr$pr_Month <- ifelse(pr$Month==1,12,pr$Month-1)
        pr$pr_Year <-  ifelse(pr$pr_Month==12,pr$Year-1,pr$Year)
        pr$Concat <- paste(pr$Month,pr$Year,pr$Producer, sep = " - ")  
        pr2 <- pr[,c("Share_Volume","Concat")]
        pr$Concat <- paste(pr$pr_Month,pr$pr_Year,pr$Producer, sep = " - ")
        pr <- dplyr::left_join(pr, pr2, by = "Concat")
        pr$Delta_Share_Volume <- ifelse(is.na(pr$Share_Volume.y),0,pr$Share_Volume.x-pr$Share_Volume.y)
        pr$Contribution_Rate <- round(pr$Contribution_Rate,4)
        pr$validation <- pr$Contribution_Rate-pr$Delta_Share_Volume
      }
      
      h <- MBI2%>%
        plot_ly(type = "scatter", mode = "lines")%>%
        add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Contribution_Rate,  color = ~Brand)%>%
        layout(title = 'Contribution Chandler Rate',
               yaxis = list(zeroline = FALSE, title = "Cherry Contribution Share Volume"),bargap = 5,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
               showlegend = T)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
    }
  } 
  
  #COFFEE ANALYSE
  {
    #SHARE VOLUME
    mercado_grafico43 <- function(){
      MBI <- manipulacao()
      #CONSIDERANDO TODAS OS ATRIBUTOS DO PRODUTO DENTRO DO UNIVERSO DO SABOR
      MBI <- MBI[MBI$Flavor==c("Coffee") & MBI$Producer=="Chandler",]
      MBI2 <- aggregate(MBI[,"Volume"], by = list(Producer = MBI$Producer, Brand = MBI$Brand, Month = MBI$Month, Year = MBI$Year), FUN = sum)
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year, sep = " - ")
      
      total_produtor_volume <- aggregate(MBI[,"Volume"], by = list(Month = MBI$Month, Year = MBI$Year), FUN = sum)
      colnames(total_produtor_volume) <- c("Month","Year","Volume_M")
      total_produtor_volume$Concat <- paste(total_produtor_volume$Month,total_produtor_volume$Year, sep = " - ")
      
      MBI2 <- dplyr::left_join(MBI2,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
      MBI2$Contribution_volume1 <- round(MBI2$Volume/MBI2$Volume_M,6)
      MBI2 <- MBI2[MBI2$Volume!=0,]
      MBI2$Contribution_Month <- ifelse(MBI2$Month==1,12,MBI2$Month-1)
      MBI2$Contribution_Year <-  ifelse(MBI2$Contribution_Month==12,MBI2$Year-1,MBI2$Year) 
      MBI2$Concat <- paste(MBI2$Month,MBI2$Year,MBI2$Brand, sep = " - ")  
      MBI12 <- MBI2[,c("Contribution_volume1","Concat")]
      MBI2$Concat <- paste(MBI2$Contribution_Month,MBI2$Contribution_Year,MBI2$Brand, sep = " - ")  
      MBI2 <- dplyr::left_join(MBI2, MBI12, by = "Concat")
      MBI2$Contribution_Rate <- ifelse(is.na(MBI2$Contribution_volume1.y),0,MBI2$Contribution_volume1.x-MBI2$Contribution_volume1.y)
      #MBI2 <- na.omit(MBI12)
      
      #PROVA REAL DO SHARE VOLUME FECHANDO 1 NO MES E ANO
      {
        pr <- aggregate(MBI2[,c("Volume","Contribution_Rate")], by = list(Month = MBI2$Month,Year = MBI2$Year,Producer = MBI2$Producer), sum)
        pr$Concat <- paste(pr$Month,pr$Year, sep = " - ")
        pr <- dplyr::left_join(pr,total_produtor_volume[,c("Concat","Volume_M")], by = "Concat")
        
        pr$Share_Volume <- round(pr$Volume/pr$Volume_M,4)
        pr$pr_Month <- ifelse(pr$Month==1,12,pr$Month-1)
        pr$pr_Year <-  ifelse(pr$pr_Month==12,pr$Year-1,pr$Year)
        pr$Concat <- paste(pr$Month,pr$Year,pr$Producer, sep = " - ")  
        pr2 <- pr[,c("Share_Volume","Concat")]
        pr$Concat <- paste(pr$pr_Month,pr$pr_Year,pr$Producer, sep = " - ")
        pr <- dplyr::left_join(pr, pr2, by = "Concat")
        pr$Delta_Share_Volume <- ifelse(is.na(pr$Share_Volume.y),0,pr$Share_Volume.x-pr$Share_Volume.y)
        pr$Contribution_Rate <- round(pr$Contribution_Rate,4)
        pr$validation <- pr$Contribution_Rate-pr$Delta_Share_Volume
      }
      
      h <- MBI2%>%
        plot_ly(type = "scatter", mode = "lines")%>%
        add_trace(x= list(Year = MBI$Year,Month = MBI$Month), y = ~Contribution_Rate,  color = ~Brand)%>%
        layout(title = 'Contribution Ali Rate',
               yaxis = list(zeroline = FALSE, title = "Coffee Contribution Share Volume"),bargap = 5,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE,title = "Date"),
               showlegend = T)
      
      h1 <- div(h, align = "center")
      
      return(suppressWarnings(print(h1)))
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
      x1 <- ggplotly(x)
      x2 <- div( x1, align="center" )
      return(x2)
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
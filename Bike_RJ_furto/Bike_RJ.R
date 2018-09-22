#Instalador pacote
{
  
  install.packages("rlang")
  install.packages('pdftools') 
  install.packages('rgdal')
  install.packages('cartography')
  install.packages("maptools")
  install.packages("gganimate")
  install.packages("glue")
  install.packages("ggplot2")
  install.packages("gganimate")
  install.packages("CaTools")
}
#Instalador biblioteca
{
  library(glue)
  library(rlang)
  library(pdftools)  
  library(tidyverse)
  library(rgdal)
  library(sp)
  library(RColorBrewer)
  library(cartography)
  library(maptools)
  library(plotly)
  library(readxl)
  library(gganimate)
  library(caTools)
}

pdfs <- c('072018','062018','052018')
#LEITURA DOS PD'S DO IPS DO RIO DE JANEIRO
for (i in 1:length(pdfs)) {
#CARREGAR O PDF
  {
    e <- pdfs[i]
    setwd <- "C:/Users/Arthur Vaz/Desktop/R_Map"
    bairro_rj = readOGR("C:/Users/Arthur Vaz/Desktop/R_Map/Limite_Bairro.shp")
    x <- pdf_text(paste('C:/Users/Arthur Vaz/Desktop/R_Map/DadosBicicleta',e,'.pdf',sep = ""))
    bairros_cod <- read_excel("C:/Users/Arthur Vaz/Desktop/R_Map/bairros_cod.xlsx")
    pdf_x <- strsplit(x, "\n")
    k <- length(pdf_x[[1]]) + length(pdf_x[[2]]) + length(pdf_x[[3]]) + length(pdf_x[[4]]) + length(pdf_x[[5]])
    df <- data.frame(matrix(unlist(pdf_x), nrow=k, byrow=T),stringsAsFactors=FALSE)
    colnames(df) <- 'Bairro'
    total <- which(grepl(pattern = 'Total', x = df$Bairro)==TRUE)
    for (i in 1:length(total)) {
      if(i==1){
        bairro <- df[1:total[i]+1,]
      }
      if(i==2){
        dia <- df[(total[i-1]+1):total[i]+1,]
      }
      if(i==3){
        hora <- df[(total[i-1]+1):total[i]+1,]
      }
    }
    rm(df, pdf_x,x,i,total)
    
    bairro_rj@data <- bairro_rj@data[,-2]
  }
  #BAIRRO
  {
    bairro <- as.data.frame(bairro)
    bairro <- bairro[-1:-2,]
    bairro <- as.data.frame(bairro)
    bairro$bairro <- as.character(bairro$bairro)
    x <- strsplit(bairro$bairro,"\\s{2,}")
    p <- sapply(x, function(z){
      z[1]
    })
    s <- sapply(x, function(z){
      z[2]
    })
    t <- sapply(x, function(z){
      z[3]
    })
    p <- as.data.frame(p)
    s <- as.data.frame(s)
    t <- as.data.frame(t)
    bairro <- cbind(p,s,t) 
    rm(p,s,t,x)
    colnames(bairro) <- c('Delegacia','Furto de Bicicleta','Roubo de Bicicleta')
    bairro$Delegacia <- as.character(bairro$Delegacia)
    bairro$BAIRRO <- str_split(bairro$Delegacia,' - ', simplify=TRUE)[,2]
    bairro <- bairro[-140:-141,]
    bairro$FB1 <- str_split(bairro$`Furto de Bicicleta`,' ', simplify=TRUE)[,1]
    bairro$FB2 <- str_split(bairro$`Furto de Bicicleta`,' ', simplify=TRUE)[,2]
    bairro$`Roubo de Bicicleta` <- ifelse(bairro$FB2=='',bairro$`Roubo de Bicicleta`,bairro$FB2)
    bairro$`Furto de Bicicleta` <- bairro$FB1
    bairro$`Furto de Bicicleta` <- as.numeric(bairro$`Furto de Bicicleta`)
    bairro$`Roubo de Bicicleta` <- as.numeric(bairro$`Roubo de Bicicleta`)
    bairro$roubo_geral <- apply(bairro[,c('Furto de Bicicleta','Roubo de Bicicleta')],1,sum)
    bairro$BAIRRO <- as.character(bairro$BAIRRO)
    bairro <- mutate_all(bairro, funs(toupper))
    bairro$roubo_geral <- as.numeric(bairro$roubo_geral)
    bairro_rj@data <- dplyr::left_join(bairro_rj@data,bairros_cod[,c("NOME","BAIRRO")], by = 'NOME')
    bairro_rj@data <- dplyr::left_join(bairro_rj@data,bairro[,c("BAIRRO","roubo_geral")], by = 'BAIRRO')
    bairro_rj@data$roubo_geral <- as.numeric(bairro_rj@data$roubo_geral)
    bairro[is.na(bairro)] <- 0
    bairro_rj@data$id <-  rownames(bairro_rj@data)
    bairrorj2 <- fortify(bairro_rj, region = "id")
    bairrorj2 <- dplyr::left_join(bairrorj2, bairro_rj@data, by='id')
    bairrorj2[is.na(bairrorj2)] <- 0
    bikes_rj <- bairrorj2
    bikes_rj$mes <- e
    if(!exists('bikesrj')){
      bikesrj <- bikes_rj
    }else{
      bikesrj <- rbind(bikesrj,bikes_rj)  
    }
    rm(bairro,bairro_rj,bikes_rj,bairrorj2,bairros_cod,hora,dia,k,x)
  }
}
  #GRAFICOS
  {
  #GRAFICO TIPO I
  {
    intervalos1=quantile(bairro$roubo_geral, probs = seq(0,1,0.125))
    intervalos1[9] = intervalos1[9] + 1
    #Plotando o mapa com tons avermelhados
    summary(bairro_rj@data$roubo_geral)
    #Mapas coropleticos
    par(mfrow = c(1,2))
    par(mar = c(0.5,0.5,1.5,0.5))
    
    spplot(obj = bairro_rj, zcol = c("roubo_geral"), at = intervalos1, col.regions = brewer.pal(8, "Reds")) #Outras opÃ§Ãµes de cores: Greens, BrBG, Accent  
  }
  #GRAFICO TIPO II
  {
    plot(bairro_rj)
    choroLayer(spdf = bairro_rj, var = "roubo_geral", border = "gray",  
               method = "equal", # "sd", "equal", "quantile", "fisher-jenks","q6" or "geom"  
               nclass = 5, lwd = 0.4, col = brewer.pal(5, "Reds"),  
               legend.pos = "topleft", legend.title.txt = "Numero de Roubo", add= TRUE)   
  }
  #GRAFICO TIPO III
  {
    x <- ggplot(data = bikesrj, aes(x = long, y = lat, group = group, fill=roubo_geral, frame = mes)) +
      #ggtitle(title = 'Mes e ano: {frame_time}') +
      labs(title = 'Roubo de Bike no RJ', x = 'Longitude', y = 'Latitude') +
      geom_polygon() +
      geom_path(color="white") +
      coord_equal() + 
      scale_fill_gradient(low="white",high="darkred") + 
      ease_aes('linear')
    
    x1 <- ggplotly(x)

  }
  #GRAFICO IV
  {
      x <- ggplot(data = bikesrj, aes(x = long, y = lat, group = group, fill=roubo_geral)) +
        #ggtitle(title = 'Mes e ano: {frame_time}') +
        labs(title = 'Roubo de Bike no RJ', x = 'Longitude', y = 'Latitude') +
        geom_polygon() +
        geom_path(color="white") +
        coord_equal() + 
        scale_fill_gradient(low="white",high="darkred") + 
        #ease_aes('linear') +
        transition_states(
          mes,
          transition_length = 2,
          state_length = 1
        ) +
        enter_fade() + 
        exit_shrink() +
        ease_aes('sine-in-out')
      
      x2 <- animate(x)
      magick::image_write(x, path="myanimation.gif")
      
      
    }
  }

  #DIA
  {
  dia <- as.data.frame(dia)
  dia <- dia[-1:-4,]
  dia <- as.data.frame(dia)
  dia$dia <- as.character(dia$dia)
  x <- strsplit(dia$dia,"\\s{1,}")
  p <- sapply(x, function(z){
    z[1]
  })
  s <- sapply(x, function(z){
    z[2]
  })
  t <- sapply(x, function(z){
    z[3]
  })
  p <- as.data.frame(p)
  s <- as.data.frame(s)
  t <- as.data.frame(t)
  dia <- cbind(p,s,t) 
  rm(p,s,t,x)
  colnames(dia) <- c('Dia','Furto de Bicicleta','Roubo de Bicicleta')
}

  #HORA
  {
    hora <- as.data.frame(hora)
    hora <- hora[-1:-4,]
    hora <- as.data.frame(hora)
    hora$hora <- as.character(hora$hora)
    x <- strsplit(hora$hora,"\\s{2,}")
    p <- sapply(x, function(z){
      z[1]
    })
    s <- sapply(x, function(z){
      z[2]
    })
    t <- sapply(x, function(z){
      z[3]
    })
    p <- as.data.frame(p)
    s <- as.data.frame(s)
    t <- as.data.frame(t)
    hora <- cbind(p,s,t) 
    rm(p,s,t,x)
    colnames(hora) <- c('Hora','Furto de Bicicleta','Roubo de Bicicleta')
}

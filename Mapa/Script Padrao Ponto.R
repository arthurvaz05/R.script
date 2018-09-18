#-------------------------------------------------------------------------------------#
# -------------------- Mapa com a localização exata de um evento  --------------------#
# -------------------- Mapa com a localização dos delitos em NY  ---------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote rgdal, maptools e dplyr
library(rgdal)
library(maptools)
library(dplyr)

#Importando o shapefile de Nova York
NYShp <- readOGR("nyshape.shp")

#Carregando o pacote readr
library(readr)

#Importando o arquivo com as localizações dos delitos em NY
NYPD = read_csv2("NYPD16jan.csv")

#Carregando o pacote spatstat
library(spatstat)

#Definindo o shapefile como uma janela onde os pontos serao plotados - necessario para o uso do pacote spatstat
NYO <- as.owin(NYShp)

#Plotando o shapefile
plot(NYO)

#Criando o padrao de pontos a ser plotado
NYppp = ppp(NYPD$Longitude, NYPD$Latitude, window=NYO)

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias de crimes em Nova York")

#Estimando o efeito de primeira ordem (intensidade) usando diferentes kernels
NYkde.q = density.ppp(x = NYppp, sigma=0.01, kernel="quartic")
NYkde.g = density.ppp(x = NYppp, sigma=0.01, kernel="gaussian")
NYkde.e = density.ppp(x = NYppp, sigma=0.01, kernel="epanechnikov")

##density.ppp - calcula a funcao de intensidade de acordo com o kernel escolhido
#Argumentos:
#x - objeto da classe ppp
#sigma - é o valor do raio (tau na expressao dos slides)
#kernel - o kernel que deseja-se usar

#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(2,2))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(NYkde.q, main="Kernel Quartico", cex.main=0.5)
plot(NYkde.g, main="Kernel Normal")
plot(NYkde.e, main="Kernel Epanechnikov")
par(mfrow=c(1,1))

#Avaliando o impacto de diferentes raios (tau)
par(mfrow=c(3,2))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(density.ppp(NYppp, sigma=0.005, kernel="gaussian"), main="Sigma=0.005", cex.main=0.5)
plot(density.ppp(NYppp, sigma=0.01, kernel="gaussian"), main="Sigma=0.01", cex.main=0.5)
plot(density.ppp(NYppp, sigma=0.02, kernel="gaussian"), main="Sigma=0.02", cex.main=0.5)
plot(density.ppp(NYppp, sigma=0.05, kernel="gaussian"), main="Sigma=0.05", cex.main=0.5)
plot(density.ppp(NYppp, sigma=0.5, kernel="gaussian"), main="Sigma=0.5", cex.main=0.5)
par(mfrow=c(1,1))

#Carregando o pacote ggmap
library(ggmap)

#Plotando o grafico com recursos do Google Maps
nyhyb = get_googlemap('New York City',zoom=10,maptype='hybrid')
ggmap(nyhyb)

#Criando o grafico com a densidade e o layout do Google Maps
google = ggmap(nyhyb) + stat_density2d(aes(x=Longitude,y=Latitude, fill = ..level..), alpha = .8, h=.025, n = 400,geom = "polygon", data = NYPD) 
plot(google)

##stat_density2d - estima um kernel gaussiano
#Argumento:
#h - raio
#n - numero de pontos na grade
#alpha - opacidade

#Fazendo o plot da densidade considerando os diferentes tipos de crime
google + scale_fill_gradient(low = "black", high= "red") + facet_wrap(~ TYPE)

#Funcao que estima o raio por meio de validacao cruzada (custosa computacionalmente)
#raio.est = bw.diggle(NYppp)
#raio.est
#plot(raio.est)

#Sorteando uma amostra de tamanho 100 para estudar o efeito de segunda ordem por conta do custo computacional
aNY = sample_n(NYPD,100)
aNYppp = ppp(aNY$Longitude, aNY$Latitude, window=NYO)
plot(aNYppp, pch=21, cex=0.9, bg="blue", main="Amostra")

#Estimando a funcao G
NY.G = Gest(aNYppp)

#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao K
NY.K = Kest(aNYppp)

#Kest - estima a funcao K de Ripley de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao F
NY.F = Fest(aNYppp)

#Fest - estima a funcao F de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Plotando as funcoes G, K e F
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(NY.G, main="Funcao G")
plot(NY.K, main="Funcao K")
plot(NY.F, main="Funcao F")
par(mfrow = c(1,1))

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(aNYppp)

#Realizando o teste de Hopkins-Skellam de Completa aleatoriedade espacial para verificar agregacao espacial
hopskel.test(aNYppp, alternative="clustered")

#Funcoes para estimar os envelopes das funcoes F, G e K
#Kest=envelope(aNYppp,Kest,nsim=10) #alto custo computacional
Gest=envelope(aNYppp,fun = Gest,nsim=10)
Fest=envelope(aNYppp,fun = Fest,nsim=10)

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(aNYppp, pch=21, cex=0.9, bg="blue")
#plot(aKest)
plot(Gest)
plot(Fest)
par(mfrow=c(1,1))










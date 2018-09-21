#Instalacoes
{
  install.packages("rgdal")
  install.packages("maptools")
  install.packages("spatstat")
  install.packages("ggmap")
}
#Biblioteca
{
  library(rgdal)
  library(maptools)
  library(dplyr)
  library(readr)
  library(spatstat)
  library(ggmap)
}

Base_Houston <- read_csv("Base Houston.csv")
HShp <- readOGR("/Users/arthurlambletvaz/Desktop/Github/R.script/Mapa/Houston_City_Limit.shp")
HShp = spTransform(HShp, CRS("+proj=longlat +datum=WGS84"))
HO <- as.owin(HShp)

plot(HO)

Houston = ppp(Base_Houston$lon, Base_Houston$lat, window=HO)

par(mar=c(0.5,0.5,1.5,0.5))
plot(Houston, pch=21, cex=0.9, bg="blue", main="Ocorrencias de crimes em Houston")



#usando diferente kernels
Houston.q = density.ppp(x = Houston, sigma=0.01, kernel="quartic")
Houston.g = density.ppp(x = Houston, sigma=0.01, kernel="gaussian")
Houston.e = density.ppp(x = Houston, sigma=0.01, kernel="epanechnikov")

par(mfrow=c(2,2))
plot(Houston, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(Houston.q, main="Kernel Quartico", cex.main=0.5)
plot(Houston.g, main="Kernel Normal")
plot(Houston.e, main="Kernel Epanechnikov")
par(mfrow=c(1,1))


par(mfrow=c(3,2))
plot(Houston, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(density.ppp(Houston, sigma=0.005, kernel="gaussian"), main="Sigma=0.005", cex.main=0.5)
plot(density.ppp(Houston, sigma=0.01, kernel="gaussian"), main="Sigma=0.01", cex.main=0.5)
plot(density.ppp(Houston, sigma=0.02, kernel="gaussian"), main="Sigma=0.02", cex.main=0.5)
plot(density.ppp(Houston, sigma=0.05, kernel="gaussian"), main="Sigma=0.05", cex.main=0.5)
plot(density.ppp(Houston, sigma=0.5, kernel="gaussian"), main="Sigma=0.5", cex.main=0.5)
par(mfrow=c(1,1))


Houston.hyb = get_googlemap('Houston City',zoom=10,maptype='hybrid')
ggmap(Houston.hyb)

google = ggmap(Houston.hyb) + stat_density2d(aes(x=lon,y=lat, fill = ..level..), alpha = .8, h=.025, n = 400,geom = "polygon", data = Base_Houston) 
plot(google)


google + scale_fill_gradient(low = "black", high= "red") + facet_wrap(~ offense)


aHous = sample_n(Base_Houston,100)
aHousppp = ppp(aHous$lon, aHous$lat, window=HO)
plot(aHousppp, pch=21, cex=0.9, bg="blue", main="Amostra")

HO.G = Gest(aHousppp)
HO.K = Kest(aHousppp)
HO.F = Fest(aHousppp)

par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(HO.G, main="Funcao G")
plot(HO.K, main="Funcao K")
plot(HO.F, main="Funcao F")
par(mfrow = c(1,1))

clarkevans.test(aHousppp)

hopskel.test(aHousppp, alternative="clustered")

Gest=envelope(aHousppp,fun = Gest,nsim=10)
Fest=envelope(aHousppp,fun = Fest,nsim=10)
Kest=envelope(aHousppp,Kest,nsim=10)

par(mfrow=c(2,2))
plot(aHousppp, pch=21, cex=0.9, bg="blue")
plot(Kest)
plot(Gest)
plot(Fest)
par(mfrow=c(1,1))



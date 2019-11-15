


myData <- data.frame(Campaign = c('Youtube ads','Facebook ads',
                                  'Mala direta','Google ads','Waze ads','Influeciadora',
                                  'Desconto ecommerce','Degustacao','SMS','Whatsapp'),
                     Channel = c('Social Media','Social Media','Email','Internet','App transito',
                                 'Boca a boca','Ecommerce','Loja','Celular','Celular'),
                     Return = c(42746,161853,5071,25046,99865,1307,4095,350,1246,38773),
                     Spend = c(13415,49659,1993,6732,27014,636,1646,101,402,9857))

#  Plot data  #
channelName = as.character(myData$Channel[1])
maxX = 1.05*max(myData$Spend)
maxY = 1.05*max(myData$Return)

myPlotDataDF = data.frame(Return = myData$Return, Spend = myData$Spend)

simpleScatterPlot <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black") +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) + 
  ggtitle(paste(channelName))

simpleScatterPlot


# Just as a linear model is governed by parameters (slope and intercept), the ADBUDG model is also governed by parameters.  There are four ADBUDG model parameters:
#   
# A - The maximum amount of return possible for a campaign given a long term investment.
# B - The minimum amount of return possible for a campaign given a long term investment
# C - Controls the shape of the curve.
# D - Represents initial market share or market saturation effects.

# The nlminb() function has several arguments.  We are only going to require the following:
#   
# objective - The function to be minimized
# start - Initial values for the parameters to be optimized
# lower - Lower bound for constrainted parameter optimization
# upper - Upper bound for constrained parameter optimization
# control - Additional control parameters

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}


startValVec = c(25000,100,1.5,100000)
minValVec = c(0,0,1.01,0)
maxValVec = c(500000, 500000, 2, 10000000)


optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = myData$Spend,
                    Return = myData$Return)

optim.parms


a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(myData$Spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = curveDFx, Return = curveDFy)

maxX = 1.05*max(curveDFx, max(myData$Spend))
maxY = 1.05*max(curveDFy, max(myData$Return))

myPlotDataDF = data.frame(Return = myData$Return, Spend = myData$Spend)
optimLineDF = data.frame(Spend = curveDFx, Return = curveDFy)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen"))  +
  scale_color_manual(labels = "Optimized ADBUDG Fit",values=c('darkgreen')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) + 
  ggtitle(paste(channelName, "Data & Model Fit", sep = " "))

scatterPlotPlusFit


adbudgReturn = function(a,b,c,d,Spend){
  adbudgReturn = sum(b+(a-b)*((Spend^c)/(d+(Spend^c))))
  return(adbudgReturn)
}

returnGoal = 400000
increment = 1000
oldSpendVec = myData$Spend
oldReturn = adbudgReturn(a,b,c,d,oldSpendVec)
newSpendVec = oldSpendVec

totalSpend = sum(oldSpendVec)
totalReturn = oldReturn
j = 1 
totalReturn_list = NULL

while(totalReturn < returnGoal){
  incReturns = NULL
  for(i in 1:length(oldSpendVec)){
    oldSpendTemp = newSpendVec[i]
    newSpendTemp = newSpendVec[i] + increment
    
    oldReturnTemp = b+(a-b)*((oldSpendTemp^c)/(d+(oldSpendTemp^c)))
    newReturnTemp = b+(a-b)*((newSpendTemp^c)/(d+(newSpendTemp^c)))
    
    incReturns[i] = newReturnTemp - oldReturnTemp
    
  }
  
  winner = which.max(incReturns)
  newSpendVec[winner] = newSpendVec[winner] + increment
  
  totalSpend = totalSpend + increment
  totalReturn = adbudgReturn(a,b,c,d,newSpendVec)
  totalReturn_list[j] = totalReturn
  j = j + 1
  if(j>200){
    if((totalReturn_list[(j-5):(j-1)]%>%mean())%>%as.numeric()%>%round(0)==totalReturn_list[j-1]%>%as.numeric()%>%round(0)){
      break
    }  
  }
  
}

plot(totalReturn_list)

plot(incReturns)


newReturnVec = b+(a-b)*((newSpendVec^c)/(d+(newSpendVec^c)))

myRecommendedData = data.frame(Campaign = myData$Campaign,
                               Channel = myData$Channel,
                               Return = newReturnVec,
                               Spend = newSpendVec)

sum(myRecommendedData$Spend) # Recommended Spend
sum(myRecommendedData$Return)  # Estimated Return from Recommended Spend
sum(myRecommendedData$Spend)/sum(myData$Spend) - 1  # % Increase in Spend to get $600K
sum(myRecommendedData$Spend)-sum(myData$Spend)

#  Graph current spend vs recommended spend  #
compareDF = data.frame(Campaign = rep(myData$Campaign,2), 
                       spendType = rep(c("Actual Spend","Recommended Spend"), 
                                       each=dim(myData)[1]), 
                       Spend = c(myData$Spend, myRecommendedData$Spend))

barChart <- ggplot(data=compareDF, aes(x=Campaign, y=Spend, fill=spendType)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_manual(values=c('darkred','darkblue'),
                    name = "") +
  scale_y_continuous(name="Spend", labels = dollar) +
  theme(axis.text.x = element_text(angle = 45, hjust = .75)) +
  ggtitle("Breakdown of Spend by Campaign")

barChart


percDiff = (myRecommendedData$Spend - myData$Spend)/myData$Spend
summaryDF = data.frame(Campaign = myRecommendedData$Campaign, 
                       Channel = myRecommendedData$Channel, 
                       actualSpend = dollar_format()(myData$Spend), 
                       recommendedSpend = dollar_format()(myRecommendedData$Spend),
                       percDiff = percent((myRecommendedData$Spend - myData$Spend)/myData$Spend))

summaryDF

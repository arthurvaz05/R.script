acum <- aggregate(M$Volume, by = list(M$Date), sum)
acum$Month <- month(acum$Group.1)
acum$Year <- year(acum$Group.1)

train <- acum[acum$Month<=7 & acum$Year==2017 |acum$Year==2016 | acum$Year==2015 , c('Group.1',"x") ]
test <- acum[acum$Month>7 & acum$Year==2017, c('Group.1',"x")]

colnames(train) <- c('ds', 'y')
train <- na.omit(train)
M1 <- prophet::prophet(train)

future <- make_future_dataframe(M1, periods = 365)
forecast <- predict(M1, future)
plot(M1, forecast)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast)
forc <- forecast[,c("ds","yhat")]

colnames(forc) <- c("Group.1","x")

test <- dplyr::left_join(test,forc, by = "Group.1")

prophet_plot_components(M1, forecast)

install.packages('Metrics')
library('Metrics')

help(Prophet)

rmse(test$x.x,test$x.y)
rmsle(test$x.x,test$x.y)
mape(test$x.x,test$x.y)
mape(test$Price,test$x)

M <- MBI_MERCADO[,c("Date","Volume","Value")]
acum <- aggregate(M[,c("Volume","Value")], by = list(M$Date), sum)
acum$Month <- month(acum$Group.1)
acum$Year <- year(acum$Group.1)
acum$Price <- round(acum$Value/acum$Volume,2)

train <- acum[acum$Month<=7 & acum$Year==2017 |acum$Year==2016 | acum$Year==2015 , c('Group.1',"Price") ]
test <- acum[acum$Month>7 & acum$Year==2017, c('Group.1',"Price")]


MBI_tb[MBI_tb$Atributos=="Coverage" & MBI_tb$Year==2017,]


teste <- dplyr::left_join(MBI1, price_market, by = c("Month","Year"))
teste$Price_index2 <- teste$Price.x/teste$Price.y
summary(teste$Price_index2)
hist(teste$Price_index)
MBI1[is.na(MBI1$Price_index),]
MBI1[MBI1$Year==2017 & MBI1$Brand=="Market",]

price_market[which(paste(MBI1$Month,MBI1$Year,"Market", sep = " - ") %in% paste(price_market$Month,price_market$Year,"Market", sep = " - ")),"Price"]

table(MBI$Flavor)

table(MBI$Flavor)

mercado_grafico21()
mercado_grafico22()
mercado_grafico23()
mercado_grafico24()
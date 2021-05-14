library(dplyr)
install.packages('pander')
library(pander)
install.packages('MLmetrics')
library(MLmetrics)
library(ggplot2)
library(rpart)

cm <- '/Users/arthurvaz/R.script/Correlacao/exemplo_covid_r/covid19.csv'

# Let's generate the data again
set.seed(42)
x <- seq(-1,1,0.01)

d <- data.frame(x = x, 
                y = sqrt(1 - x^2) + rnorm(length(x),mean = 0, sd = 0.05))


preds <- predict(rpart(y~x, data = d, method = "anova"), type = "vector")

# Set up a chart
ggplot(data = d, mapping = aes(x = x)) +
  geom_point(aes(y = y), size = 0.5) +
  geom_line(aes(y=preds, color = '2')) +
  scale_color_brewer(name = "", labels='CART', palette="Set1")


d$mean <- d$y%>%mean()
d$cart <- preds
#Métrica de erro do modelo preditivo
MAE(d$cart,d$y)
#Métrica de erro do modelo baseline
MAE(d$mean,d$y)









covid <- read.csv(cm, 
               stringsAsFactors = FALSE,
               na.strings=c("","NA") # read in blanks as NAs
)%>% 
  select(-starts_with("cxr"))

dx2y(covid, target = "covid_19_test_results", confidence = TRUE) %>% 
  filter(x2y >0) %>% 
  pander(split.tables = Inf)


dx2y(covid) %>%head(10) %>% pander

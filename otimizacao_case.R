install.packages('lpSolveAPI')
install.packages('reshape')
install.packages('gridExtra')


#Carregar as bases
library(lpSolveAPI)
library(reshape)
library(gridExtra)
library(dplyr)

#criar o dataframe
containers <- data.frame(containers=c('ct1','ct2','ct3'), peso=c(10,8,12), capacidade=c(5000,4000,8000))
containers%>%lapply(class)
containers
produtos <- data.frame(produtos=c('p1','p2','p3','p4','p5'),disponivel=c(18,10,5,20,9),volume=c(400,300,200,500,350),lucro=c(2000,2500,5000,3500,3000))
produtos%>%lapply(class)
produtos

modelo_lp <- make.lp(2*NROW(containers)+NROW(produtos),12)

coluna <- 0 
row <- 0

#criar um modelo lp para cada coluna
for(ct in containers$containers){
  row <- row+1
  for (prod in seq(1,NROW(produtos$produtos))) {
    coluna <- coluna + 1
    
    set.column(modelo_lp, coluna,c(1,produtos[produtos,'volume'],1), indices = c(row,NROW(containers)+row,NROW(containers)*2+produtos))
  }
}

#restricao de peso
set.constr.value(modelo_lp, rhs = containers$peso, constraints = seq(1,NROW(containers)))

#restricao de capacidade
set.constr.value(modelo_lp, rhs = containers$capacidade, constraints = seq(1,NROW(containers)+1,NROW(containers)*2))

#restricao de capacidade
set.constr.value(modelo_lp, rhs = containers$capacidade, constraints = seq(1,NROW(containers)+1,NROW(containers)*2))

#restricao de disponivel
set.constr.value(modelo_lp, rhs = produtos$disponivel, constraints = seq(1,NROW(containers)*2+1,NROW(containers)*2+NROW(produtos)))

#funcao objetiva direcao
lp.control(modelo_lp, sense = 'max')

#visualizar o problema estrutura
write.lp(modelo_lp,'model.lp',type='lp')

require(lpSolveAPI)

#7 variaveis que representam os canais digitais
model<-make.lp(ncol=7)
#a função para maximizar
m1<-lp.control(model, sense="max", verbose="neutral")
#Função objetiva, lembrando que o vetor necessita ser do tamanho da quantidade de variaveis
m2<-set.objfn(model, obj=c(1/250,1/200,1/150,1/100,1/100,1/300,1/150))
#Colocar limites minimos de investimento para cada variavel
m3<-set.bounds(model, lower=c(250000,200000,75000,50000,20000,40000,35000))
#Combinacoes binarias de investimento em diferentes canais
m4<-add.constraint(model, c(1,1,0,0,0,0,0), "<=",600000)
m5<-add.constraint(model, c(1,1,1,1,1,0,0), type="<=",1000000)
#Adicionar a restricao de LTV
m6<-add.constraint(model, c((1500/250-500/250),(800/200-500/200),(300/150-500/150),
                            (100/100-500/100),(100/100-500/100),(100/100-500/100),(100/100-500/100)), type=">=",0)

#Adicionando os nomes de cada variavel, restricao
rownames=c("facebook & Adword budget constraint","total budget","LTV")
colnames=c("Adword","Facebook","Email","Affiliated","instagram","Waze","Youtube")
dimnames(model)=list(rownames,colnames)
#Adicionar nome para o problema no caso especifico
name.lp(model,"Maximize Number Of Users")
#Salvar a estrutura do problema
getwd()
write.lp(model, filename="lp_model.txt")

#Executar o solver
solve(model)
get.variables(model)
get.objective(model)

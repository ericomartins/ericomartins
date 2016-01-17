source("Demo\\anexo04.R")

library(xgboost)
library(Matrix)

############################# PROCESSO 01 - CARGA #############################

ciclo_teste <- 2013
ciclo_alvo <- 2015
mes_inicio <- 8
ciclo_mes_prev   <- 5

casos <- read.csv("Demo\\anexo01.csv", encoding="UTF-8")

nrow(casos)
head(casos[casos$bairro == "Copacabana" & casos$caso_anomes == 200201,],1)




########################## PROCESSO 02 - TESTE INICIAL ########################
train02 <- subset(casos, ciclo_num != ciclo_alvo | (ciclo_num == ciclo_alvo & ciclo_mes <= ciclo_mes_prev))
train02 <- subset(train02, ciclo_num != ciclo_teste | (ciclo_num == ciclo_teste & ciclo_mes <= ciclo_mes_prev))
train02 <- subset(train02, ciclo_parte == "predicao")
train02 <- train02[order(train02$qtd_casos,train02$bairro,train02$caso_anomes),] 
test02  <- subset(casos, ciclo_num == ciclo_teste & ciclo_mes > ciclo_mes_prev)

modelopred02 <- criamodelo(train02,"qtd_casos")

predicao02 <- criapredicao(modelopred02,test02)

avaliapredicao(predicao02)



####################### PROCESSO 03 - FEATURES DERIVADAS ####################
casos03 <- criafeaturesderivadas(casos,ciclo_teste,ciclo_alvo)

nrow(casos03)
head(casos03[casos03$bairro == "Copacabana" & casos03$caso_anomes == 200201,],1)



########################## PROCESSO 04 - TESTE FINAL ########################
train04 <- subset(casos03, ciclo_num != ciclo_alvo | (ciclo_num == ciclo_alvo & ciclo_mes <= ciclo_mes_prev))
train04 <- subset(train04, ciclo_num != ciclo_teste | (ciclo_num == ciclo_teste & ciclo_mes <= ciclo_mes_prev))
train04 <- subset(train04, ciclo_parte == "predicao")
train04 <- train04[order(train04$qtd_casos,train04$bairro,train04$caso_anomes),] 

test04  <- subset(casos03, ciclo_num == ciclo_teste & ciclo_mes > ciclo_mes_prev)

modelopred04 <- criamodelo(train04,"qtd_casos")
predicao04 <- criapredicao(modelopred04,test04)

avaliapredicao(predicao04)


############################ PROCESSO 05 - VALIDACAO ##########################
valid05  <- subset(casos03, ciclo_num == ciclo_alvo & ciclo_mes > ciclo_mes_prev)
predicao05  <- criapredicao(modelopred04,valid05)

avaliapredicao(predicao05)






############################## GRAFICO 05 ##############################
dfgraf5 <- casos[casos$ciclo_num != 2015 & casos$ciclo_parte == "predicao",c("ciclo_mes","ciclo_num","qtd_casos")]
graf5 <- aggregate(qtd_casos ~ ciclo_mes + ciclo_num, data=dfgraf5,sum)
graf5 <- graf5[order(graf5$ciclo_num,graf5$ciclo_mes),] 
graf5real <- aggregate(qtd_casos ~ ciclo_mes + ciclo_num, data=casos[casos$ciclo_num == 2015 & casos$ciclo_parte == "predicao",c("ciclo_mes","ciclo_num","qtd_casos")],sum)
graf5real$ciclo_num <- 201501
graf5 <- rbind(graf5,graf5real)
graf5pred <- aggregate(pred ~ ciclo_mes + ciclo_num, data=predicao05,sum)
names(graf5pred)[names(graf5pred) == 'pred'] <- 'qtd_casos'
graf5pred$ciclo_num <- 201502
graf5 <- rbind(graf5,graf5pred)


#dev.off()
par(cex=.8)
plot( graf5[graf5$ciclo_num == 2003,c(1,3)],type="l",col="gray",
      xlim=c(6,12),ylim=c(0,50000), xaxt = "n",xlab="Ciclo_Mês - Mês",ylab="Nº casos",
      main="Rio de Janeiro - Validação da curva de predição")
lines(graf5[graf5$ciclo_num == 2002,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2004,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2005,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2006,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2007,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2008,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2009,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2010,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2011,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2012,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2013,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2014,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 2015,c(1,3)],type="l",col="gray")
lines(graf5[graf5$ciclo_num == 201501,c(1,3)],type="l",col="green")
lines(graf5[graf5$ciclo_num == 201502,c(1,3)],type="l",col="red",lty=2)

legend("topright", legend = c("Histórico","2015 - Real","2015 - Previsto"), col=c("gray","green","red"), pch=22)
axis(side = 1, at=6:12 ,labels = c("06-jan","07-fev","08-mar","09-abr","10-mai","11-jun","12-jul"))


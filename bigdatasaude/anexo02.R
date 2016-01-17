
casos <- read.csv("Demo\\anexo01.csv")

############################## GRAFICO 01 ##############################
graf1 <- aggregate(qtd_casos ~ caso_ano, data=casos[casos$caso_ano < 2015,],sum)

#dev.off()
barplot(graf1$qtd_casos, main="Rio de Janeiro - Incidência anual de casos de dengue",
        xlab="Ano",ylab="Nº casos", names.arg=graf1$caso_ano,border="black", col="darkblue")

graf1


############################## GRAFICO 02 ##############################
graf2 <- aggregate(qtd_casos ~ caso_ano + caso_mes, data=casos[casos$caso_ano < 2015,],sum)

#dev.off()
par(cex=.8)
plot( graf2[graf2$caso_ano == 2002,c(2,3)],type="l",col=1,
      xlim=c(1,12),ylim=c(0,50000), xaxt = "n",xlab="Mês",ylab="Nº casos",
      main="Rio de Janeiro - Curva mensal por ano de casos de dengue")
lines(graf2[graf2$caso_ano == 2003,c(2,3)],type="l",col=2)
lines(graf2[graf2$caso_ano == 2004,c(2,3)],type="l",col=3)
lines(graf2[graf2$caso_ano == 2005,c(2,3)],type="l",col=4)
lines(graf2[graf2$caso_ano == 2006,c(2,3)],type="l",col=5)
lines(graf2[graf2$caso_ano == 2007,c(2,3)],type="l",col=6)
lines(graf2[graf2$caso_ano == 2008,c(2,3)],type="l",col=7)
lines(graf2[graf2$caso_ano == 2009,c(2,3)],type="l",col=8)
lines(graf2[graf2$caso_ano == 2010,c(2,3)],type="l",col=9)
lines(graf2[graf2$caso_ano == 2011,c(2,3)],type="l",col=10)
lines(graf2[graf2$caso_ano == 2012,c(2,3)],type="l",col=11)
lines(graf2[graf2$caso_ano == 2013,c(2,3)],type="l",col=12)
lines(graf2[graf2$caso_ano == 2014,c(2,3)],type="l",col=13)
legend("topright", legend = 2002:2014, col=1:13, pch=22) 
axis(side = 1, at=1:12 ,labels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"))

############################## GRAFICO 03 ##############################
dfgraf3 <- casos[,c("ciclo_mes","ciclo","qtd_casos")]
dfgraf3$ciclo <- as.integer(substr(dfgraf3$ciclo,6,9))
dfgraf3 <- subset(dfgraf3,ciclo %in% 2002:2014,select=c(ciclo,ciclo_mes,qtd_casos))
graf3 <- aggregate(qtd_casos ~ ciclo_mes + ciclo, data=dfgraf3,sum)
graf3 <- graf3[order(graf3$ciclo,graf3$ciclo_mes),] 
#dev.off()
par(cex=.8)
plot( graf3[graf3$ciclo == 2002,c(1,3)],type="l",col=1,
      xlim=c(1,12),ylim=c(0,50000), xaxt = "n",xlab="Ciclo_Mês - Mês",ylab="Nº casos",
      main="Rio de Janeiro - Curva mensal por ano de casos de dengue")
lines(graf3[graf3$ciclo == 2003,c(1,3)],type="l",col=2)
lines(graf3[graf3$ciclo == 2004,c(1,3)],type="l",col=3)
lines(graf3[graf3$ciclo == 2005,c(1,3)],type="l",col=4)
lines(graf3[graf3$ciclo == 2006,c(1,3)],type="l",col=5)
lines(graf3[graf3$ciclo == 2007,c(1,3)],type="l",col=6)
lines(graf3[graf3$ciclo == 2008,c(1,3)],type="l",col=7)
lines(graf3[graf3$ciclo == 2009,c(1,3)],type="l",col=8)
lines(graf3[graf3$ciclo == 2010,c(1,3)],type="l",col=9)
lines(graf3[graf3$ciclo == 2011,c(1,3)],type="l",col=10)
lines(graf3[graf3$ciclo == 2012,c(1,3)],type="l",col=11)
lines(graf3[graf3$ciclo == 2013,c(1,3)],type="l",col=12)
lines(graf3[graf3$ciclo == 2014,c(1,3)],type="l",col=13)
legend("topleft", legend = c("Ciclo 2002","Ciclo 2003","Ciclo 2004","Ciclo 2005","Ciclo 2006","Ciclo 2007",
                             "Ciclo 2008","Ciclo 2009","Ciclo 2010","Ciclo 2011","Ciclo 2012",
                             "Ciclo 2013","Ciclo 2014"), col=1:13, pch=22) 
axis(side = 1, at=1:12 ,labels = c("01-ago","02-set","03-out","04-nov","05-dez","06-jan","07-fev","08-mar","09-abr","10-mai","11-jun","12-jul"))



############################## GRAFICO 04 ##############################
graf4 <- aggregate(qtd_casos ~ caso_ano, data=casos[casos$caso_ano < 2015,],sum)
graf4 <- graf4[order(-graf4$qtd_casos),] 

#dev.off()
barplot(graf4$qtd_casos, main="Rio de Janeiro - Incidência anual de casos de dengue",
        xlab="Ano",ylab="Nº casos", names.arg=graf4$caso_ano,border="black", col=ifelse(graf4$caso_ano == 2013,'darkblue','gray'))

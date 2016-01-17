criamodelo <- function(train,feat_pred){

  set.seed(1413) 
  
  names(train)[names(train) == feat_pred] <- 'feat_pred'
  
  train[is.na(train)]   <- 0
  htam <- as.integer(0.015 * nrow(train))
  h <- sample(nrow(train),htam)
  
  train_int_sparse <- sparse.model.matrix(feat_pred~.-1, data = train[-h,])
  train_int_label = train[-h,c("feat_pred")]
  
  test_int_sparse <- sparse.model.matrix(feat_pred~.-1, data = train[h,])
  test_int_label = train[h,c("feat_pred")]
  
  dtrain <- xgb.DMatrix(data=train_int_sparse,label=train_int_label)
  dtest  <- xgb.DMatrix(data=test_int_sparse, label=test_int_label)
  
  watchlist <- list(int_test=dtest,int_train=dtrain)
  xgb <- xgb.train(params=list(objective="reg:linear",booster="gbtree"), data=dtrain, nrounds=2000, verbose=1, early.stop.round=100, watchlist=watchlist, maximize=FALSE)
  
  
  return(xgb)
  
}


criapredicao <- function(xgb,df){
  
  df_sparse <- sparse.model.matrix(qtd_casos~.-1, data = df)
  
  pred <- predict(xgb, df_sparse)
  pred <- as.data.frame(pred)
  pred <- data.frame(df,pred)
  pred$pred <- round(pred$pred, 0)
 
  
  return(pred)
  
}


avaliapredicao <- function(dfpred){
  
  pred_mes <- aggregate(cbind(dfpred$pred,dfpred$qtd_casos), by=list(dfpred$ciclo_mes), FUN=sum)
  names(pred_mes) <- c("ciclo_mes","pred","qtd_casos")
  
  corr_mes <- cor(pred_mes$qtd_casos,pred_mes$pred)
  rmse_mes <- sum(abs(pred_mes$pred - pred_mes$qtd_casos)) /  sum(pred_mes$qtd_casos)
  
  par(mfrow=c(1,2),cex=.8)
  
  title <- paste("RMSE=",round(rmse_mes*100,2),"%")
  g_range <- range(0,pred_mes$pred * 1.1, pred_mes$qtd_casos * 1.1)
  plot(pred_mes$pred, type="o", pch=22, lty=2, col="red", ylim=g_range,ann=FALSE,frame=FALSE )
  lines(pred_mes$qtd_casos, type="o", col="green")
  title(main=title, col.main="blue", font.main=1)
  title(xlab="Ciclo Mês")
  title(ylab="Qtd Casos")
  legend("topright", legend = c("Previsto","Real"), col=c("red","green"), pch=22) 
  
  title <- paste("Previsto=", format(sum(pred_mes$pred),big.mark=".",decimal.mark=",")," ", "Real:",format(sum(pred_mes$qtd_casos),big.mark=".",decimal.mark=","),"Erro:",round(abs((1 - sum(pred_mes$pred) / sum(pred_mes$qtd_casos))) * 100,2),"%")
  g_range <- range(0,sum(pred_mes$pred) * 1.1, sum(pred_mes$qtd_casos) * 1.1)
  ap <- rbind(sum(pred_mes$pred),sum(pred_mes$qtd_casos))
  barplot(ap,ylim=g_range,col=c("red","green"), beside = TRUE)
  title(main=title, col.main="blue", font.main=1)
    
}

criafeaturesderivadas <- function(casos,ciclo_teste,ciclo_alvo){
  
  lstciclos_ref <- unique(casos$ciclo_num)
  ref <- subset(casos,ciclo_parte=="referencia")
  
  # totultmesref: Soma dos casos no último mês do período de referência por bairro, de todos os ciclos
  ref_ult_ss <-  subset(ref,ciclo_mes == ciclo_mes_prev)
  ref_ult <- aggregate(qtd_casos ~ bairro + ciclo_num, data = ref_ult_ss, sum)
  names(ref_ult)[names(ref_ult) == 'qtd_casos'] <- 'totultmesref'
  casos <- merge(casos,ref_ult)
  
  # desviopadraoref: Desvio padrão da parte referência por ciclo
  ref_sd <- aggregate(qtd_casos ~ ciclo_num, data = ref, sd)
  names(ref_sd)[names(ref_sd) == 'qtd_casos'] <- 'desviopadraoref'
  casos <- merge(casos,ref_sd)
    
  # fatorpred_ultref: Razão da média entre cada mês de predição e o último mês de referencia, por bairro, de todos os ciclos
  # fatorpred_mesant: Razão da média entre cada mês de predição e o mês anterior a ele, por bairro, de todos os ciclos
  ref_all <- subset(casos,ciclo_num != ciclo_alvo | (ciclo_num == ciclo_alvo & ciclo_mes <= ciclo_mes_prev))
  ref_cross <- aggregate(qtd_casos ~ bairro + ciclo_mes, data = ref_all, mean)
  names(ref_cross)[names(ref_cross) == 'qtd_casos'] <- 'refmean'
  
  ref_bairros <- subset(ref_cross,ciclo_mes==5,select=c(bairro,refmean))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'reffator'
    
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==6,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred06'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==7,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred07'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==8,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred08'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==9,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred09'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==10,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred10'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==11,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred11'
  
  ref_bairros <- merge(ref_bairros,subset(ref_cross,ciclo_mes==12,select=c(bairro,refmean)))
  names(ref_bairros)[names(ref_bairros) == 'refmean'] <- 'pred12'
  
  ref_bairros$fator06 <- ref_bairros$pred06 / ref_bairros$reffator
  ref_bairros$fator07 <- ref_bairros$pred07 / ref_bairros$reffator
  ref_bairros$fator08 <- ref_bairros$pred08 / ref_bairros$reffator
  ref_bairros$fator09 <- ref_bairros$pred09 / ref_bairros$reffator
  ref_bairros$fator10 <- ref_bairros$pred10 / ref_bairros$reffator
  ref_bairros$fator11 <- ref_bairros$pred11 / ref_bairros$reffator
  ref_bairros$fator12 <- ref_bairros$pred12 / ref_bairros$reffator
  
  ref_bairros$previous06 <- ref_bairros$pred06 / ref_bairros$reffator
  ref_bairros$previous07 <- ref_bairros$pred07 / ref_bairros$pred06
  ref_bairros$previous08 <- ref_bairros$pred08 / ref_bairros$pred07
  ref_bairros$previous09 <- ref_bairros$pred09 / ref_bairros$pred08
  ref_bairros$previous10 <- ref_bairros$pred10 / ref_bairros$pred09
  ref_bairros$previous11 <- ref_bairros$pred11 / ref_bairros$pred10
  ref_bairros$previous12 <- ref_bairros$pred12 / ref_bairros$pred11
  
  ref_bairros[is.na(ref_bairros)] <- 0
  ref_bairros[mapply(is.infinite, ref_bairros)] <- 999
  
  ref_bairro_mes <- subset(ref_bairros,select=c(bairro,fator06))
  names(ref_bairro_mes)[names(ref_bairro_mes) == 'fator06'] <- 'fatorpred_ultref'
  ref_addp <- subset(ref_bairros,select=c(bairro,previous06))
  names(ref_addp)[names(ref_addp) == 'previous06'] <- 'fatorpred_mesant'
  ref_bairro_mes <- merge(ref_bairro_mes,ref_addp)
  ref_bairro_mes$ciclo_mes <- 6
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator07))
  names(ref_add)[names(ref_add) == 'fator07'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 7
  ref_addp <- subset(ref_bairros,select=c(bairro,previous07))
  names(ref_addp)[names(ref_addp) == 'previous07'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator08))
  names(ref_add)[names(ref_add) == 'fator08'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 8
  ref_addp <- subset(ref_bairros,select=c(bairro,previous08))
  names(ref_addp)[names(ref_addp) == 'previous08'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator09))
  names(ref_add)[names(ref_add) == 'fator09'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 9
  ref_addp <- subset(ref_bairros,select=c(bairro,previous09))
  names(ref_addp)[names(ref_addp) == 'previous09'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator10))
  names(ref_add)[names(ref_add) == 'fator10'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 10
  ref_addp <- subset(ref_bairros,select=c(bairro,previous10))
  names(ref_addp)[names(ref_addp) == 'previous10'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator11))
  names(ref_add)[names(ref_add) == 'fator11'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 11
  ref_addp <- subset(ref_bairros,select=c(bairro,previous11))
  names(ref_addp)[names(ref_addp) == 'previous11'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  ref_add <- subset(ref_bairros,select=c(bairro,fator12))
  names(ref_add)[names(ref_add) == 'fator12'] <- 'fatorpred_ultref'
  ref_add$ciclo_mes <- 12
  ref_addp <- subset(ref_bairros,select=c(bairro,previous12))
  names(ref_addp)[names(ref_addp) == 'previous12'] <- 'fatorpred_mesant'
  ref_add <- merge(ref_add,ref_addp)
  ref_bairro_mes <- rbind(ref_bairro_mes,ref_add)
  
  casos <- merge(casos,ref_bairro_mes, all = TRUE)
  casos$fatorpred_ultref <-  ifelse(is.na(casos$fatorpred_ultref),1,casos$fatorpred_ultref)
  casos$fatorpred_mesant <-  ifelse(is.na(casos$fatorpred_mesant),1,casos$fatorpred_mesant)
  
  # ciclomenorerro:  Total de casos de cada mes de predição do bairro que comparando o período de referência com o atual apresenta o menor erro absoluto
  tcref1 <- subset(casos,ciclo_parte == "referencia",select=c(ciclo_num,qtd_casos))
  tcref <-  aggregate(qtd_casos ~ ciclo_num, data=tcref1, sum)
  names(tcref)[names(tcref) == 'qtd_casos'] <- 'totref'
  tc1 <- subset(casos,ciclo_num != ciclo_alvo & ciclo_num != ciclo_teste,select=c(ciclo_num,qtd_casos))
  tc <-  aggregate(qtd_casos ~ ciclo_num, data=tc1, sum)
  names(tc)[names(tc) == 'qtd_casos'] <- 'ciclomenorerro'
  
  crefteste <- which(abs(tcref[tcref$ciclo_num != ciclo_teste,c("totref")]-tcref[tcref$ciclo_num == ciclo_teste,2])==min(abs(tcref[tcref$ciclo_num != ciclo_teste,c("totref")]-tcref[tcref$ciclo_num == ciclo_teste,2])))
  crefalvo <- which(abs(tcref[tcref$ciclo_num != ciclo_alvo,c("totref")]-tcref[tcref$ciclo_num == ciclo_alvo,2])==min(abs(tcref[tcref$ciclo_num != ciclo_alvo,c("totref")]-tcref[tcref$ciclo_num == ciclo_alvo,2])))
  tc <- rbind(tc,c(ciclo_teste,tc[crefteste,2]))
  tc <- rbind(tc,c(ciclo_alvo,tc[crefalvo,2]))
  casos <- merge(casos,tc)
  
  # ciclosimilar: Pesos de cada mês de predição do ciclo do bairro mais similar à parte de referência de atual, entre todos os bairros existentes
  similref <- subset(casos,ciclo_parte=="referencia",select=c(bairro,ciclo_num,ciclo_mes,qtd_casos))
  similpred <- subset(casos,ciclo_parte=="predicao",select=c(bairro,ciclo_num,ciclo_mes,qtd_casos))
  
  bairros <- unique(casos$bairro)
  
  similtab <- subset(similref,ciclo_mes == 1,select=c("bairro","ciclo_num","qtd_casos"))
  names(similtab)[names(similtab) == 'qtd_casos'] <- 'm1'
  similx <- subset(similref,ciclo_mes == 2,select=c("bairro","ciclo_num","qtd_casos"))
  names(similx)[names(similx) == 'qtd_casos'] <- 'm2'
  similtab <- merge(similtab,similx)
  similx <- subset(similref,ciclo_mes == 3,select=c("bairro","ciclo_num","qtd_casos"))
  names(similx)[names(similx) == 'qtd_casos'] <- 'm3'
  similtab <- merge(similtab,similx)
  similx <- subset(similref,ciclo_mes == 4,select=c("bairro","ciclo_num","qtd_casos"))
  names(similx)[names(similx) == 'qtd_casos'] <- 'm4'
  similtab <- merge(similtab,similx)
  similx <- subset(similref,ciclo_mes == 5,select=c("bairro","ciclo_num","qtd_casos"))
  names(similx)[names(similx) == 'qtd_casos'] <- 'm5'
  similtab <- merge(similtab,similx)
  
  res <- data.frame(ciclo_num= numeric(0), ciclo_mes= integer(0), bairro= numeric(0), qtd_casos= numeric(0))
  for (b in 1:length(bairros)) {
    
    objbairro <- bairros[b]
    
    for (c in 1:length(lstciclos_ref)) {
      
      objciclo <- lstciclos_ref[c]
      
      dfobj <- similtab[similtab$bairro == objbairro & similtab$ciclo_num == objciclo,] 
      dftab <- similtab[similtab$bairro != objbairro | similtab$ciclo_num != objciclo,] 
      dftab$rmse <- abs(dftab$m1 - dfobj[,c("m1")]) + abs(dftab$m2 - dfobj[,c("m2")]) + abs(dftab$m3 - dfobj[,c("m3")]) + abs(dftab$m4 -  dfobj[,c("m4")]) + abs(dftab$m5 - dfobj[,c("m5")]) 
      minrmse <- min(dftab$rmse) 
      dfresult <- dftab[dftab$rmse == minrmse,c("bairro","ciclo_num")] 
      dfresult <- merge(similpred,dfresult) 
      dfresult <- aggregate(qtd_casos ~ ciclo_mes, data = dfresult, mean) 
      dfresult$ciclo_num <- objciclo 
      dfresult$bairro <- objbairro 
      dfresult <- dfresult[,c("ciclo_num","ciclo_mes","bairro","qtd_casos")] 
      res <- rbind(res,dfresult)
      
    } 
  }
  
  names(res)[names(res) == 'qtd_casos'] <- 'ciclosimilar'
  casos <- merge(casos,res, all = TRUE)
  casos[is.na(casos$ciclosimilar),c("ciclosimilar")] <- casos[is.na(casos$ciclosimilar),c("qtd_casos")]
 
  casos <-  casos[c("ciclosimilar","totultmesref","desviopadraoref","fatorpred_ultref","fatorpred_mesant","ciclomenorerro",
                    "ciclo","bairro","caso_anomes","ciclo_mes","caso_ano","caso_mes",
                    "regiao","zona","area","area_grupo","bairro_ha","bairro_pop2010","ciclo_num","ciclo_parte","qtd_casos")]  
  
  return(casos)
  
}

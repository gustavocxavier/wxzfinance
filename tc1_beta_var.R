#TÍTULO: Investment Recommendation: Analyzing dynamic beta and value-at-risk (VaR) portfolios in R software
#AUTORES: Leandro Wickboldt, Polyandra Zampiere e Gustavo Xavier

# APÊNDICE – SCRIPS DO R
#### Instalando e carregando pacotes necessários ####
install.packages("tseries"); library(tseries)
install.packages("fGarch"); library(fGarch)
install.packages("fBasics"); library(fBasics)
install.packages("data.table"); library(data.table)
install.packages("dplyr"); library(dplyr)
install.packages("readxl"); library(readxl)

#### Coletando dados e analisando o desempenho individual (para Tabela 1) ####
# Coleta de dados do S&P500
SP500 <- get.hist.quote(instrument = "^GSPC",
                        start="2011-05-06", # Dia 06 de maio de 2011
                        end="2018-07-04",   # Dia 04 de julho de 2018
                        quote = c("Open", "High", "Low", "Close", "Adjusted"),
                        provider = "yahoo", compression = "d")

SP500_ret  <- (((SP500$Adjusted/lag(SP500$Adjusted))-1)*100) # Criando a série de retornos
SP500r <- SP500_ret[-1,"Return"] #Eliminando a primeira linha da séries de retornos
basicStats(SP500r) #estatísticas básicas da série
sd(SP500r)/mean(SP500r) #coeficiente de variação
max(SP500r)-min(SP500r) #variação

# Coleta de dados da Empresa 1 (AMZN): Amazon
amzn <- get.hist.quote(instrument = "AMZN",
                         start="2011-05-06", # Início 06 de maio de 2011
                         end="2018-07-04",   # Final 04 de julho de 2018 (Feriado)
                         quote = c("Open", "High", "Low", "Close", "Adjusted"),
                         provider = "yahoo", compression = "d",retclass = "zoo")

amzn_ret  <- (((amzn$Adjusted/lag(amzn$Adjusted))-1)*100) # Criando a série de retornos
amznr <- amzn_ret[-1,"Return"] #Eliminando a primeira linha da séries de retornos
basicStats(amznr) #estatísticas básicas da série
sd(amznr)/mean(amznr) #coeficiente de variação
max(amznr)-min(amznr) #variação

# Coleta de dados da Empresa 2 (PFE): Pfizer
pfe <- get.hist.quote(instrument = "PFE",
                       start="2011-05-06", # Início 06 de maio de 2011
                       end="2018-07-04",   # Final 04 de julho de 2018 (Feriado)
                       quote = c("Open", "High", "Low", "Close", "Adjusted"),
                       provider = "yahoo", compression = "d",retclass = "zoo")

pfe_ret  <- (((pfe$Adjusted/lag(pfe$Adjusted))-1)*100) # Criando a série de retornos
pfer <- pfe_ret[-1,"Return"] #Eliminando a primeira linha da séries de retornos
basicStats(pfer) #estatísticas básicas da série
sd(pfer)/mean(pfer) #coeficiente de variação
max(pfer)-min(pfer) #variação

# Coleta de dados da Empresa 3 (KO): Coca-Cola
ko <- get.hist.quote(instrument = "KO",
                      start="2011-05-06", # Início 06 de maio de 2011
                      end="2018-07-04",   # Final 04 de julho de 2018 (Feriado)
                      quote = c("Open", "High", "Low", "Close", "Adjusted"),
                      provider = "yahoo", compression = "d",retclass = "zoo")

ko_ret  <- (((ko$Adjusted/lag(ko$Adjusted))-1)*100) # Criando a série de retornos
kor <- ko_ret[-1,"Return"] #Eliminando a primeira linha da séries de retornos
basicStats(kor) #estatísticas básicas da série
sd(kor)/mean(kor) #coeficiente de variação
max(kor)-min(kor) #variação

# Comando para exportar as séries para Excel. Para criar o gráfico 2, você
# deverá acumular os retornos em 100 e inserir gráfico de linhas no Excel.
write.table(SP500r, "SP500.csv", quote=F, sep=";", dec = ",", row.names=T)
getwd() #verificando o diretório para onde o arquivo foi exportado

# Para o quadro 1, calcule os betas dos portfólios por média ponderada (equação 7)
# ponderada simples, a partir dos betas coletados no provedor Yahoo Finance
# Ex.: Empresa A (AG): 1,63 e Empresa B (IF): 0.91
(0.5*1.63)+(0.5*0.91) #Beta simulado do portfólio agressivo

#### Estimação e guarda dos desvios-padrão e betas dinâmicos (quadro 2) ####

get_beta <- function(Stock, Market, OOS=500) {
  # Função para computar desvios-padrão e beta dinâmicos
  #
  # Stock .... Retornos da ação
  # Market ... Retornos da carteira de mercado
  # OOS ...... Número de observações fora da amostra
  #
  
  require(fGarch)
  require(zoo)
  
  endT   = length(Market)   # Número total de observações
  startT = endT - OOS    # início da janela fora da amostra
  
  # Correlação dentro da amostra
  Ri <- head(Stock, startT)
  Rm <- head(Market, startT)
  
  corRiRm <- cor(Ri,Rm)
  
  fcst <- matrix(NA, nrow=OOS ,ncol=2)
  for (t in startT:endT-1) {
    # Previsão do desvio-padrão da ação
    Ri = ts(head(Stock,t))
    Ri_fit  <- garchFit(formula=~garch(1,1),data=Ri,trace=FALSE)
    Ri_fcst <- predict(Ri_fit,n.ahead=1)
    s_Ri    <- Ri_fcst[1,3]
    # Previsão do desvio-padrão do mercado
    Rm = ts(head(Market,t))
    Rm_fit  <- garchFit(formula=~garch(1,1),data=Rm,trace=FALSE)
    Rm_fcst <- predict(Rm_fit,n.ahead=1)
    s_Rm    <- Rm_fcst[1,3]
    # Desvio-padrão condicional
    fcst[t-startT+1,1] <- s_Ri
    # Beta dinâmico
    corRiRm <- cor(Ri,Rm)
    fcst[t-startT+1,2] <- corRiRm * s_Ri / s_Rm
    cat(".")
  }
  cat("\n")
  fcst <- as.data.frame(fcst)
  colnames(fcst) <- c("desviopadrao", "Beta")
  fcst$desviopadrao <- zoo(fcst$desviopadrao, order.by=tail(rownames(as.data.frame(Stock)), OOS))
  fcst$Beta     <- zoo(fcst$Beta    , order.by=tail(rownames(as.data.frame(Stock)), OOS))
  return(fcst)
  # return(Beta[,1])
}
#este procedimento dura alguns minutos....aguarde
bp_amzn <- get_beta(amznr, SP500r, OOS = 252)
bp_pfe <- get_beta(pfer, SP500r, OOS = 252)
bp_ko <- get_beta(kor, SP500r, OOS = 252)
bp_sp500 <-get_beta(SP500r, SP500r, OOS = 252)

#armazena os betas dinâmicos dos ativos e verifica se são próximos aos histórios
#mantendo ou alterando o ativo para compor as carteiras. O beta do S&P500 é 1.
#Para preencher o quadro 2
bp_Amazon <- mean(bp_amzn$Beta)
bp_Pfizer <- mean(bp_pfe$Beta)
bp_Coca <- mean(bp_ko$Beta)
bp_SP500 <-mean(bp_sp500$Beta)
# Ex.: Empresa A (AG): 1,3989 e Empresa B (IF): 0.9188
(0.5*bp_Amazon)+(0.5*bp_Pfizer) #Beta dinâmico do portfólio agressivo

#### Análise da relação retorno e risco das carteiras (para quadro 3) ####
agressiva <- as.data.frame(matrix(NA, nrow = 252, ncol = 2))
colnames(agressiva) <- c("p1", "p2")
agressiva$p1 <- as.numeric(0.5)
agressiva$p2 <- as.numeric(0.5)
agressiva$amzn <- as.numeric(tail(amznr, 252))
agressiva$pfe <- as.numeric(tail(pfer, 252))
agressiva$rp <- agressiva$p1 * agressiva$amzn + agressiva$p2 * agressiva$pfe
agressiva$bpamzn <- as.numeric(bp_amzn$Beta)
agressiva$bppfe <- as.numeric(bp_pfe$Beta)
agressiva$bport <- agressiva$p1 * agressiva$bpamzn + agressiva$p2 * agressiva$bppfe
agressiva$dppamzn <- as.numeric(bp_amzn$desviopadrao)
agressiva$dpppfe <- as.numeric(bp_pfe$desviopadrao)
agressiva$cov <-as.numeric(cov(agressiva$amzn,agressiva$pfe))
agressiva$dppp <-as.numeric(((agressiva$p1^2*agressiva$dppamzn^2)+(agressiva$p2^2*agressiva$dpppfe^2)+(2*agressiva$p1*agressiva$p2*agressiva$cov))^(1/2))
#mostrando retorno esperado rescente, beta, retorno da carteira e relação retorno por risco
mean(agressiva$bport);mean(agressiva$amzn);mean(agressiva$pfe);mean(agressiva$rp);(mean(agressiva$rp)/mean(agressiva$bport)) 

indexfund1 <- as.data.frame(matrix(NA, nrow = 252, ncol = 2))
colnames(indexfund1) <- c("p1", "p2")
indexfund1$p1 <- as.numeric(0.5)
indexfund1$p2 <- as.numeric(0.5)
indexfund1$amzn <- as.numeric(tail(amznr, 252))
indexfund1$ko <- as.numeric(tail(kor, 252))
indexfund1$rp <- indexfund1$p1 * indexfund1$amzn + indexfund1$p2 * indexfund1$ko
indexfund1$bpamzn <- as.numeric(bp_amzn$Beta)
indexfund1$bpko <- as.numeric(bp_ko$Beta)
indexfund1$bport <- indexfund1$p1 * indexfund1$bpamzn + indexfund1$p2 * indexfund1$bpko
indexfund1$dppamzn <- as.numeric(bp_amzn$desviopadrao)
indexfund1$dppko <- as.numeric(bp_ko$desviopadrao)
indexfund1$cov <-as.numeric(cov(indexfund1$amzn,indexfund1$ko))
indexfund1$dppp <-as.numeric(((indexfund1$p1^2*indexfund1$dppamzn^2)+(indexfund1$p2^2*indexfund1$dppko^2)+(2*indexfund1$p1*indexfund1$p2*indexfund1$cov))^(1/2))
#mostrando retorno esperado rescente, beta, retorno da carteira e relação retorno por risco
mean(indexfund1$bport);mean(indexfund1$amzn);mean(indexfund1$ko);mean(indexfund1$rp);(mean(indexfund1$rp)/mean(indexfund1$bport)) 

defensiva <- as.data.frame(matrix(NA, nrow = 252, ncol = 2))
colnames(defensiva) <- c("p1", "p2")
defensiva$p1 <- as.numeric(0.5)
defensiva$p2 <- as.numeric(0.5)
defensiva$pfe <- as.numeric(tail(pfer, 252))
defensiva$ko <- as.numeric(tail(kor, 252))
defensiva$rp <- defensiva$p1 * defensiva$pfe + defensiva$p2 * defensiva$ko
defensiva$bppfe <- as.numeric(bp_pfe$Beta)
defensiva$bpko <- as.numeric(bp_ko$Beta)
defensiva$bport <- defensiva$p1 * defensiva$bppfe + defensiva$p2 * defensiva$bpko
defensiva$dpppfe <- as.numeric(bp_pfe$desviopadrao)
defensiva$dppko <- as.numeric(bp_ko$desviopadrao)
defensiva$cov <-as.numeric(cov(defensiva$pfe,defensiva$ko))
defensiva$dppp <-as.numeric(((defensiva$p1^2*defensiva$dpppfe^2)+(defensiva$p2^2*defensiva$dppko^2)+(2*defensiva$p1*defensiva$p2*defensiva$cov))^(1/2))
#mostrando retorno esperado rescente, beta, retorno da carteira e relação retorno por risco
mean(defensiva$bport);mean(defensiva$pfe);mean(defensiva$ko);mean(defensiva$rp);(mean(defensiva$rp)/mean(defensiva$bport)) 

#guardando e mostrando retorno rescente do S&P500 252 dias (05.07.2017 a 04.07.2018)
SP500_252 <- as.numeric(tail(SP500r, 252))
mean(SP500_252)

#### Apurando VaR das Carteiras (p) (para quadro 4) ####

# VaR Condicional - Carteira Agressiva
VaR_agressiva <- agressiva$rp + agressiva$dppp * qnorm(0.01)
mean(VaR_agressiva)
# Perda máxima monetáRia - Carteira Agressiva
VaRdolar_agressiva         <- (mean(VaR_agressiva)         / 100) * 10000000
mean(VaRdolar_agressiva)

# VaR Condicional - Carteira Indexfund1
VaR_indexfund <- indexfund1$rp + indexfund1$dppp * qnorm(0.01)
mean(VaR_indexfund)
# Perda máxima monetáRia - Carteira Indexfund1
VaRdolar_indexfund         <- (mean(VaR_indexfund)         / 100) * 10000000
mean(VaRdolar_indexfund)

# VaR Condicional - Carteira Defensiva
VaR_defensiva <- defensiva$rp + defensiva$dppp * qnorm(0.01)
mean(VaR_defensiva)
# Perda máxima monetáRia - Carteira Defensiva
VaRdolar_defensiva         <- (mean(VaR_defensiva)         / 100) * 10000000
mean(VaRdolar_defensiva)

# VaR Condicional - SP500
VaR_SP500_252 <- SP500_252 + bp_sp500$desviopadrao * qnorm(0.01)
mean(VaR_SP500_252)
# Perda máxima monetáRia - SP500
VaRdolar_SP500_252       <- (mean(VaR_SP500_252)         / 100) * 10000000
mean(VaRdolar_SP500_252)

### Aula 3 Equações de diferença: Se B = 1 Tem raiz unitaria

### Dick-Fuller: H0: B1 = 1 HA: B1<1

library(urca)
library(readxl)
library(quantmod)
library(tidyverse)

BVSP<- na.omit(getSymbols("^BVSP", src ="yahoo",
                  from='2012-01-10', to =Sys.Date(), auto.assign = FALSE))

BVSP <- as.tibble(BVSP)
BVSP2 <- mutate(BVSP, Var_BVSP = (BVSP$BVSP.Close[-1] - BVSP$BVSP.Close)/ BVSP$BVSP.Close * 100)
BVSP3 <- cbind(BVSP2$BVSP.Close,BVSP2$Var_BVSP, BVSP$BVSP.Volume)
BVSP3 <- as.tibble(BVSP3)

BVSP3 <- BVSP3[-2047,]

colnames(BVSP3)[1] <- "BVSP"
colnames(BVSP3)[2] <- "variacao"
colnames(BVSP3)[3] <- "volume"


dados_diarios <- ts(BVSP3, start = c(2012,01,10), frequency = 365)
plot(dados_diarios, col="blue", main="Indice Ibovespa", xlab="Dias")

variacao <- ts(BVSP3$variacao, start = c(2012,01,10), frequency = 365)
Ibovespa <- ts(BVSP3$BVSP, start = c(2012,01,10), frequency = 365)
volume <- ts(BVSP3$volume, start = c(2012,01,10), frequency = 365)

plot(variacao, main="Percentual de variacao")
plot(Ibovespa, main="Indice do dia", col="red")
plot(volume, main="Indice do dia", xlab="Dias", col="blue")

TesteDF_variacao_none <- ur.df(variacao, "none", lags = 0)
summary(TesteDF_variacao_none)

TesteDF_variacao_drift <- ur.df(variacao, "drift", lags = 0)
summary(TesteDF_variacao_drift)

TesteDF_variacao_trend <- ur.df(variacao, "trend", lags = 0)
summary(TesteDF_variacao_trend)

col1_variacao <- c("", -2.58,"", -3.43, -3.43,"", -3.96,-3.96,-3.96)
col2_variacao <- c("", -41.58,"", -41.572, 0.133,"", -41.578,0.805,-0.853)
col3_variacao <- c("", "0.000","", "0.000", 0.894,"", "0.000",0.421,0.394)
col4_resultado <- c("", "Estacionária",
                    "", "Estacionária", "sem drift",
                    "","Estacionária", "Sem drift", "sem tendência")

resultado_variacao <- cbind(col1_variacao,col2_variacao,col3_variacao,col4_resultado)

colnames(resultado_variacao) <- c("T crítico",
                                  "Estatística T", "p-value",
                                  "Resultado")

rownames(resultado_variacao) <- c("SEM CONST E SEM TENDÊNCIA",
                                  "Yt-1",
                                  "COM CONSTANTE",
                                  "Yt-1", "Drift",
                                  "COM CONSTANTE E COM TENDÊNCIA",
                                  "Yt-1", "Drift", "Trend")

view(resultado_variacao)

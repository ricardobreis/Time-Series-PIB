################################################################################################
#
# ANÁLISE DE SÉRIES TEMPORAIS - MBA Business Analytics e Big Data
# Por: RICARDO REIS
#
# CASE - PIB
#
################################################################################################


# Carrega pacotes ---------------------------------------------------------

library(forecast)
rm(list = ls())


# carrega o arquivo com a série temporal do PIB ---------------------------

pib <- read.csv2("~/Downloads/ipeadata[10-10-2019-07-53].csv")
pib <- read.csv2("~/R-Projetos/PIBTimeSeries/pib.csv")
names(pib) <- c("periodo", "valor")
pib <- pib[301:356,]


# Converte a série do PIB no formato de série temporal --------------------

pib_ts <- ts(pib$valor, start=c(2015,1), end=c(2019,8), frequency = 12)

#Plota o gráfico da série temporal
plot(pib_ts, xlab="Tempo", ylab="PIB", bty="l")


# Separa as amostras em treinamento e teste -------------------------------

tam_amostra_teste <- 8

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(pib_ts) - tam_amostra_teste

#cria a série temporal de treinamento
treinamento_ts <- window(pib_ts, start=c(2015, 1), end=c(2015,tam_amostra_treinamento))

#cria a série temporal de teste
validacao_ts <- window(pib_ts, start=c(2015, tam_amostra_treinamento + 1), end=c(2019,tam_amostra_treinamento+tam_amostra_teste))

#plota o gráfico da série temporal de treinamento e teste
plot(treinamento_ts, xlab="Tempo", ylab="PIB", xaxt="n", xlim=c(2015, 2020), bty="l")

axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts, bty="l", col="red")


# Modelo de Tendência Linear ----------------------------------------------

#Estima o modelo de tendência linear
modelo_tendencia_linear <- tslm(treinamento_ts ~ trend)

#resumo do modelo
summary(modelo_tendencia_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_linear, test="LB")

#plot modelo com tendência
plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", bty="l")
lines(modelo_tendencia_linear$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_linear_proj <- forecast(modelo_tendencia_linear, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_tendencia_linear_proj, xlab="Tempo", ylab="Vendas", xaxt="n" , xlim=c(2015, 2020), bty="l", flty=2)

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts)
lines(modelo_tendencia_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_tendencia_linear_proj, validacao_ts)


# Modelo de Tendência Quadrática ------------------------------------------

#Estima o modelo de Tendência Quadrática
modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))

#resumo do modelo
summary(modelo_tendencia_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_poli$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduoss
Acf(modelo_tendencia_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_poli, test="LB")

#plot modelo com tendência
plot(treinamento_ts, xlab="Tempo", ylab="PIB", bty="l")
lines(modelo_tendencia_poli$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_tendencia_poli_proj, xlab="Tempo", ylab="PIB", xaxt="n", bty="l", flty=2,main="Forecast from Polynomial regression model")

axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts)
lines(modelo_tendencia_poli_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_tendencia_poli_proj, validacao_ts)



# Modelo de Tendência Exponencial -----------------------------------------

# OBS
# Melhorar Modelo
modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)

#resumo do modelo
summary(modelo_tendencia_exp)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_exp$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_exp$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_exp, test="LB")

#plot modelo com tendência
plot(treinamento_ts, xlab="Tempo", ylab="PIB", bty="l")
lines(modelo_tendencia_exp$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, h = tam_amostra_teste, level=0)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_tendencia_exp_proj, xlab="Tempo", ylab="PIB", xaxt="n", ylim=c(1000, 1000000), xlim=c(2015, 2020), bty="l", flty=2 ,main="Forecast from Exp regression model")
axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))
lines(validacao_ts)
lines(modelo_tendencia_exp_proj$fitted, lwd=2, col="blue")

#Verifica a acuracia do modelo
accuracy(modelo_tendencia_exp_proj, validacao_ts)



# Modelo Sazonal Linear ---------------------------------------------------

#Estima o modelo de sazonal linear
modelo_sazonalidade_linear <- tslm(treinamento_ts ~ season)

#resumo do modelo
summary(modelo_sazonalidade_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonalidade_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonalidade_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonalidade_linear, test="LB")

#plot modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="PIB", bty="l")
lines(modelo_sazonalidade_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o períoodo de validação
modelo_sazonalidade_linear_proj <- forecast(modelo_sazonalidade_linear, h = tam_amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_sazonalidade_linear_proj, xlab="Tempo", ylab="PIB", xaxt="n", xlim=c(2015, 2020), bty="l", flty=2, main="Forecast from Seasonal regression model")

axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts)
lines(modelo_sazonalidade_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_sazonalidade_linear_proj, validacao_ts)


# Modelo de Tendência Linear Com Sazonalidade -----------------------------

#Estima o modelo de tendência linear com sazonalidade
modelo_sazonalidade_tendencia_linear <- tslm(treinamento_ts ~ season + trend)

#resumo do modelo
summary(modelo_sazonalidade_tendencia_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonalidade_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonalidade_tendencia_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonalidade_tendencia_linear, test="LB")

#plot modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_sazonalidade_tendencia_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonalidade_tendencia_linear_proj <- forecast(modelo_sazonalidade_tendencia_linear, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_sazonalidade_tendencia_linear_proj, xlab="Tempo", ylab="Vendas", xaxt="n", xlim=c(2015, 2020), bty="l", flty=2, main="Forecast from Seasonal and Trend regression model")

axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts)
lines(modelo_sazonalidade_tendencia_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_sazonalidade_tendencia_linear_proj, validacao_ts)


# Modelo de Tendência Quadrática Com Sazonalidade -------------------------

#Estima o modelo de tendência quadrática
modelo_sazonal_tend_poli <- tslm(treinamento_ts ~ season + trend + I(trend^2))

#resumo do modelo
summary(modelo_sazonal_tend_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonal_tend_poli$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonal_tend_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonal_tend_poli, test="LB")

#plot modelo com sazonal_tend
plot(treinamento_ts, xlab="Tempo", ylab="PIB", bty="l")
lines(modelo_sazonal_tend_poli$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonal_tend_poli_proj <- forecast(modelo_sazonal_tend_poli, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_sazonal_tend_poli_proj, xlab="Tempo", ylab="PIB", xaxt="n", xlim=c(2015, 2020), bty="l", flty=2, main="Forecast from Seasonal & Tendencia regression model")

axis(1, at=seq(2015, 2020, 1), labels=format(seq(2015, 2020,1)))

lines(validacao_ts)
lines(modelo_sazonal_tend_poli_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_sazonal_tend_poli_proj, validacao_ts)


# Modelo de tendência aditiva e sazonalidade multiplicativa ---------------

#estima o modelo de suavização na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "AAM", restrict = FALSE)

#resumo modelo
summary(modelo_ses)

#projeta os próximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o gráfico da projecão
plot(modelo_ses_proj, ylab="PIB", xlab="Tempo", bty="l", xaxt="n", xlim=c(2015,2020), flty=2)

axis(1, at=seq(2015,2020, 1), labels=format(seq(2015,2020, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")


# Modelo ZZZ --------------------------------------------------------------

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ZZZ", restrict = FALSE, allow.multiplicative.trend = TRUE)

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylab="PIB", xlab="Tempo", bty="l", xaxt="n", xlim=c(2015,2020), flty=2)

axis(1, at=seq(2015,2020, 1), labels=format(seq(2015,2020, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

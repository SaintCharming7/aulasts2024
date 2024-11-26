# Aula 1
# RLS

# criando dados amostrais

# razão alunos p/ professores

rap <- c(15, 17, 19, 20, 22, 23.5, 25)
notas <- c(680, 640, 670, 660, 630, 660, 635)

# criando um gráfico de dispersão
plot(notas ~ rap, ylab="",pch=20)

# RLS teórica 
# y = a + b1xi+e

# ajustando a linha
abline(a = 713, b = -3)


# IID

# set seed
set.seed(123)

# gerando um vetor de datas
Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# vetor de empregabilidade
X <- c(5000, rep(NA, length(Date)-1))

# Gerando a série temporal com influências aleatórias
for (t in 2:length(Date)) {
  
  X[t] <- -50 + 0.98 * X[t-1] + rnorm(n = 1, sd = 200)
  
}

# Rresultados da plotagem
plot(x = Date, 
     y = X, 
     type = "l", 
     col = "purple", 
     ylab = "Trabalhadores", 
     xlab = "Ano",
     lwd=2)

# Aula 2
# Séries temporais
# Bibliotecas que vamos utilizar
#install.packages(quantmod)
library(quantmod)
#install.packages(fredr)
library(fredr)
#install.packages(ggplot2)
library(ggplot2)
#install.packages(dplyr)
library(dplyr)
#install.packages("WDI")
library(WDI)

## crescimento real do PIB EUA (trimestre) 
getSymbols('GDPC1',src='FRED')

# convertendo o PIB em vetor
gdp_data <- data.frame(date=index(GDPC1), GDP=coredata(GDPC1))
str(gdp_data)
# gráfico utilizando o ggplot
eua_real <- ggplot(gdp_data, aes(x=date, y=GDPC1)) +
  geom_line(color = "navyblue") +
  labs(title= "PIB Real EUA", x="Ano", y="em US$ Bi")+theme_minimal()
eua_real


pib_cres_eua <- WDI(country = "US", indicator = "NY.GDP.MKTP.KD.ZG", start = 1960, end = 2020)
str(pib_cres_eua)

# Filter data for the United States
pib_cres_eua <- subset(pib_cres_eua, country == "United States")

# Plot GDP growth time series
ggplot(pib_cres_eua, aes(x=year, y=NY.GDP.MKTP.KD.ZG)) +
  geom_line(color = "navyblue") +
  labs(title="Taxa de crescimento EUA", x="Ano", y=" Crescimento em %")+theme_minimal()

# Autocorrelação

library(tidyquant)
#library(quantmod) #biblio p/ baixar os dados do yahoo finance
library(xts)

getSymbols("^NYA", src = "yahoo", from = "2013-01-01", to = "2023-12-31") #baixando os dados da NYSE entre 2013-2023

#calculando os retornos
?dailyReturn
returns <- dailyReturn(NYA)
str(returns)
# convertendo em um data-frame
ret_df <- as.data.frame(returns)
ret_df <- data.frame(Date = index(returns), Returns = coredata(returns))
str(ret_df)
# Create the ggplot graph
ggplot(ret_df, aes(x = Date, y = daily.returns)) +
  geom_line(color = "royalblue") +
  labs(title = "Retornos Diários NYSE (2013-2023)",
       x = "",
       y = "Retornos")+theme_minimal()

acf(na.omit(ret_df), main = " Autocorrelação amostral NYSE")

# RBG

set.seed(123)
y = rnorm(250)

ts.plot(y,main="Processo RBG",xlab="time",
        ylab="y(t)", col="blue", lwd=2)
abline(h=0)

# PD

set.seed(123)
e = rnorm(250)
y.dt = 0.1*seq(1,250) + e
ts.plot(y.dt, lwd=2, col="blue", main="PD c/ Ruído")
abline(a=0, b=0.1)

#PA
# Simulação de 4 PAs diferentes começando em 0 (em um modelo AR)
set.seed(1)

PAs <- ts(replicate(n=4,
                    arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))
matplot(PAs, 
        type = "l", 
        col = c("royalblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "",
        xlab = "Tempo",
        ylab = "Valor")

# PA flutuação

set.seed(1)

PAsd <- ts(replicate(n = 4, 
                     arima.sim(model = list(order = c(0, 1, 0)), 
                               n = 100,
                               mean = -0.2)))
matplot(PAsd, 
        type = "l", 
        col = c("royalblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "",
        xlab = "Tempo",
        ylab = "Valor")

# plot U.S. unemployment rate & Japanese industrial production
plot(merge(as.zoo(USdes), as.zoo(JPIndProd)), 
     plot.type = "single", 
     col = c("royalblue", "darkred"),
     lwd = 2,
     xlab = "Data",
     ylab = "",
     main = "")
# add a legend
legend("topleft", 
       legend = c("USdes", "JPIndProd"),
       col = c("royalblue", "darkred"),
       lwd = c(2, 2))


# Aula 3/4
# as bibliotecas que vamos utilizar
library(quantmod) #Yahoo Finance/Yahoo Finanças
library(fredr) # BC americano st. Louis
library(ggplot2)
library(dplyr)
library(WDI) # Banco Mundial 
#install.packages(forecast)
library(forecast)
library(tseries)
library(zoo)
library(lmtest)



# Teste rudimentar ADF

data <- c(3, 4, 4, 5, 6, 7, 6, 6, 7, 8, 9, 12, 10)

plot(data, type='l')

# teste ADF

adf.test(data)

# Série temporal mais complexa

#importando dados da biblioteca FRED
# Taxa de câmbio USD vs Libra Esterlina (GB)

getSymbols('DEXUSUK', src = 'FRED')

# convertendo a taxa de câmbio em vetor
usd_ukp <- data.frame(date=index(DEXUSUK), GDP=coredata(DEXUSUK))
plot(usd_ukp, main = "", type = "l")
str(usd_ukp)

usd_ukp$DEXUSUK <- as.numeric(usd_ukp$DEXUSUK)
usd_ukp_diff <- diff(usd_ukp$DEXUSUK)
plot(usd_ukp_diff, main = "", type = "l")

# removendo as NAs
usd_ukp_diff_clean <- na.omit(usd_ukp_diff)
str(usd_ukp_diff_clean)
plot(usd_ukp_diff_clean, main = "", type = "l")

adf.test(usd_ukp_diff_clean)
str(usd_ukp_diff)

# convertendo o PIB em vetor

getSymbols('GDPC1',src='FRED')
gdp_data <- data.frame(date=index(GDPC1), GDP=coredata(GDPC1))
str(gdp_data)

# Convertendo a variavel "date" p/ um série temporal
gdp_ts <- ts(gdp_data$GDPC1, start = c(1947, 1), frequency = 4)

plot(gdp_data, type = "l")


# estimando o modelo
ar.ols(gdp_ts, 
       order.max = 1, 
       demean = F, 
       intercept = T)


ar_ols <- ar.ols(gdp_ts, order.max = 1, demean = FALSE, intercept = TRUE)
str(ar_ols)

gdp_ts <- data.frame(gdp_ts)
str(gdp_ts)

# construíndo as defasagens 
gdp_ts$lag_GDPC1 <- stats::lag(gdp_ts$gdp_ts)
head(gdp_ts)

# Construindo a formula
#criando as defasagens
formula <- gdp_ts ~ lag_GDPC1

# Criando o AR como função linear ou seja como regressão linear
ar_lm <- lm(formula, data = gdp_ts)

summary(ar_lm)

# Moving average (MA)
# MA nos último 4 trimestres
gdp_data$GDP_MA <- rollmean(gdp_data$GDP, k = 4, fill = NA, align = "right")
head(gdp_data)
str(gdp_data)

# gráfico GDP e MA
ggplot(gdp_data, aes(x = date)) +
  geom_line(aes(y = GDPC1, color = "PIB")) +   # Plot the original GDP
  geom_line(aes(y = GDP_MA, color = "MA 4-trimestres")) + # Plot the moving average
  labs(title = "", 
       x = "", 
       y = "PIB (Bilhões USD - 2012)",
       color = "") +
  theme_minimal()

# MA c/ o USD/GBP
#calculando uma MA c/ o período de 10 estagios
usd_ukp_diff_clean_MA <- rollmean(usd_ukp_diff_clean, k = 10, fill = NA, align = "right")

#combinando os dados originais e MA em um df
data_plot <- data.frame(
  index = seq_along(usd_ukp_diff_clean),   # Create an index for the x-axis
  usd_ukp_diff_clean = usd_ukp_diff_clean,
  usd_ukp_diff_clean_MA = usd_ukp_diff_clean_MA
)

# Gráfico entre os dados originais e a média móvel
ggplot(data_plot, aes(x = index)) +
  geom_line(aes(y = usd_ukp_diff_clean, color = "dados originais")) +  # Plot original data
  geom_line(aes(y = usd_ukp_diff_clean_MA, color = "MA c/ 10 pontos")) + # Plot moving average
  labs(title = "",
       x = "",
       y = "",
       color = "") +
  theme_minimal()


str(gdp_ts)

# ARMA/ARIMA

arma_model <- auto.arima(usd_ukp_diff_clean, max.p = 5, max.q = 5, stationary = TRUE)
summary(arma_model)

arma_model <- arima(usd_ukp_diff_clean, order = c(1, 0, 1))  # ARMA(1, 1)
summary(arma_model)

# Gráfico dos resíduos 
plot(residuals(arma_model), main = "Resíduos ARMA", type = "l")

# ACF dos Resíduos
acf(residuals(arma_model), main = "ACF dos Resíduos")

# Ljung-Box Test
Box.test(residuals(arma_model), lag = 20, type = "Ljung-Box")

forecast_values <- forecast(arma_model, h = 10)  # prorjeção p/ 20 pontos no futuro
plot(forecast_values)

arima_models <- arima(usd_ukp_diff_clean, order = c(2, 0, 2))  # ARMA(2, 2)
summary(arima_models)

# Aula 6

# as bibliotecas que vamos utilizar
library(quantmod)
library(fredr)
library(ggplot2)
library(dplyr)
library(WDI)
library(forecast) # previsão
library(tseries) #séries temporais
library(zoo)
library(lmtest)

# buscando uma série de passageiros aereos entre 1949-1960
data("AirPassengers")

# Identificando os meses
head(AirPassengers)

# Observando a sazonalidade da série
plot(AirPassengers, main="Passageiros por companhia aérea (1949-1960)", ylab="Número de passageiros", xlab="ano")

# Check basic statistics
summary(AirPassengers)

# Testando o modelo c/ os dados dos passageiros
#Primeiro vamos testar a estacionariedade
#P/ isso vamos utilizar o ADF

# Perform Augmented Dickey-Fuller test
adf.test(AirPassengers)

# Aplicando diferenciação regular (d=1)
diff_AirPassengers <- diff(AirPassengers, differences = 1)

# Gráfico da diferenciação
plot(diff_AirPassengers, main="Diferenciação sob dados companhia aérea", ylab="Diff", xlab="ano")

# ADF novamente
adf.test(diff_AirPassengers)


# 'plotando' o ACF e PACF para identificar os parametros p,d,q
par(mfrow = c(1, 2))
acf(diff_AirPassengers, main="ACF da diferenciação")
pacf(diff_AirPassengers, main="PACF da diferenciação")


# Ajustando o SARIMA automaticamente
sarima_model <- auto.arima(AirPassengers, seasonal = TRUE)

summary(sarima_model)

# Forecast the next 12 months (1 year ahead)
# Projetando os próximos 12 meses (1 ano na frente)
forecast_values <- forecast(sarima_model, h=12)
str(forecast_values)

# Gráfico
plot(forecast_values, main="SARIMA Projeção  AirPassengers")
?forecast


# Verificando se os resíduos apresentam RB
checkresiduals(sarima_model)

# Plot ACF of residuals to ensure no autocorrelation
acf(residuals(sarima_model), main="ACF dos resíduos")

# Ljung-Box Test
Box.test(residuals(sarima_model), lag = 2, type = "Ljung-Box")

#Aula 7

#Instalando as bibliotecas p/ hoje
library(quantmod)
library(tseries)
library(FinTS)

# Step 2: Get financial data from FRED for S&P 500 (SP500) and NASDAQ (NASDAQCOM)
# Note: FRED uses slightly different symbols for indices
getSymbols(c("SP500", "NASDAQCOM"), src = "FRED", from = "2020-01-01", to = Sys.Date())


# calculando os retornos diarios p/ S&P 500 e NASDAQ
sp500_returns <- dailyReturn(SP500)  # Daily returns for S&P 500
nasdaq_returns <- dailyReturn(NASDAQCOM)  # Daily returns for NASDAQ


# Visualizando os retornos para conferir a volatilidade
par(mfrow = c(1, 2))
plot(sp500_returns, main = "S&P 500 Retornos diários")
plot(nasdaq_returns, main = "NASDAQ Retornos diários")

# Testando o modelo ARCH
# Fazendo o test LM p/ os retornos
ArchTest(sp500_returns)  
ArchTest(nasdaq_returns) 


# testando a significância nos efeitos do ARCH
sp500_arch <- garch(sp500_returns, order = c(0, 1))  # ARCH(1) S&P 500
summary(sp500_arch)

nasdaq_arch <- garch(nasdaq_returns, order = c(0, 1))  # ARCH(1) NASDAQ
summary(nasdaq_arch)

# Volatilidade condicional (sigma t)
sp500_volatility <- ts(sp500_arch$fitted.values[, 1], start = c(2020, 1), frequency = 252)
nasdaq_volatility <- ts(nasdaq_arch$fitted.values[, 1], start = c(2020, 1), frequency = 252)
summary(sp500_volatility)
summary(nasdaq_volatility)

par(mfrow = c(1,2))
plot(sp500_volatility, main = "S&P 500 Volatilidade Condicional (ARCH)", ylab = "Volatilidade")
plot(nasdaq_volatility, main = "NASDAQ Conditional Volatility (ARCH)", ylab = "Volatilidade")

# Aula 8

# Instalando a biblios necessarias p/ nosso modelo GARCH!
#install.packages("quantmod")
#install.packages("rugarch")
#install.packages("tidyverse")

# Biblios
library(quantmod)
library(rugarch) # Biblio que roda o GARCH
library(tidyverse)
library(ggplot2)


# Buscando os dados do S&P 500
#getSymbols("SP500", src = "FRED")

# Buscando os dados dos títulos do tesouro EUA(10 anos)
getSymbols("GS10", src = "FRED")

getSymbols("GS5", src = "FRED")


head(GS10)
head(GS5)
# calculando os retornos

gs10_returns <- diff(log(GS10))[-1] 

gs5_returns <- diff(log(GS5))[-1]  

# calculando os retornos em log


gs10_returns <- diff(log(GS10))[-1] 
plot(gs10_returns)

gs5_returns <- diff(log(GS5))[-1]
plot(gs5_returns)

# Especificando o GARCH
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "norm")
summary(garch_spec)

# Ajustando o GARCH(1,1) p/ os títulos 5 anos
garch_fit_gs5 <- ugarchfit(spec = garch_spec, data = gs5_returns)
summary(garch_fit_gs5)
str(garch_fit_gs5)
# Extraíndo a vol. condicional (sigma) p/ os títulos 5 anos
vol_gs5 <- as.data.frame(sigma(garch_fit_gs5))
vol_gs5$Date <- index(GS10)[-1]

# Ajustando o GARCH(1,1) p/ os títulos 10 anos
garch_fit_gs10 <- ugarchfit(spec = garch_spec, data = gs10_returns)
summary(garch_fit_gs10)
# Extraíndo a vol. condicional (sigma) p/ os títulos 10 anos
vol_gs10 <- as.data.frame(sigma(garch_fit_gs10))
vol_gs10$Date <- index(GS10)[-1]
summary(vol_gs10)

# Combine the two volatility series into a long format for ggplot
volatility_data <- data.frame(Date = vol_gs5$Date, GS5 = vol_gs5[,1], GS10 = vol_gs10[,1]) %>%
  gather(key = "Series", value = "Volatility", -Date)
summary(volatility_data)

# Plot the conditional volatility for both series
ggplot(volatility_data, aes(x = Date, y = Volatility, color = Series)) +
  geom_line(size = 0.5) +
  labs(title = "Volatilidade Condicional (GARCH)",
       subtitle = "Titulos 5 anos (EUA) vs. Títulos 10 anos (EUA)",
       x = "Data",
       y = "Volatilidade") +
  theme_minimal() +
  scale_color_manual(values = c("GS5" = "deeppink", "GS10" = "blue"))

# Conferindo os resíduos
residuals_gs5 <- residuals(garch_fit_gs5, standardize = TRUE)
acf(residuals_gs5^2, main = "ACF dos resíduos ao quadrado títulos 5 anos")

residuals_gs10 <- residuals(garch_fit_gs10, standardize = TRUE)
acf(residuals_gs10^2, main = "ACF dos resíduos ao quadrado títulos 10 anos")


#Aula 9
#install.packages("vars")
library(vars) #biblio do modelo VAR
library(quantmod)
library(ggplot2)
library(tseries)

# Baixando os dados da inflação (CPI) e desemprego (UNRATE) pelo FRED
getSymbols(c("CPIAUCSL", "UNRATE"), src = "FRED")

# Convertendo a série temporal p/ o R
cpi <- CPIAUCSL["1980::2023"]
unemployment <- UNRATE["1980::2023"]

# Calculando a inflação <-> Inflation, c/ mudança percentual a.a.
inflation <- diff(log(cpi)) * 100
colnames(inflation) <- "Inflation"

# unindo inflação e desemprego em um df
data <- merge(inflation, unemployment)
colnames(data) <- c("Inflation", "Unemployment")

# Removendo valores perdidos
data <- na.omit(data)

# Visualizando a estrutura dos dados
head(data)

# Visualizando os dados

autoplot(data, facets = TRUE) + 
  ggtitle("Taxa de inflação e desemprego (EUA)") +
  theme_minimal()

# Teste de ADF p/ verificar estacionariedade
adf.test(data$Inflation)
adf.test(data$Unemployment)

# Diferenciado o desemprego p/ obter estacionariedade
data$diff_Unemployment <- diff(data$Unemployment)

# Removendo NAs
diff_unemployment_clean <- na.omit(data$diff_Unemployment)

# gráfico
autoplot(diff_unemployment_clean, facets = TRUE) +
  ggtitle("Taxa de desemprego (EUA)")+
  theme_minimal()

# Perform the ADF test again on the differenced Unemployment series
adf.test(diff_unemployment_clean)

# colocando as duas séries novamente em um df
data <- merge(inflation, diff_unemployment_clean)

# Removendo novamente as NAs
data_clean <- na.omit(data)

# VAR
# Selecionando as defasanges
lag_selection <- VARselect(data_clean, lag.max = 10, type = "const")

print(lag_selection)

# ajustando o VAR c/ duas defasagens
var_model <- VAR(data_clean, p = lag_selection$selection["AIC(n)"], type = "const")

summary(var_model)

#projeções utilizando o VAR

# Projeções p/ os próximos 12 meses
var_forecast <- predict(var_model, n.ahead = 12)
str(var_forecast)
# gráfico das projeções
fanchart(var_forecast, names = "Inflação", main = "Projeção Inflação")
fanchart(var_forecast, names = "Desemprego", main = "Projeção Desemprego")

str(var_model)

# Corrected IRF computation
irf_model <- irf(var_model, impulse = "diff_Unemployment", response = "Inflation", boot = TRUE)
plot(irf_model)


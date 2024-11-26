# Aulas 10 - 18

# Aula 10/11

#install.packages(c("quantmod", "urca", "vars"))
library(quantmod)  # Dados do FRED
library(urca)      # P/ os testes de raíz unitárira e cointegração
library(vars)      # p/ os testes VAR e VECM
library(ggplot2)
library(reshape2)


# vamos trabalhar c/ os dados das taxas de juros americanas
getSymbols(c("GS10", "FEDFUNDS"), src = "FRED")

# View the data
head(GS10)
head(FEDFUNDS)

tbill10 <- GS10["1955::2020"]
ff <- FEDFUNDS["1955::2020"]

# Mtransformando as séries em uma matriz
interest_data <- merge(tbill10, ff)
colnames(interest_data) <- c("GS10", "FEDFUNDS")
str(interest_data)

# teste de ADF
adf_gs10 <- ur.df(interest_data$GS10, type = "drift", selectlags = "AIC")
summary(adf_gs10)

adf_fedfunds <- ur.df(interest_data$FEDFUNDS, type = "drift", selectlags = "AIC")
summary(adf_fedfunds)

# Diferenciando as séries

# Differencing the GS10 and FEDFUNDS series
diff_gs10 <- diff(interest_data$GS10)
diff_fedfunds <- diff(interest_data$FEDFUNDS)

# gráficos
autoplot(diff_gs10, facets = TRUE) +
  ggtitle("Tbill (EUA)")+
  theme_minimal()

autoplot(diff_fedfunds, facets = TRUE) +
  ggtitle("Juros (EUA)")+
  theme_minimal()


# Removendo as NAs
diff_gs10 <- na.omit(diff_gs10)
diff_fedfunds <- na.omit(diff_fedfunds)

#testes de ADF
adf_diff_gs10 <- ur.df(diff_gs10, type = "drift", selectlags = "AIC")
summary(adf_diff_gs10)

adf_diff_fedfunds <- ur.df(diff_fedfunds, type = "drift", selectlags = "AIC")
summary(adf_diff_fedfunds)

# Johansen Cointegration Test
johansen_test <- ca.jo(interest_data, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)

# Utilizando o VECM p/ verificar a cointegração
vecm_model <- cajorls(johansen_test, r = 1)  # Assumindo cointegração
summary(vecm_model)

# o VECM confere o equilíbrio a longo prazo
str(interest_data)

# Plotando
# Convertendo em um df
interest_data_df <- as.data.frame(interest_data)
interest_data_df$Date <- index(interest_data)


# plotando os dados em um formato amplo
interest_data_long <- melt(interest_data_df, id.vars = "Date", variable.name = "Series", value.name = "Value")

# gráfico
ggplot(interest_data_long, aes(x = Date, y = Value, color = Series)) +
  geom_line(size = 1) +
  labs(title = "GS10 vs FEDFUNDS",
       x = "",
       y = "Taxa de Juros",
       color = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom")

# aula 12

# Install required packages
#install.packages("quantmod")
#install.packages("strucchange")

# Load libraries
library(quantmod)
library(strucchange)#biblioteca para quebras estruturais
library(ggplot2)
library(lmtest)

# Baixando dados do PIB EUA
getSymbols("GDP", src = "FRED")


# convertendo em uma série temporal
gdp_ts <- ts(GDP, start = c(1947, 1), frequency = 4)  # PIB trimestral em 1947

# Step 3: Plot the data to visualize potential breaks
plot(gdp_ts, main="U.S. PIB (1947-Presente)", ylab="PIB (Bilhões)", col="blue")


# Aplicanado quebras estruturais
# Usando a função breakpoints() para mudanças de estrutura
breakpoints_gdp <- breakpoints(gdp_ts ~ 1)

# Visualizando quebras
plot(breakpoints_gdp, main = "Quebras Estruturais PIB Americano (1947-Presente)")

summary(breakpoints_gdp)

# utilizando o QLR

gdp_lm <- lm(GDP ~ 1)  # regressão linear simples
quand_test <- sctest(gdp_lm, type = "Quandt-Andrews")
quand_test
summary(quand_test)

# Aula 13

# Biblioteca
library(gmm) # Biblioeca do GMM

# Vamos criar dados aleatorios
set.seed(123)  

# Gerando um processo AR(1)
n <- 500  # no. Obs
alpha <- 0.5  # Intercepto
beta <- 0.8  # AR(1) coeficiente
epsilon <- rnorm(n)  # Epsilon dist. normalmente

# Criando uma série temporal
y <- numeric(n)
y[1] <- rnorm(1)  # valor inicial

# Simulando um processo AR(1)
for (t in 2:n) {
  y[t] <- alpha + beta * y[t - 1] + epsilon[t]
}

# Convertendo uma df
data_df <- data.frame(y = y)

# embaralhando os dados
shuffled_indices <- sample(1:n)  #
data_df <- data.frame(y = y[shuffled_indices])  # re-ordenando Y


str(data_df)  

# Definindo o AR(1) Condição do momento
ar1_moments <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  
  # Criando Vas defasadas
  y_lag <- data$y[1:(nrow(data) - 1)]
  y_current <- data$y[2:nrow(data)]
  
  # Resíduos do processo AR(1)
  residuals <- y_current - alpha - beta * y_lag
  
  # Retorno dos momentos de condiçõees usando Yt-1 como instrumentos
  return(cbind(residuals * y_lag))
}

# GMM Estimatimação
# parametros alpha e beta
initial_params <- c(0.1, 0.5)

#Preparando dados para o GMM
gmm_data <- data_df[2:nrow(data_df), , drop=FALSE]  


str(gmm_data)

# Estimando o GMM
gmm_result <- gmm(ar1_moments, x = gmm_data, t0 = initial_params)

# Resultados
summary(gmm_result)

# Aula 14

# Variáveis Instrumentais (IVs)
# install.packages("AER)
library(AER) # Pacote c/ os dados em demanda por cigarros
data("CigarettesSW") # base de dados de cigarros

head(CigarettesSW)

# Normalmente, desejamos modelar a demanda por cigarros (quantidades de cigarros consumidos) como uma função do preço, 
# mas como o preço é endógeno, usaremos impostos (taxs) como um instrumento para o preço (price).

# calcular preços per capita reais
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

# calcular o imposto sobre vendas
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# verificar a correlação entre imposto sobre vendas e preço
cor(CigarettesSW$salestax, CigarettesSW$price)
#> [1] 0.6141228

# gerar um subconjunto para o ano de 1995
c1995 <- subset(CigarettesSW, year == "1995")

# executar a regressão do primeiro estágio
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)

coeftest(cig_s1, vcov = vcovHC, type = "HC1")

# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared
#> [1] 0.4709961

# armazenando os valores previstos
lcigp_pred <- cig_s1$fitted.values

# execute a regressão de 2 estágios
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)

# executar regressão em dois estágios usando 'ivreg()'
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")


# teste de verificação para a est.F de primeiro estágio
summary(cig_ivreg, diagnostics = TRUE)

# Aula 16 + 17 + 18
# Projeção usando a "rolagem" da janela

# Utilizando nossas bibliotecas de hoje
library(quantmod)     # para buscar dados do FRED
library(zoo)          # para as operações de deslize
library(dplyr)
library(forecast)     # para o teste DM
library(tseries)      # para análise de séries temporais

# Download de dados do PIB EUA
getSymbols("GDP", src = "FRED")

# Convertendo em DF para manuseio mais facilitado
gdp_data <- data.frame(date = index(GDP), GDP = coredata(GDP))


# Especificando a janela para 20 trimestres ou 5 anos.
window_size <- 20


# Definindo as variáveis independentes e dependentes
# Vamos regredir PIB ou GDP na sua própria defasagem
gdp_data <- gdp_data %>%
  mutate(GDP_Lag1 = lag(GDP, 1))

# Removendo NAs
gdp_data <- na.omit(gdp_data)

# Definindo uma função para executar a regressão linear e retornar os coeficientes
rolling_regression <- function(df) {
  df <- as.data.frame(df)  
  model <- lm(GDP ~ GDP_Lag1, data = df)
  return(coef(model))
}

# fazendo a regressão deslizante usando rollapply
rolling_coefs <- rollapply(data = gdp_data[, c("GDP", "GDP_Lag1")], 
                           width = window_size, 
                           FUN = rolling_regression, 
                           by.column = FALSE, 
                           align = "right")

# Converta coeficientes de rolagem em um quadro de dados para facilitar a manipulação
rolling_coefs_df <- data.frame(date = gdp_data$date[(window_size):nrow(gdp_data)], 
                               intercept = rolling_coefs[, 1], 
                               slope = rolling_coefs[, 2])


head(rolling_coefs_df)

# Plotando os coeficientes para visualização
library(ggplot2)

ggplot(rolling_coefs_df, aes(x = date)) +
  geom_line(aes(y = slope, color = "Inclinação")) +
  geom_line(aes(y = intercept, color = "Intercepto")) +
  labs(title = "Coeficientes da Regressão da Janela Deslizante", y = "Valor do Coeficiente", x = "") +
  theme_minimal() +
  scale_color_manual(values = c("blue4", "pink4"))


# Forecast Diagnostic Tests

# Preparando previsões fora da amostra
# Definindo o horizonte e criando a rolagem

forecast_horizon <- 4
actual_values <- gdp_data$GDP[(window_size + forecast_horizon):nrow(gdp_data)]
forecast_values <- numeric(length(actual_values))

for (i in 1:length(actual_values)) {
  train_data <- gdp_data[1:(window_size + i - 1), ]
  model <- lm(GDP ~ GDP_Lag1, data = train_data)
  forecast_values[i] <- predict(model, newdata = data.frame(GDP_Lag1 = train_data$GDP[nrow(train_data)]))
}

# computando os erros de previsão
forecast_errors <- actual_values - forecast_values

# Teste Diebold-Mariano (DM)
# Assumindo que forecast2 é outro método de previsão com o qual você deseja comparar forecast_values
# forecast_errors_2 <- actual_values - forecast_values_2

# Using a placeholder second forecast
set.seed(123)
forecast_errors_2 <- forecast_errors + rnorm(length(forecast_errors), mean = 0, sd = 0.1)
dm_test_result <- dm.test(forecast_errors, forecast_errors_2, alternative = "two.sided")
print(dm_test_result)

# West Test (1996) p/ erros autocorrelacionados
# West (1996) Test Função
WestTest <- function(errors1, errors2, k) {
  # Diff nos erros de previsão
  diff_errors <- errors1 - errors2
  n <- length(diff_errors)
  
  # diferença da média
  mean_diff <- mean(diff_errors)
  
  # Ajuste de variância para erros de previsão sobrepostos
  var_adj <- 0
  for (i in 1:k) {
    cov_term <- cov(diff_errors[1:(n - i)], diff_errors[(i + 1):n])
    var_adj <- var_adj + 2 * (1 - i / (k + 1)) * cov_term
  }
  adjusted_var <- var(diff_errors) + var_adj / n
  
  # Est. de teste
  west_stat <- mean_diff / sqrt(adjusted_var / n)
  p_value <- 2 * (1 - pnorm(abs(west_stat)))  # two-sided p-value
  
  return(list(statistic = west_stat, p_value = p_value))
}

# aplicando o teste de west
west_test_result <- WestTest(forecast_errors, forecast_errors_2, k = 1)
print(west_test_result)


# Giacomini-White (GW) Teste p/ predição condicional
library(sandwich)
library(lmtest)

# Corte `gdp_data` para corresponder ao comprimento de `forecast_errors`
gdp_data_trimmed <- tail(gdp_data, n = length(forecast_errors))


print(nrow(gdp_data_trimmed))  
print(length(forecast_errors))  

# Combine variáveis em um único quadro de dados
gw_data <- data.frame(
  forecast_errors_diff = forecast_errors - forecast_errors_2,
  GDP_Lag1 = gdp_data_trimmed$GDP_Lag1
)

# teste de GW
gw_model <- lm(forecast_errors_diff ~ GDP_Lag1, data = gw_data)
gw_test_result <- coeftest(gw_model, vcov = NeweyWest(gw_model))

print(gw_test_result)

#baixando a taxa de desemprego
getSymbols("GDP", src = "FRED")
getSymbols("UNRATE", src = "FRED")

# convertendo em df
gdp_data <- data.frame(date = index(GDP), GDP = coredata(GDP))
unrate_data <- data.frame(date = index(UNRATE), UNRATE = coredata(UNRATE))

# colocando ambas em um df
merged_data <- merge(gdp_data, unrate_data, by = "date")

# Criação de valores defasados para o PIB e a taxa de desemprego
window_size <- 20
merged_data <- merged_data %>%
  mutate(GDP_Lag1 = lag(GDP, 1),
         UNRATE_Lag1 = lag(UNRATE, 1)) %>%
  na.omit()  # Remove NAs created by lagging

# regressão de rolagem para GDP e desemprego
rolling_regression <- function(df) {
  df <- as.data.frame(df)
  model <- lm(GDP ~ GDP_Lag1 + UNRATE_Lag1, data = df)
  return(coef(model))
}

# Applicando a regressão
rolling_coefs <- rollapply(
  data = merged_data[, c("GDP", "GDP_Lag1", "UNRATE_Lag1")],
  width = window_size,
  FUN = rolling_regression,
  by.column = FALSE,
  align = "right"
)

# convertendo os coeficientes em df
rolling_coefs_df <- data.frame(
  date = merged_data$date[window_size:nrow(merged_data)],
  intercept = rolling_coefs[, 1],
  GDP_slope = rolling_coefs[, 2],
  UNRATE_slope = rolling_coefs[, 3]
)

# Plot Rolling Coefficients
ggplot(rolling_coefs_df, aes(x = date)) +
  geom_line(aes(y = GDP_slope, color = "Inclinação")) +
  geom_line(aes(y = UNRATE_slope, color = "Inclinação Desemprego")) +
  geom_line(aes(y = intercept, color = "Intercepto PIB")) +
  labs(title = "Regressão de rolagem dos Coeficientes", y = "Valor do Coeficiente", x = "") +
  theme_minimal() +
  scale_color_manual(values = c("black", "deeppink", "turquoise4"))

# Previsão com PIB e modelo UNRATE
forecast_horizon <- 4
actual_values <- merged_data$GDP[(window_size + forecast_horizon):nrow(merged_data)]
forecast_values <- numeric(length(actual_values))

for (i in 1:length(actual_values)) {
  train_data <- merged_data[1:(window_size + i - 1), ]
  model <- lm(GDP ~ GDP_Lag1 + UNRATE_Lag1, data = train_data)
  forecast_values[i] <- predict(model, newdata = data.frame(
    GDP_Lag1 = train_data$GDP[nrow(train_data)],
    UNRATE_Lag1 = train_data$UNRATE[nrow(train_data)]
  ))
}

# Calcular erros de previsão
forecast_errors <- actual_values - forecast_values

# Previsão de comparação: PIB apenas (como um espaço reservado)
forecast_values_gdp_only <- numeric(length(actual_values))
for (i in 1:length(actual_values)) {
  train_data <- merged_data[1:(window_size + i - 1), ]
  model_gdp <- lm(GDP ~ GDP_Lag1, data = train_data)
  forecast_values_gdp_only[i] <- predict(model_gdp, newdata = data.frame(
    GDP_Lag1 = train_data$GDP[nrow(train_data)]
  ))
}

# Calcular erros para o modelo somente do PIB
forecast_errors_gdp_only <- actual_values - forecast_values_gdp_only

# Diebold-Mariano (DM) Teste
dm_test_result <- dm.test(forecast_errors, forecast_errors_gdp_only, alternative = "two.sided")
print(dm_test_result)

# West Test for Autocorrelated Errors
WestTest <- function(errors1, errors2, k) {
  diff_errors <- errors1 - errors2
  n <- length(diff_errors)
  mean_diff <- mean(diff_errors)
  var_adj <- 0
  for (i in 1:k) {
    cov_term <- cov(diff_errors[1:(n - i)], diff_errors[(i + 1):n])
    var_adj <- var_adj + 2 * (1 - i / (k + 1)) * cov_term
  }
  adjusted_var <- var(diff_errors) + var_adj / n
  west_stat <- mean_diff / sqrt(adjusted_var / n)
  p_value <- 2 * (1 - pnorm(abs(west_stat)))
  
  return(list(statistic = west_stat, p_value = p_value))
}

west_test_result <- WestTest(forecast_errors, forecast_errors_gdp_only, k = 1)
print(west_test_result)

# Giacomini-White (GW) Test
merged_data_trimmed <- tail(merged_data, n = length(forecast_errors))
gw_data <- data.frame(
  forecast_errors_diff = forecast_errors - forecast_errors_gdp_only,
  GDP_Lag1 = merged_data_trimmed$GDP_Lag1,
  UNRATE_Lag1 = merged_data_trimmed$UNRATE_Lag1
)

gw_model <- lm(forecast_errors_diff ~ GDP_Lag1 + UNRATE_Lag1, data = gw_data)
gw_test_result <- coeftest(gw_model, vcov = NeweyWest(gw_model))
print(gw_test_result)


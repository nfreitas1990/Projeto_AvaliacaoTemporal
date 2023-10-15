

source("scripts/03_decomposicao.R")

# Pacotes -----------------------------------------------------------------
library(tidymodels)
library(modeltime.resample)
library(tsfeatures)
library(dplyr)
library(ggplot2)

# Dados -------------------------------------------------------------------

# Definir  índice de intervenção --------------------------
# Anos_mes: tempo
# time: Criar indicador do tempo (continuo)
# BeforeAfter: Antes (0) e Depois (1) do impacto
# TimeSince: Var. contínua desde o início da intervenção antes (0) depois (1:n())
#            por estação
mensal_pre <- 
  turbidez_mensal_ts_inputNAs |> 
  filter(Ano_mes <= as.Date("2015-10-01")) |> 
  mutate(BeforeAfter = 0,
         TimeSince = 0)
mensal_pos <- 
  turbidez_mensal_ts_inputNAs |> 
  filter(Ano_mes > as.Date("2015-10-01")) |> 
  mutate(BeforeAfter = 1) |> 
  #arrange(c(Estacao,Ano_mes)) |> 
  group_by(Estacao) |> 
  mutate(TimeSince = seq(from = 1, to = n(), by = 1))

# Junção com os índices
dados_mensais <- bind_rows(mensal_pre, mensal_pos)

# Indicador de tempo "time" por estação
dados_mensais <- 
  dados_mensais |> 
  group_by(Estacao) |> 
  arrange(Ano_mes) |> 
  group_by(Estacao) |> 
  mutate(Time= seq(from = 1, to = n(), by = 1))

rd019 <- dados_mensais |> 
  filter(Estacao == "RD019")

rd019_pre <- mensal_pre |> 
  filter(Estacao == "RD019")

max(rd019_pre$Ano_mes)



# Transformar -------------------------------------------------------------
# Transformar em tsibble
dados_mensais.ts <- 
  dados_mensais |> 
  mutate(Data = as.Date(Ano_mes)) |>
  mutate(Mes = month(Data)) |> 
  arrange(Estacao,Data) |> 
  as_tibble(index = Ano_mes,
            key = Estacao)


rd019.ts <- dados_mensais.ts |> 
  filter(Estacao == "RD019")

# Grafico -----------------------------------------------------------------
rd019.ts |> 
  gg_tsdisplay(Turbidez, plot_type = "partial")



# Vetor dos Indicadores
step <- rd019$BeforeAfter
ramp <- rd019$TimeSince

step_pre <- rd019_pre$BeforeAfter
ramp_pre <- rd019_pre$TimeSince

# 2. ARIMA: Escolher os parâmetros -----------------------------------------------
# Converter em ts
dados.ts <- ts(rd019[,"Turbidez"], frequency = 12, start = min(rd019$Ano_mes))

dados.ts_pre <- ts(rd019_pre[,"Turbidez"], frequency = 12, start = min(rd019_pre$Ano_mes))


# indicadores:



# Automatizar a escolha do modelo ARIMA
model_autoarima <- auto.arima(dados.ts,
                     seasonal = TRUE,
                     xreg = cbind(ramp,step),
                     stepwise = FALSE,
                     trace = TRUE)

# Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors # Todos os dados

model_autoarima_pre <- auto.arima(dados.ts_pre,
                              seasonal = TRUE,
                              #xreg = cbind(ramp,step),
                              stepwise = FALSE,
                              trace = TRUE)

# Best model: ARIMA(3,0,0)(1,0,1)[12] with non-zero mean 



# 3. Diagnostico -------------------------------------------------------------

# Check residuals
checkresiduals(model_autoarima)
Box.test(model_autoarima$residuals, lag = 24, type = "Ljung-Box")

# Check residuals
checkresiduals(model_autoarima_pre)
Box.test(model_autoarima_pre$residuals, lag = 24, type = "Ljung-Box")

# Transformar dados -------------------------------------------------------
# Transformar em tsibble
dados_mensais <- 
  dados_mensais |> 
  mutate(Data = as.Date(Ano_mes)) |>
  mutate(Mes = month(Data)) |> 
  arrange(Estacao,Data) |> 
  as_tibble(index = Ano_mes,
            key = Estacao)

rd019 <- dados_mensais |> 
  filter(Estacao == "RD019")

# Ver a série ----------------------------------------------------------
# Grafico
rd019 |> 
  ggplot(aes(x = Ano_mes, y= Turbidez)) +
  geom_line()

# 4. Separar Treino | Teste --------------------------------------------------

# Separar os dados Pós impacto:
# Todos os dados após o impacto serão usado para comparar com o que 
#        o modelo preve que vai acontecer.
rd019_pos <- rd019 |> 
  filter(Ano_mes > as.Date("2015-10-01"))

rd019_pre <- rd019 |> 
  filter(Ano_mes <= as.Date("2015-10-01")) |> 
  arrange(Ano_mes)


# Separar a base de treino e teste
split <-
  time_series_split(
    rd019_pre,
    Data,
    initial = "14 years", # usado para ajustar
    assess = "2 years")  # usado para testar
split


# Gráfico
split %>%                        # como ficou a divisão
  tk_time_series_cv_plan() %>%   # transforma o split em uma tabela
  plot_time_series_cv_plan(Data, Turbidez)  # plot da tabela


backtest <- 
  time_series_cv(
    training(split),
    Data,
    cumulative = FALSE,
    initial = "7 years",    # usado para ajustar
    assess = "2 years",     # usado para testar
    skip = "6 month",       # de 2 em 2 anos
    slice_limit = 10        # separar base para deixar 1 teste (1 para teste e 9 para treino)
    
  )

# Gráfico dos slices do Treino
plot_time_series_cv_plan(backtest, Data, Turbidez)



# Ajuste Modelo -----------------------------------------------------------

# Ajuste Modelo -----------------------------------------------------------

# xxxxxxxxx
# Modelo 1: naive com sazonalidade
# Especificação do Modelo Mais simples com Sazonalidade de 1 ano
model_snaive <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")
# Ajuste
fit_snaive <- model_naive %>%
  fit(Turbidez ~ Data, training(split))


# Modelo 2: Regressao com sinalização do impacto
# Especificar o modelo
model_reglinear <- parsnip::linear_reg() %>%
  set_engine("lm")
# Ajuste
fit_reglinear <- model_reglinear |>
  fit(Turbidez ~ Data + as.factor(Mes),
      training(split))

# Modelo 3: Auto Arima
model_autoarima <- modeltime::arima_reg() |>
  set_engine("auto_arima")
# Ajuste
fit_autoarima <- model_autoarima |>
  fit(Turbidez ~ Data, training(split))

# xxxxxxxxx
# Modelo 4: Arima escolhido
model_arima <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 0,
  non_seasonal_ma = 2,
  seasonal_ar = 0,
  seasonal_differences = 0,
  seasonal_ma = 2) |>
  set_engine("arima")

# Ajuste
fit_arima <- model_arima |>
  fit(Turbidez ~ Data, training(split))


# Tabela com Modelos ------------------------------------------------------
# Tabela de Modelos: modeltime_table ( )--------------------------------------
# Cria tabela para Comparar o desempenho dos modelos
models_tbl <- modeltime_table(
  fit_reglinear,
  fit_snaive,
  fit_autoarima,
  fit_arima)

calibration_tbl <-
  models_tbl |>
  modeltime_calibrate(new_data = testing(split))


forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = dados_pre)

plot_modeltime_forecast(forecasts)

calibration_tbl |>
  modeltime_accuracy() |>
  View()



fit_arima

forecasts <- fit_arima |>
  modeltime_forecast(
    new_data = rd019)
plot_modeltime_forecast(forecasts)

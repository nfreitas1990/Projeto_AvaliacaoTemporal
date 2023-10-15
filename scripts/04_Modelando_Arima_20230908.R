source("scripts/03_decomposicao.R")


# Objetivo ----------------------------------------------------------------



# Pacotes -----------------------------------------------------------------
library(dplyr)
library(tidymodels)
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

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Opção 1. AutoArima ------------------------------------------------------


# ||| Criar indicadores 
# ||| Dados Totais RD019
rd019 <- 
  dados_mensais |> 
  filter(Estacao== "RD019")

step <- as.vector(rd019$BeforeAfter)
ramp <- as.vector(rd019$TimeSince)

# ||| BoxCox: Transformar
lambda_BC <- car::powerTransform(rd019$Turbidez)
lambda_BC
rd019$bc_Turbidez <- (((rd019$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

# ||| Dados pré impacto
# ||| Não tem indicadores 
rd019_pre <- 
  mensal_pre |> 
  filter(Estacao== "RD019")

# rd019_pre <- 
#   rd019 |> 
#   filter(Ano_mes <= as.Date("2015-10-01"))

# ||| Converter em ts
# ||| Dados Totais
dados.ts <- ts(rd019[,"Turbidez"], frequency = 12, start = min(rd019$Ano_mes))
dados.ts

# ||| Converter em ts
# ||| Dados Pre
dados.ts_pre <- ts(rd019_pre[,"Turbidez"], frequency = 12, start = min(rd019_pre$Ano_mes))
dados.ts_pre

# ||| Converter em ts
# ||| Dados Totais com transformação BoxCox
dados.ts.bc <- ts(rd019[,"bc_Turbidez"], frequency = 12, start = min(rd019$Ano_mes))
dados.ts.bc

# ||| Converter em ts
# ||| Dados Totais com transformação BoxCox
dados.ts_pre_bc <- ts(rd019[,"bc_Turbidez"], frequency = 12, start = min(rd019_pre$Ano_mes))
dados.ts_pre_bc

# Auto.arima( ) ----------------------------------------------------------------
# Rodar o autoarima com todos os dados para encontrar os melhores parâmetros 
# para o modelo Arima


# ||| Dados Totais
# ||| Sem Transformação
mod_arima_totais <- auto.arima(dados.ts,
                         seasonal = TRUE,
                         xreg = cbind(step,ramp),
                         stepwise = FALSE,
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors


# ||| Dados Totais
# ||| Com Transformação
mod_arima_totais_bc <- auto.arima(dados.ts.bc,
                         seasonal = TRUE,
                         xreg = cbind(step,ramp),
                         stepwise = FALSE,
                         max.d = 1,
                         max.D = 1,
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 


# ||| Dados Pre
# ||| Sem Transformação
mod_arima_pre <- auto.arima(dados.ts_pre,
                               seasonal = TRUE,
                               #xreg = cbind(step,ramp),
                               stepwise = FALSE,
                               trace = TRUE)
# Best model: ARIMA(3,0,0)(1,0,1)[12] with non-zero mean 

# ||| Dados Pre
# ||| Com Transformação
mod_arima_pre_bc <- auto.arima(dados.ts_pre_bc,
                            seasonal = TRUE,
                            #xreg = cbind(step,ramp),
                            stepwise = FALSE,
                            trace = TRUE)
# Best model: ARIMA(2,0,0)(2,1,1)[12]
# Best model: ARIMA(2,0,0)(2,1,0)[12] 


#> Conclusão podemos testar quatro modelos Arima para modelar os dados
#> Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors  - dados totais
#> Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors  - dados totais boxcox
#> Best model: ARIMA(3,0,0)(1,0,1)[12] with non-zero mean - dados pre
#> Best model: ARIMA(2,0,0)(2,1,1)[12]                    - dados pre boxcox


# Diagnósticos ------------------------------------------------------------

# ||| Dados Totais
# ||| Sem Transformação
# Checar os resíduos - MODELO escala normal
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima_totais)
Box.test(mod_arima_totais$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.

# ||| Dados Totais
# ||| Com Transformação
# Checar os resíduos - MODELO Log
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima_totais_bc)
Box.test(mod_arima_totais_bc$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.

# ||| Dados Pre
# ||| Sem Transformação
# Checar os resíduos - MODELO escala normal
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima_pre)
Box.test(mod_arima_pre$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.

# ||| Dados Pre
# ||| Com Transformação
# Checar os resíduos - MODELO escala normal
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima_pre_bc)
Box.test(mod_arima_pre_bc$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.


# Resumo Modelos ----------------------------------------------------------

# ||| Dados Totais
# ||| Sem Transformação
#> Estimar Parâmetros
summary(mod_arima_totais)
confint(mod_arima_totais)

# ||| Dados Totais
# ||| Com Transformação
#> Estimar Parâmetros em log
summary(mod_arima_totais_bc)
confint(mod_arima_totais_bc)





# ||| Dados Pre
# ||| Sem Transformação
summary(mod_arima_pre)
confint(mod_arima_pre)

# ||| Dados Pre
# ||| Com Transformação
summary(mod_arima_pre_bc)
confint(mod_arima_pre_bc)


tab_comparacao <- data.frame(Mod_Total = mod_arima_totais$aic,
                             Mod_Total_BC = mod_arima_totais_bc$aic,
                             Mod_Pre=mod_arima_pre$aic,
                             Mod_Pre_BC=mod_arima_pre_bc$aic)
row.names(tab_comparacao) <- "AIC"
tab_comparacao




# Resultado ---------------------------------------------------------------
summary(mod_arima_totais)

# O coeficiente "step" é 4150.5544. Isso significa que após a interrupção 
# (no tempo em que "step" passa de 0 para 1), espera-se que a série aumente 
# em 4150.5544 unidades em média. Esta é uma mudança imediata na série no
# momento da interrupção.

# ramp é -58.9379, após o impacto a cada mês a turbidez diminui em média
# esse valor em unidades.


summary(mod_arima_totais_bc)

# Voltar para a escala original
lambda <- lambda_BC$lambda

# ramp
valor.ramp <- -0.02 # Substitua pelo valor predito 

if (lambda != 0) {
  estimate_ramp <- ((lambda * valor.ramp) + 1)^(1/lambda)
} else {
  estimate_ramp <- exp(estimate_ramp)
}
estimate_ramp

# step
valor.step <- 2.7903 # Substitua pelo valor predito 

if (lambda != 0) {
  estimate_step <- ((lambda * valor.step) + 1)^(1/lambda)
} else {
  estimate_step <- exp(estimate_step)
}
estimate_step

# valores
estimate_ramp
estimate_step


#> Conclusão: a interpretação desse modelo não faz tanto sentido, quanto do 
#> modelo sem transformar, mesmo depois que eu volto para a escala original


# Selecionar Modelo -----------------------------------------------------------
# Aplicar o Modelo escolhido a todos os dados usando os indicadores de 
# step e ramp
# Modelos selecionados pelo auto.arima:
#> Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors  - dados totais
#> Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors  - dados totais boxcox
#> Best model: ARIMA(3,0,0)(1,0,1)[12] with non-zero mean - dados pre
#> Best model: ARIMA(2,0,0)(2,1,1)[12]                    - dados pre boxcox


# ||| Transformar dados 
#     Transformar em tsibble
dados_mensais <- 
  dados_mensais |> 
  mutate(Data = as.Date(Ano_mes)) |>
  mutate(Mes = month(Data)) |> 
  arrange(Estacao,Data) |> 
  as_tibble(index = Ano_mes,
            key = Estacao)
lambda_BC <- car::powerTransform(dados_mensais$Turbidez)
lambda_BC
dados_mensais$bc_Turbidez <- (((dados_mensais$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

 rd019_pre <- dados_mensais |> 
   filter(Estacao == "RD019") |> 
   filter(Ano_mes <= as.Date("2015-10-01"))

# Ano atípico de precipitação
 rd019_pre <- 
   rd019_pre |> 
   mutate(Ano_atipico = ifelse(Data<= as.Date("2015-09-30") & Data >= as.Date("2014-01-01"),
                               "atipico","tipico"))
 rd019_pre$Ano_atipico <- as.factor(rd019_pre$Ano_atipico)

 rd019_pre$Ano_atipico <-ifelse(rd019_pre$Ano_atipico == "atipico", 1, 0)
 View(rd019_pre)
 
 
# Grafico
rd019_pre |> 
  ggplot(aes(x = Ano_mes, y= Turbidez)) +
  geom_line()

# Separar Treino | Teste --------------------------------------------------
# Separar a base de treino e teste
quant_anos <- max(year(rd019_pre$Ano_mes)) -
min(year(rd019_pre$Ano_mes))
quant_anos

split <-
  time_series_split(
    rd019_pre,
    Data,
    initial = "11 years", # usado para ajustar
    assess = "7 years")  # usado para testar
split


# Gráfico
split %>%                        # como ficou a divisão
  tk_time_series_cv_plan() %>%   # transforma o split em uma tabela
  plot_time_series_cv_plan(Data, Turbidez)  # plot da tabela

# Backtest
backtest <- 
  time_series_cv(
    rsample::training(split),
    Data,
    cumulative = FALSE,
    initial = "7 years",    # usado para ajustar
    assess = "2 years",     # usado para testar
    skip = "12 month",       # de 2 em 2 anos
    slice_limit = 10        # separar base para deixar 1 teste (1 para teste e 9 para treino)
    )

# Gráfico dos slices do Treino
plot_time_series_cv_plan(backtest, Data, Turbidez)


# ||||||||| Teste ---------------------------------------------------------
# Ajuste do Modelo --------------------------------------------------------

# > Modelo: AutoArima
model_autoarima <- modeltime::arima_reg(seasonal_period = "1 year") |>
  set_engine("auto_arima")

# Ajuste
fit_autoarima <- model_autoarima %>%
  fit(bc_Turbidez ~ Data, training(split))

# > Modelo: Naive
model_snaive <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")
# Ajuste
fit_snaive <- model_snaive %>%
  fit(bc_Turbidez ~ Data, training(split))

# > Modelo 2: Regressao Linear
# Especificar o modelo
model_reglinear <- parsnip::linear_reg() %>%
  set_engine("lm")
# Ajuste
fit_reglinear <- model_reglinear |>
  fit(bc_Turbidez ~ Data,
      training(split))

# > Modelo 3: Arima escolhido
model_arima1 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 3,
  non_seasonal_differences = 0,
  non_seasonal_ma = 2,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima")

# Ajuste
fit_arima1 <- model_arima1 |>
  fit(bc_Turbidez ~ Data , training(split))





# Tabela com Modelos ------------------------------------------------------
# Tabela de Modelos: modeltime_table ( )--------------------------------------
# Cria tabela para Comparar o desempenho dos modelos
models_tbl <- modeltime_table(
  fit_snaive,
  fit_reglinear,
  fit_autoarima,
  fit_arima1)

# Calibrando com dados teste
calibration_tbl <-
  models_tbl |>
  modeltime_calibrate(new_data = testing(split))

# Previsão
forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = rd019_pre)
plot_modeltime_forecast(forecasts)

# Acurácia
calibration_tbl |>
  modeltime_accuracy() |>
  View()

# DIAGNOSTICO
calibration_tbl |>
  modeltime_residuals() |> 
  filter(.model_id == 4) %>%
  plot_acf_diagnostics(.index, .residuals)

modelo <- calibration_tbl |>
  modeltime_residuals() |> 
  filter(.model_id == 4)



# Ajuste Modelo -----------------------------------------------------------


# ||| Modelo 1: naive com sazonalidade
# Especificação do Modelo Mais simples com Sazonalidade de 1 ano
model_snaive <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")
# Ajuste
fit_snaive <- model_snaive %>%
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 2: Regressao com sinalização do impacto
# Especificar o modelo
model_reglinear <- parsnip::linear_reg() %>%
  set_engine("lm")
# Ajuste
fit_reglinear <- model_reglinear |>
  fit(bc_Turbidez ~ Data,
      training(split))


# ||| Modelo 3: Arima escolhido
model_arima1 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 0,
  non_seasonal_ma = 2,
  seasonal_ar = 0,
  seasonal_differences = 0,
  seasonal_ma = 2) |>
  set_engine("arima")

# Ajuste
fit_arima1 <- model_arima1 |>
  fit(bc_Turbidez ~ Data , training(split))


# ||| Modelo 4: Arima escolhido
model_arima2 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 0,
  non_seasonal_ma = 1,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima")
fit_arima2 <- model_arima2 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 5: Arima escolhido
model_arima3 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 3,
  non_seasonal_differences = 0,
  non_seasonal_ma =0,
  seasonal_ar = 1,
  seasonal_differences = 0,
  seasonal_ma = 1) |>
  set_engine("arima")
fit_arima3 <- model_arima3 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 6: Arima escolhido
model_arima4 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,
  non_seasonal_differences = 0,
  non_seasonal_ma =0,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima")
fit_arima4 <- model_arima4 |>
  fit(bc_Turbidez ~ Data, training(split))

# ||| Modelo 7: Arima escolhido
model_arima5 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,
  non_seasonal_differences = 0,
  non_seasonal_ma =0,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 3) |>
  set_engine("arima")
fit_arima5 <- model_arima5 |>
  fit(bc_Turbidez ~ Data, training(split))

# ||| Modelo 8: Arima escolhido
model_arima6 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,
  non_seasonal_differences = 1,
  non_seasonal_ma =0,
  seasonal_ar = 2,
  seasonal_differences = 0,
  seasonal_ma = 3) |>
  set_engine("arima")
fit_arima6 <- model_arima6 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 9: Arima escolhido
model_arima7 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 3,          # p
  non_seasonal_differences = 1, # d
  non_seasonal_ma =0,           # q
  seasonal_ar = 3,              # P
  seasonal_differences = 0,     # D
  seasonal_ma = 3) |>           # Q
  set_engine("arima")
fit_arima7 <- model_arima7 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 10: Arima escolhido
model_arima8 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 3,          # p
  non_seasonal_differences = 1, # d
  non_seasonal_ma =0,           # q
  seasonal_ar = 3,              # P
  seasonal_differences = 1,     # D
  seasonal_ma = 3) |>           # Q
  set_engine("arima")
fit_arima8 <- model_arima8 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 3: Arima escolhido
model_arima9 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 0,
  non_seasonal_ma = 1,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima")

# Ajuste
fit_arima9 <- model_arima9 |>
  fit(bc_Turbidez ~ Data, training(split))


# ||| Modelo 10: Arima escolhido
model_arima10 <- modeltime::arima_reg() |> 
  set_engine("auto_arima")

# Ajuste
fit_arima10 <- model_arima10|>
  fit(bc_Turbidez ~ Data, training(split))


# Tabela com Modelos ------------------------------------------------------
# Tabela de Modelos: modeltime_table ( )--------------------------------------
# Cria tabela para Comparar o desempenho dos modelos
models_tbl <- modeltime_table(
  fit_snaive,
  fit_reglinear,
  fit_arima1,
  fit_arima2,
  fit_arima3,
  fit_arima4,
  fit_arima5,
  #fit_arima6,
  fit_arima7,
  fit_arima8,
  fit_arima9,
  fit_arima10)

# Calibrando com dados teste
calibration_tbl <-
  models_tbl |>
  modeltime_calibrate(new_data = testing(split))

# Previsão
forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = rd019_pre)

plot_modeltime_forecast(forecasts)

# Acurácia
# Escolher modelo com mape mais baixo
# Escolher modelo com rsquare mais alto
# Vou escolher o modelo 8 que tem 52% de explicação e o menor mape dentre
# os que possuem maior explicação
calibration_tbl |>
  modeltime_accuracy() |>
  View()


# DIAGNOSTICO
calibration_tbl |>
  modeltime_residuals() |> 
  filter(.model_id == 11) %>%
  plot_acf_diagnostics(.index, .residuals)



# Refit - re-trains the models on the full dataset so we can get the most
# accurate predictions in the future
future_forecast <- calibration_tbl |>
  filter(.model_id == 11 ) |> 
  modeltime_refit(rd019_pre) |> 
  modeltime_forecast(
    h = "92 months",
    actual_data = rd019_pre,
    keep_data = T
  )

future_forecast |> 
  plot_modeltime_forecast()  



# Tentando: Gráfico -------------------------------------------------------
# Fazer Previsão para tudo
previsao_antes <- 
  calibration_tbl |>
  filter(.model_id == 11 ) |>
  modeltime_forecast(
    new_data = rd019_pre,
    actual_data = rd019_pre,
    keep_data = T)
plot_modeltime_forecast(previsao_antes)

previsao_antes <- previsao_antes |> 
  filter(.key == "prediction")


previsao_depois <- 
 calibration_tbl |>
   filter(.model_id == 11 ) |> 
  modeltime_refit(rd019_pre) |> 
  modeltime_forecast(
    h = "104 months",
    actual_data = rd019_pre,
    keep_data = T
  )

plot_modeltime_forecast(previsao_depois)



# .model_id=4
# .key="prediction"
valores_preditos <- cbind(previsao_antes, previsao_depois)

View(previsao_depois)


# Grafico -----------------------------------------------------------------
rd019_pos <- 
  rd019 |> 
  filter(Ano_mes > as.Date("2015-10-01"))


future_forecast |> 
  ggplot(aes(x = yearmonth(Data), y = .value, col = .key)) +
  geom_line(size = 0.6) +
  # geom_line(data= previsao_antes,
  #           aes(x = yearmonth(Ano_mes),
  #               y = .value) )+
  labs(x = "Tempo", y = "Turbidez") +
  geom_vline(xintercept = as.Date("2015-11-01"), col = "firebrick", linetype = "dashed") +
  geom_line(data = rd019_pos, aes(x = yearmonth(Ano_mes), y = bc_Turbidez),
            col = "black",size = 0.6, lty = 1) +
  scale_color_manual(values = c("actual" = "black", "prediction" = "orange")) +
  theme_few()

#View(future_forecast)



# Cohens ------------------------------------------------------------------

previsto <- 
  future_forecast |> 
  filter(.key == "prediction") |> 
  select(.index, .value) |> 
  mutate(Ano_mes = yearmonth(.index),
         Status = "Previsto",
         Estacao = "RD019") |> 
  rename(Turbidez = .value) |> 
  select(-.index) 
  
observado <- rd019_pos |> 
  select(Ano_mes, bc_Turbidez) |> 
  rename(Turbidez = bc_Turbidez) |> 
  mutate(Status = "Atual")

previsto <- as.data.frame(previsto)
observado <- as.data.frame(observado)



predicao_futura <- as.numeric(predicao_futura[,".value"])
lsr::cohensD(previsto[,"Turbidez"],observado[,"Turbidez"])

tab <- rbind(previsto, observado)

# Gráfico predito vs Observado --------------------------------------------

tab |> 
  ggplot(aes(x = yearmonth(Ano_mes), y = Turbidez, col = Status)) +
  geom_line(size = 0.6) +
  labs(x = "Tempo", y = "Turbidez") +
  geom_vline(xintercept = as.Date("2015-11-01"), col = "firebrick", linetype = "dashed") +
  scale_color_manual(values = c("Atual" = "black", "Previsto" = "orange")) +
  theme_few()





# Previsão para a base toda
forecasts <- calibration_tbl |>
 # filter(.model_id == 7) |> 
  modeltime_forecast(
    new_data = rd019_pre,
    actual_data = rd019_pre)
plot_modeltime_forecast(forecasts)










# -------------------------------------------------------------------------
# t-sibble obj
rd019_pre.ts <-  ts(rd019_pre[,"Turbidez"],
                        frequency = 12,
                        start = min(rd019_pre$Ano_mes))

# t-sibble obj boxcox
rd019_pre.ts.bc <-  ts(rd019_pre[,"bc_Turbidez"],
                           frequency = 12,
                           start = min(rd019_pre$Ano_mes))


# Normal: Fit o modelo escolhido para os dados antes do impacto
modelo_arima_antes <- Arima(window(rd019_pre.ts, end = c(2015,10)),
                            order = c(3,1,0),
                            seasonal = list (order = c(3,1,3),
                                             period = 12))

# BoxCox: Fit o modelo escolhido para os dados antes do impacto
modelo_arima_antes <- Arima(window(rd019_pre.ts.bc, end = c(2015,10)),
                            order = c(3,1,0),
                            seasonal = list (order = c(3,1,3),
                                             period = 12))




mes_final <- as.Date(max(rd019$Ano_mes))
m1_total <- Arima(window(dados.ts, end = mes_final),
                                           order = c(3,1,0),
                                           xreg = cbind(ramp,step),
                                           seasonal = list (order = c(3,1,3),
                                                            period = 12)) 


# ||| Diagnostico
qqnorm(m1_total$residuals) # não normal
qqline(m1_total$residuals)
acf(m1_total$residuals)

fc <- forecast(m1_total, h = 20)
fc.ts <- ts(as.numeric(fc$mean), start = c(350,03), frequency = 12) 



# ||| Gráfico
graf_total <- 
  rd019 |> 
  ggplot(aes(x = Time, y = Turbidez))+
  geom_point(col = "gray", shape = 19)+
  labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(rd019$bc_Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = m1_total$), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(dados1$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()








# Contrafactual -----------------------------------------------------------
# Modelar os dados excluindo o pós intervenção para o contrafactual
antes_impacto <- 
  rd019 |> 
  filter(Ano_mes<= as.Date("2015-10-01"))

# t-sibble obj
antes_impacto.ts <-  ts(antes_impacto[,"Turbidez"],
                        frequency = 12,
                        start = min(antes_impacto$Ano_mes))

# t-sibble obj boxcox
antes_impacto.ts.bc <-  ts(antes_impacto[,"bc_Turbidez"],
                        frequency = 12,
                        start = min(antes_impacto$Ano_mes))

# Modelo só com dados antes do impacto
# Neste caso usando o modelo sem transformar
modelo_arima_antes <- Arima(window(antes_impacto.ts, end = c(2015,10)),
                            order = c(1,0,2),
                            seasonal = list (order = c(0,0,2),
                                             period = 12))

# Confirmando com outro comando que já me dá os gráficos de diagnóstico
astsa::sarima(antes_impacto.ts, 1,0,2, P=0,D=0, Q=2, S =12)

# Fazendo com os dados transformados
astsa::sarima(antes_impacto.ts.bc, 1,0,1, P=2,D=1, Q=1, S =12)



# Forecast 12 months post-intervention and convert to time series object
fc <- forecast(modelo_arima_antes, h = 12)
fc.ts <- ts(as.numeric(fc$mean), start = c(350,03), frequency = 12) 

# Combine with observed data
model.ts.2 <- ts.union(antes_impacto.ts, fc.ts)
model.ts.2



# Plot
plot(model.ts.2,
     type = "l",
     plot.type = "s",
     col = c('blue','red'),
     xlab = "Month",
     ylab = "Dispensings",
     linetype = c("solid","dashed"),
     #ylim = c(0,40000)
)
abline (v=350, lty="dashed", col="gray")



# ||||||||||| Diferenciação
# ACF dados sem diferenciação
astsa::acf2(rd019$Turbidez, max.lag= 36)

# 1 ordem
astsa::acf2(diff(rd019$Turbidez), max.lag= 36)

# View ACF/PACF plots of differenced/seasonally differenced data
astsa::acf2(diff(diff(rd019$Turbidez,12)), max.lag=24)


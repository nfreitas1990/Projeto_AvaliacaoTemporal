
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

rd019 <- dados_mensais |> 
  filter(Estacao == "RD019")


# Grafico
rd019 |> 
  ggplot(aes(x = Ano_mes, y= Turbidez)) +
  geom_line()

# Separar Treino | Teste --------------------------------------------------
# Separar a base de treino e teste
quant_anos <- max(year(rd019$Ano_mes)) -
  min(year(rd019$Ano_mes))
quant_anos

split <-
  time_series_split(
    rd019,
    Data,
    initial = "20 years", # usado para ajustar
    assess = "2 years")  # usado para testar
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

# Ajuste Modelo -----------------------------------------------------------


# ||| Modelo 1: naive com sazonalidade
# Especificação do Modelo Mais simples com Sazonalidade de 1 ano
model_snaive <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")
# Ajuste
fit_snaive <- model_snaive %>%
  fit(bc_Turbidez ~ Data + as.factor(BeforeAfter), training(split))


# ||| Modelo 2: Regressao com sinalização do impacto
# Especificar o modelo
model_reglinear <- parsnip::linear_reg() %>%
  set_engine("lm")
# Ajuste
fit_reglinear <- model_reglinear |>
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter),
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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))


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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))


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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))


# ||| Modelo 6: Arima escolhido
model_arima4 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,
  non_seasonal_differences = 0,
  non_seasonal_ma =0,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima", include.drift = T)
fit_arima4 <- model_arima4 |>
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))

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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))

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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))


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
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))


# ||| Modelo 10: Arima escolhido
model_arima8 <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,          # p
  non_seasonal_differences = 1, # d
  non_seasonal_ma =0,           # q
  seasonal_ar = 3,              # P
  seasonal_differences = 1,     # D
  seasonal_ma = 3) |>           # Q
  set_engine("arima")
fit_arima8 <- model_arima8 |>
  fit(bc_Turbidez ~ Data+ as.factor(BeforeAfter), training(split))



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
  fit_arima6,
  fit_arima8
  )

# Calibrando com dados teste
calibration_tbl <-
  models_tbl |>
  modeltime_calibrate(new_data = testing(split))

# Previsão
forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = rd019)

plot_modeltime_forecast(forecasts)

# Acurácia
# Escolher modelo com mape mais baixo
# Escolher modelo com rsquare mais alto
# 
calibration_tbl |>
  modeltime_accuracy() |>
  View()


# DIAGNOSTICO
calibration_tbl |>
  modeltime_residuals() |> 
  filter(.model_id == 7) %>%
  plot_acf_diagnostics(.index, .residuals)



# Refit - re-trains the models on the full dataset so we can get the most
# accurate predictions in the future
future_forecast <- calibration_tbl |>
  filter(.model_id == 6) |> 
  modeltime_refit(rd019) |> 
  modeltime_forecast(
    h = "92 months",
    actual_data = rd019,
    keep_data = T
  )

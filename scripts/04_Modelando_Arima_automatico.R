

# Arima -------------------------------------------------------------------

# 1. Dados --------------------------------------------------------------


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

funcao_arima <- function(estacao){

# ||| Criar indicadores 
dados1 <- 
  dados_mensais |> 
  filter(Estacao== estacao)

step <- as.vector(dados1$BeforeAfter)
ramp <- as.vector(dados1$TimeSince)

# ||| BoxCox: Transformar
lambda_BC <- car::powerTransform(dados1$Turbidez)
lambda_BC
dados1$bc_Turbidez <- (((dados1$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

# ||| Converter em ts
# ||| Dados Totais
dados.ts <- ts(dados1[,"Turbidez"], frequency = 12, start = min(dados1$Ano_mes))
dados.ts

# ||| Converter em ts
# ||| Dados Totais com transformação BoxCox
dados.ts.bc <- ts(dados1[,"bc_Turbidez"], frequency = 12, start = min(dados1$Ano_mes))
dados.ts.bc

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

# ||| Dados Totais
# ||| Com Transformação
mod_arima_totais_bc <- auto.arima(dados.ts.bc,
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp),
                                  stepwise = FALSE,
                                  max.d = 1,
                                  max.D = 1,
                                  trace = TRUE)

# Diagnósticos ------------------------------------------------------------

# ||| Dados Totais
# ||| Sem Transformação
# Checar os resíduos - MODELO escala normal
# Ho: Não há autocorrelação nos resíduos
diag1 <- checkresiduals(mod_arima_totais)
diag2 <- Box.test(mod_arima_totais$residuals, lag = 24, type = "Ljung-Box")

# ||| Dados Totais
# ||| Com Transformação
# Checar os resíduos - MODELO Log
# Ho: Não há autocorrelação nos resíduos
diag1_bc <- checkresiduals(mod_arima_totais_bc)
diag2_bc <- Box.test(mod_arima_totais_bc$residuals, lag = 24, type = "Ljung-Box")



# Resumo Modelos ----------------------------------------------------------

# ||| Dados Totais
# ||| Sem Transformação
#> Estimar Parâmetros
resultado <- summary(mod_arima_totais)
confint(mod_arima_totais)

# ||| Dados Totais
# ||| Com Transformação
#> Estimar Parâmetros em log
resultado_bc <- summary(mod_arima_totais_bc)
confint(mod_arima_totais_bc)

# Salvar ------------------------------------------------------------------

# Salvar tabela como um objeto no ambiente
resultado_salvar <- paste0( "summary_", estacao)
assign(resultado_salvar,resultado, envir = .GlobalEnv)

diagnostico_salvar <- paste0( "diagnostico_", estacao)
assign(diagnostico_salvar,diag1, envir = .GlobalEnv)

# Salvar tabela como um objeto no ambiente
resultado_salvar_bc <- paste0( "summary_bc_", estacao)
assign(resultado_salvar_bc,resultado_bc, envir = .GlobalEnv)

diagnostico_salvar_bc <- paste0( "diagnostico_bc_", estacao)
assign(diagnostico_salvar_bc,diag1_bc, envir = .GlobalEnv)


} 




# Rodar o Modelo para as estações -----------------------------------------
purrr::set_names(unique(dados_mensais$Estacao)) |>
  purrr::map(funcao_arima)

summary_RD019
summary_RD023
summary_RD033
summary_RD035
summary_RD044
summary_RD045
summary_RD053
summary_RD058
summary_RD059
summary_RD067
summary_RD071
summary_RD072
summary_RD083




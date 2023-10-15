source("scripts/03_decomposicao.R")


# Objetivo ----------------------------------------------------------------




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


# ||| Dados Totais RD019
# ||| Criar indicadores 
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
# ||| Criar indicadores 
rd019_pre <- 
  mensal_pre |> 
  filter(Estacao== "RD019")

step_pre <- as.vector(rd019_pre$BeforeAfter)
ramp_pre <- as.vector(rd019_pre$TimeSince)




# ||| Converter em ts
dados.ts <- ts(rd019[,"Turbidez"], frequency = 12, start = min(rd019$Ano_mes))
dados.ts

# Com transformação boxCox
dados.ts.bc <- ts(rd019[,"bc_Turbidez"], frequency = 12, start = min(rd019$Ano_mes))
dados.ts.bc


# ||| Auto.arima( ) - Com indicadores

# Sem Transformação
mod_arima1 <- auto.arima(dados.ts,
                         seasonal = TRUE,
                         xreg = cbind(step,ramp),
                         stepwise = FALSE,
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors


# Com Transformação
mod_arima2 <- auto.arima(dados.ts.bc,
                         seasonal = TRUE,
                         xreg = cbind(step,ramp),
                         stepwise = FALSE,
                         max.d = 1,
                         max.D = 1,
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 



# Diagnósticos ------------------------------------------------------------

# Checar os resíduos - MODELO escala normal
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima1)
Box.test(mod_arima1$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.


# Checar os resíduos - MODELO Log
# Ho: Não há autocorrelação nos resíduos
checkresiduals(mod_arima2)
Box.test(mod_arima2$residuals, lag = 24, type = "Ljung-Box")

#> Conclusão: Modelo ajustado. Pois Não há autocorrelação nos resíduos. E a 
#> distribuição parece normal.


# Resumo Modelos ----------------------------------------------------------
#> Estimar Parâmetros
summary(mod_arima1)
confint(mod_arima1)

#> Estimar Parâmetros em log
summary(mod_arima2)
confint(mod_arima2)

tab_comparacao <- data.frame(Mod1 = mod_arima1$aic, Mod2 = mod_arima2$aic)
row.names(tab_comparacao) <- "AIC"
tab_comparacao


# Resultado ---------------------------------------------------------------
summary(mod_arima1)

# O coeficiente "step" é 4150.5544. Isso significa que após a interrupção 
# (no tempo em que "step" passa de 0 para 1), espera-se que a série aumente 
# em 4150.5544 unidades em média. Esta é uma mudança imediata na série no
# momento da interrupção.

# ramp é -58.9379, após o impacto a cada mês a turbidez diminui em média
# esse valor em unidades.


summary(mod_arima2)

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

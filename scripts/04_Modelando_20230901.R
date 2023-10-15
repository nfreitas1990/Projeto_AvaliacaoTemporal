source("scripts/03_decomposicao.R")


# Objetivo ----------------------------------------------------------------
# Esse script tem como objetivo Avaliar a magnitude das diferenças 
# pré e pós impacto na qualidade da água da bacia do rio doce



# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Modelos -----------------------------------------------------------------



# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Opção 1: Regressão Segmentada com sazonalidade --------------------------
# Referencia: Travis-Lumer et al. Effect size quantification for interrupted time
# series analysis: implementation in R and analysis for Covid‑19 research.2022

#install_github("Yael-Travis-Lumer/its2es")
library(devtools)
library(its2es)

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

# Definir a fórmula  --------------------------
form1 <- as.formula("Turbidez ~ Ano_mes") # só com tempo
form2 <- as.formula("bc_Turbidez ~ Ano_mes") # Tranformado por BoxCox
# Indicador de tempo "time" por estação
dados_mensais <- 
  dados_mensais |> 
  group_by(Estacao) |> 
  arrange(Ano_mes) |> 
  group_by(Estacao) |> 
  mutate(Time= seq(from = 1, to = n(), by = 1))

# checar se o indicador fez o que eu queria. que era colocar o tempo do 1 até
# o final da série para cada estação
# dados_mensais |> 
#   filter(Estacao=="RD019") |> 
#   summarise(unique(Time)) |> 
#   View()

# Separar os indicadores em vetores
BeforeAfter <- dados_mensais$BeforeAfter
TimeSince <- dados_mensais$TimeSince

# Fit Linear Model -----------------------------------------------------

# rd019
rd019 <- 
  dados_mensais |> 
  filter(Estacao == "RD019")

# Transformar
lambda_BC <- car::powerTransform(rd019$Turbidez)
lambda_BC
rd019$bc_Turbidez <- (((rd019$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

# Indicador de intervencao para a função its_lm
intervention_start_ind <- which (rd019$Ano_mes == as.Date("2015-11-01"))

fit1 <- its_lm(data = rd019,
               form = form1,
               time_name = "Time",
               intervention_start_ind = intervention_start_ind,
               freq = 12,
               seasonality = "full",
               impact_model = "full",
               counterfactual = TRUE,
               print_summary = FALSE)

fit2 <- its_lm(data = rd019,
               form = form2,
               time_name = "Time",
               intervention_start_ind = intervention_start_ind,
               freq = 12,
               seasonality = "full",
               impact_model = "full",
               counterfactual = TRUE,
               print_summary = FALSE)


# Gráfico -----------------------------------------------------------------

# Ajeitar os dados porque o gráfico nao estava funcionando
dados_plotar<- fit1$data 
dados_plotar <- 
  dados_plotar|> 
  mutate(time = as.Date(Ano_mes))

dados_plotar_2<- fit2$data 
dados_plotar_2 <- 
  dados_plotar_2|> 
  mutate(time = as.Date(Ano_mes))


# Gráfico padrao do pacote
p <- plot_its_lm(data = dados_plotar,
                 intervention_start_ind = intervention_start_ind,
                 y_lab = "Time",
                 response="Turbidez", date_name= "time")
p

  
  

p2 <- plot_its_lm(data = dados_plotar_2,
                 intervention_start_ind = intervention_start_ind,
                 y_lab = "Time",
                 response="bc_Turbidez", date_name= "time")
p2 

# Não entendi o que foi usado para plotar o gráfico. Então abri o código da
# função para tentar entender quais valores que o modelo estava usando. 
# Parece que ele não usa os dados originais para traçar as linhas do 
# fitted values, não entendi o porquê.
# Pelo o que entendi:
# pred: dados preditos para a série de turbidez sem o impacto
# pred_c: contrafactual
# time: tive que criar essa coluna para o gráfico plotar, mas o tempo
# está errada nela. não sei pq ficou assim
body(plot_its_lm)
body(its_lm)
# its_lm_fourier()



# Gráfico -----------------------------------------------------------------
# Regressao segmentada com Sazonalidade
dados_plotar |> 
ggplot()+
  geom_point(aes(x = Ano_mes, y = Turbidez), color = "gray")+
  geom_line(aes(y= predC, x = Ano_mes),
            color = "orange",
            size = 1)+
  
  geom_line(aes(y = pred, x = Ano_mes))+
  geom_vline(xintercept = as.Date("2015-09-01"),
             linetype = "dashed", color = "red")+
  # annotate("text",y=3000)
  theme_few()

colnames(dados_plotar)


# Resultados --------------------------------------------------------------
fit1
fit2


# Dignostico --------------------------------------------------------------
# Não consigo checar o diagnóstico
acf(resid(fit1$model))$acf[2] # autocorrelacionado
acf(resid(fit2$model))$acf[2] # autocorrelacionado
qqnorm(resid(fit1$model)) #normal - mais ou menos
qqline(resid(fit1$model))



# Conclusão - Regressão Segmentada ----------------------------------------
# Não sei ao certo como interpretar os valores. Os valores parecem ser 
# autocorrelacionados.

# Referencia: Travis-Lumer et al. Effect size quantification for interrupted time
# series analysis: implementation in R and analysis for Covid‑19 research.2022



# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Opção 2: Regressão Linear     -------------------------------------------
# Referencia: https://ds4ps.org/pe4ps-textbook/docs/p-020-time-series.html#fig:mixedeffect
# Regressão Linear com indicadores

# Dados com os indicadores
dados_mensais

# Acrescentar variável de sazonalidade
dados_mensais <- 
  dados_mensais |> 
  mutate(Mes = month(Ano_mes))

# Selecionar uma série temporal só = estação rd019
# rd019
rd019 <- 
  dados_mensais |> 
  filter(Estacao == "RD019")

# Transformar
lambda_BC <- car::powerTransform(rd019$Turbidez)
lambda_BC
rd019$bc_Turbidez <- (((rd019$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)


# Fit linear model
modelo <- lm(Turbidez ~ Time + BeforeAfter + TimeSince, data =rd019)
modelo_bc <- lm(bc_Turbidez ~ Time + BeforeAfter + TimeSince, data =rd019)

summary(modelo)
summary(modelo_bc)



# Fit modelo com sazonalidade
# Parece que podemos (embora não seja o melhor jeito) inserir esses termos 
# para considerar sazonalidade. Nem sei se fiz certo. ChatGPT diz q sim. rs
# https://stats.stackexchange.com/questions/477991/linear-regression-with-hour-of-the-day
# Esses parâmetros
modelo2 <- lm(Turbidez ~ 
                Time +
                BeforeAfter +
                TimeSince +
                cos(2 * pi * (Time - 1)/12) +
                sin(2 * pi * (Time - 1)/12),
              data =rd019)
modelo2_bc <- lm(bc_Turbidez ~ 
                Time +
                BeforeAfter +
                TimeSince +
                cos(2 * pi * (Time - 1)/12) +
                sin(2 * pi * (Time - 1)/12),
              data =rd019)

summary(modelo2)
summary(modelo2_bc)


# Diagnostico ---------------------------------------------------------------
plot(modelo)
plot(modelo_bc)
plot(modelo2)
plot(modelo2_bc)

# Modelos transformado por Box-Cox são mais aceitáveis.
# Nos modelos sem transformação:
# -Resíduos vs valores preditos: Tem padrão nos resíduos
# -Resíduos não tem distribuição normal
# -Resíduo vs Leverage: Tem pontos influentes - pontos do impacto provavelmente

# Conclusão: Melhor optar pelos modelos tranformados. Modelo2_bc (transformado
# e com sazonalidade parece ser o melhor ajustado segundo o diagnóstico)


# Comparação --------------------------------------------------------------
# Nulo
mod0 <- lm(Turbidez ~ 1, data = rd019)

anova(mod0, modelo, test = "Chi") # linear p<0.05 - modelo é melhor
anova(modelo,modelo2, test = "Chi") # modelo2 melhor


#> Modelo linear com sazonalidade é o melhor. E o modelo transformado por
#> BoxCox apresenta melhor dignóstico. Só que ele lineariza o gráfico. 
#> Tenta trocar no item abaixo pelo modelo transformado, a linha de tendencia 
#> fica somente uma linha reta




# Gráfico -----------------------------------------------------------------
plot(rd019$Time,
     rd019$Turbidez,
     bty="n",
     pch=19,
     col="gray",
     xlab = "Tempo", 
     ylab = "Turbidez")

abline(v = min(which(rd019$Ano_mes > as.Date("2015-10-01"))),
       col="firebrick",
       lty=2 )

text(x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
     y = max(rd019$Turbidez),
     "Impacto", col="firebrick", cex=1.3, pos=4 )

# Adicionar a linha de regressão linear
ts <- lm(Turbidez ~ Time + BeforeAfter + TimeSince, data = rd019)
lines(rd019$ Time, ts$fitted.values, col="steelblue", lwd = 2 )

# Refaz o gráfico com o modelo com sazonalidade e veja como esse termo
# realmente possibilita sobe e desce da curva (ciclos)
ts <- lm(Turbidez ~ Time + BeforeAfter + TimeSince+
  cos(2 * pi * (Time - 1)/12) +
  sin(2 * pi * (Time - 1)/12), data = rd019)

# Resumo Modelo Lineares sem tranformação
summary(modelo)
summary(modelo2)

#> Modelo:
#> Time: Não deu significativo. Indica não haver tendencia de aumento ou
#>       diminuição na turbidez antes do impacto
#> BeforeAfter: Indica aumento imediato dos valores de Turbidez após o impacto.
#> TimeSince: Indica que a tendencia se alterou após a intervenção. O efeito
#>            é negativo, indicando que a cada mês que passa os valores de 
#>            turbidez diminuem em 8.85 unidades de turbidez.

#> Modelo2 : A sazonalidade tem foi significativa


# Resumo Modelos Lineares BoxCox
summary(modelo_bc)
summary(modelo2_bc)

# Modelos tranformados: Nesses modelos o tempo também foi significativo


# Conclusão ---------------------------------------------------------------
# Parece que a regressão linear não tem um bom ajuste quando não transformamos
# os dados. Quando acrescentamos esse ter de seno e coseno (que vi na net que 
# considera sazonalidade, dando maior flexibilidade ao modelo) o modelo fica um
# pouco mais explicativo, mas os gráficos de diagnostico não melhoram muito.
# Quando transformo os dados com BoxCox, o diagnóstico fica bem melhor e 
# Esses modelos possuem desempenho melhor. Só que na hora que ploto o gráfico
# acho que fica melhor usar os modelo linear sem transformação para conseguir
# ver a tendencia. Não sei se ploto o que dá para ver sazonalidade junto

# Resumo dos modelos
summary(modelo)
summary(modelo_bc)
summary(modelo2)
summary(modelo2_bc)

# Predict -----------------------------------------------------------------
# Quando ocorre a intervenção?
min(which(rd019$Ano_mes == as.Date("2015-11-01")))
rd019[218:221,]

# Logo após a intervenção
# Note que BeforeAfter e TimeSince é igual a 1 porque é o momento logo após
# a intervenção

# Para quais dados eu quero predizer?
data1 <- as.data.frame( cbind( Time = 219, BeforeAfter = 1, TimeSince = 1 )) 

# Predict
y1 <-predict( modelo2, data1 ) # modelo sem transformar
y1_bc <-predict( modelo2_bc, data1 ) 

# Voltando o valor transformado BoxCox para a escala original
lambda <- lambda_BC$lambda
y_pred_transformed <- y1_bc #4.469257  # Substitua pelo valor predito 

if (lambda != 0) {
  y_pred_original <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
} else {
  y_pred_original <- exp(y_pred_transformed)
}
y_pred_original



# GRÁFICO
# Plotar observações
# plot(y = rd019$Turbidez, x = rd019$Time,
#       bty="n",
#       col = gray(0.5,0.5), pch=19,
#       #xlim = c(1, 365), 
#       #ylim = c(0, 300),
#       xlab = "Tempo", 
#       ylab = "Turbidez (NTU)")
# 
# # Adicionar o ponto que mostra o nível predito para o tempo da intervenção
# points( 219, y_pred_original, col = "dodgerblue4", 
#         pch = 19, bg = "dodgerblue4", cex = 2 )
# text(219, y_pred_original, labels = "2015 nov", pos = 4, cex = 1 )
# text(219, y_pred_original-200, labels = "243 NTU", pos = 4, cex = 1 )
# 
# # linha da interrupção
# abline(v=219, col="red", lty=2 )


# Predic - para 3 anos após a interrupção
# momento da interrupção + 36 meses depois
# Time 255
min(which(rd019$Ano_mes == as.Date("2015-11-01")))+  36
linha <- rd019[255,]  

# Neste caso, BeforeAfter = 1 (sempre 1 pq indica depois impact)/ TimeSince = 37
data2 <- as.data.frame(cbind(Time = linha$TimeSince, 
                               BeforeAfter = linha$BeforeAfter,
                               TimeSince = linha$TimeSince )) 
# Predict
y2 <-predict( modelo2, data2 ) # modelo sem transformar
y2_bc <-predict( modelo2_bc, data2 ) 

# Voltando o valor transformado para a escala original
lambda <- lambda_BC$lambda
y_pred_transformed <- y2_bc # Substitua pelo valor predito 

if (lambda != 0) {
  y_pred_original2 <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
} else {
  y_pred_original2 <- exp(y_pred_transformed2)
}
y_pred_original2


# |||||||
# GRÁFICO
# Plotar observações
 plot(y = rd019$Turbidez, x = rd019$Time,
    bty="n",
      col = gray(0.5,0.5), pch=19,
      #xlim = c(1, 365), 
      #ylim = c(0, 300),
      xlab = "Tempo", 
      ylab = "Turbidez (NTU)")
 
 # Adicionar o ponto que mostra o nível predito para o tempo da intervenção
 points(219, y_pred_original, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
 points(255, y_pred_original2, col = "dodgerblue4", 
         pch = 19, bg = "dodgerblue4", cex = 2 )
 text(219, y_pred_original, labels = "2015 nov", pos = 4, cex = 1 )
 text(255, y_pred_original2, labels = "2018 nov", pos = 4, cex = 1 )
 
 # linha da interrupção
 abline(v=219, col="red", lty=2 )


# Contrafactual -----------------------------------------------------------
# BeforeAfter
# TimeSince
# Ambos serão igual a zero pois quero um momento em que a intervenção não 
# ocorreu.


# Colocar zero para as variáveis, como se a intervenção não tivesse ocorrido
data3 <- as.data.frame(cbind( Time= 219, BeforeAfter = 0, TimeSince = 0))
y3 <- predict(modelo2, data3) #Counterfactual
y3_bc <- predict(modelo2_bc, data3)


# |||||||
# GRÁFICO
# Plotar observações
# plot(y = rd019$Turbidez, x = rd019$Time,
#      bty="n",
#      col = gray(0.5,0.5), pch=19,
#      #xlim = c(1, 365), 
#      #ylim = c(0, 300),
#      xlab = "Tempo", 
#      ylab = "Turbidez (NTU)")
# 
# # We add a  point showing the level of wellbeing at time = 219
# points(219, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)
# 
# # We add a point indicating the counterfactual
# points(2219, y3, col = "darkorange2", pch = 19, cex = 2)
# 
# # Label for our predicted outcome
# text(219, y2, labels = "Y at t = 219", pos = 4, cex = 1)
# 
# #Label for the counterfactual 
# text(219, y3, labels = "C at t = 219", pos = 4, cex = 1)
# 
# # Line marking the interruption
# abline( v=219, col="red", lty=2 )
# 
# 



#|||||||||
# Estimar todos os valores preditos de Y - apenas usar o dataset
pred1 <- predict(modelo, rd019) # linear
pred1_bc <- predict(modelo_bc, rd019) # linear boxcox
pred2 <- predict(modelo2, rd019) # curva com sazonalidade
pred2_bc <- predict(modelo2, rd019) # curva com sazonalidade boxcox

# Voltar pred1_bc e pred2_bc em escala BoxCox para a escala original
 lambda <- lambda_BC$lambda
 y_pred_transformed <- pred1_bc # Substitua pelo valor predito 
 
 if (lambda != 0) {
   y_pred1_bc <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
 } else {
   y_pred1_bc <- exp(y_pred1_bc)
 }
 y_pred1_bc

#
 lambda <- lambda_BC$lambda
 y_pred_transformed <- pred2_bc # Substitua pelo valor predito 
 
 if (lambda != 0) {
   y_pred2_bc <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
 } else {
   y_pred2_bc <- exp(y_pred2_bc)
 }
 y_pred2_bc
 
 
# Criar um novo dataset onde BeforeAfter e TimeSince sejam iguais a zero.
# como se a intervenção não tivesse ocorrido
datanew <- as.data.frame(cbind(Time = rep(1:max(rd019$Time)),
                               BeforeAfter = rep(0),
                               TimeSince = rep(0))) 

# Predict contrafactual
pred1.2 <- predict(modelo, datanew) # linear 
pred1.2_bc <- predict(modelo_bc, datanew) # linear boxcox
pred2.2 <- predict(modelo, datanew) # sazonalidade
pred2.2_bc <- predict(modelo_bc, datanew) # sazonalidade boxcox

# Voltar pred1 em escala BoxCox para a escala original
 lambda <- lambda_BC$lambda
 y_pred_transformed <- pred1.2_bc # Substitua pelo valor predito 
 
 if (lambda != 0) {
   y_pred1.2_bc <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
 } else {
   pred1.2_bc <- exp(pred1.2_bc)
 }
 pred1.2_bc

#
 y_pred_transformed <- pred2.2_bc # Substitua pelo valor predito 
 
 if (lambda != 0) {
   y_pred2.2_bc <- ((lambda * y_pred_transformed) + 1)^(1/lambda)
 } else {
   pred2.2_bc <- exp(pred2.2_bc)
 }
 pred2.2_bc 
 


# GRAFICO - modelo

# Momento da interrupção
interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01")))
tempo_antes_interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01"))) - 1
obs_final <- max(rd019$Time)
#
plot(y = rd019$Turbidez, x = rd019$Time,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     #xlim = c(1, 365), 
     #ylim = c(0, 300),
     xlab = "Tempo", 
     ylab = "Turbidez (NTU)")

lines( rep(1:tempo_antes_interrupcao), pred1[1:tempo_antes_interrupcao], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), pred1[interrupcao:obs_final], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), pred1.2[interrupcao:obs_final], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, -80, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(219, -80, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=219, col="darkorange2", lty=2 )



# |||||||||||||||||||
# GRAFICO - modelo bc

# Momento da interrupção
interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01")))
tempo_antes_interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01"))) - 1
obs_final <- max(rd019$Time)
#
plot(y = rd019$Turbidez, x = rd019$Time,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     #xlim = c(1, 365), 
     #ylim = c(0, 300),
     xlab = "Tempo", 
     ylab = "Turbidez (NTU)")

lines( rep(1:tempo_antes_interrupcao), y_pred1_bc[1:tempo_antes_interrupcao], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), y_pred1_bc[interrupcao:obs_final], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), y_pred1.2_bc[interrupcao:obs_final], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, -80, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(219, -80, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline(v=219, col="darkorange2", lty=2 )


# |||||||||||||||||||
# GRAFICO - modelo 2 (com sazonalidade)
# Não está funcionando pq na hora de prever o modelo lá em cima, eu não 
# coloquei os termos de sazonalidade

# Momento da interrupção
interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01")))
tempo_antes_interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01"))) - 1
obs_final <- max(rd019$Time)

#
plot(y = rd019$Turbidez, x = rd019$Time,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     #xlim = c(1, 365), 
     #ylim = c(0, 300),
     xlab = "Tempo", 
     ylab = "Turbidez (NTU)")

lines( rep(1:tempo_antes_interrupcao), pred2.2[1:tempo_antes_interrupcao], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), y_pred2.2_bc[interrupcao:obs_final], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), y_pred2.2_bc[interrupcao:obs_final], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, -80, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(219, -80, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline(v=219, col="darkorange2", lty=2 )



# |||||||||||||||||||
# GRAFICO - modelo 2 (com sazonalidade e box cox)

# Momento da interrupção
interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01")))
tempo_antes_interrupcao <- min(which(rd019$Ano_mes == as.Date("2015-11-01"))) - 1
obs_final <- max(rd019$Time)

#
plot(y = rd019$Turbidez, x = rd019$Time,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     #xlim = c(1, 365), 
     #ylim = c(0, 300),
     xlab = "Tempo", 
     ylab = "Turbidez (NTU)")

lines( rep(1:tempo_antes_interrupcao), pred2[1:tempo_antes_interrupcao], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), pred2.2[interrupcao:obs_final], col="dodgerblue4", lwd = 3 )
lines( rep(interrupcao:obs_final), pred2.2[interrupcao:obs_final], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, -80, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(219, -80, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline(v=219, col="darkorange2", lty=2 )




# Diagnostico -------------------------------------------------------------
plot(resid(modelo)) # não está bom
plot(resid(modelo_bc)) # está melhor. Mas parece um cone
plot(resid(modelo2))
plot(resid(modelo2_bc))

acf(resid( modelo ))$acf[2]   
acf(resid( modelo_bc ))$acf[2]   
acf(resid( modelo2 ))$acf[2]   
acf(resid( modelo2_bc ))$acf[2]   

# Muito autocorrelacionado. Todos parecem ser modelos autoregressivos para 
# o Arima.





# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Opção 3: Regressão Segmentada -------------------------------------------
# Na primeira regressão segmentada usei um pacote desenvolvido no artigo
# agora estou usando outro pacote
library(segmented)

# Definir a posição do ponto de segmentação (onde a mudança ocorre)
min(which(rd019$Ano_mes == as.Date("2015-11-01")))
rd019[218:221,]

# Definir o modelo linear original
mod1 <- lm(Turbidez ~ Time + BeforeAfter + TimeSince, data = rd019)
mod1_bc <- lm(bc_Turbidez ~ Time + BeforeAfter + TimeSince, data = rd019)

# Ajustar o modelo segmentado
modelo_segmentado <- segmented::segmented(mod1,
                                          seg.Z = ~ Time,
                                          psi =  list(Time = 220)) 
modelo_segmentado_bc <- segmented::segmented(mod1_bc,
                                          seg.Z = ~ Time,
                                          psi =  list(Time = 219))

# Imprimir o resumo do modelo segmentado
# Note que o valor muda se eu colocar 220 ou 221
summary(modelo_segmentado)

# Time: O coeficiente para a variável Time é 0.1950. Não foi significativo.

# BeforeAfter: O coeficiente para a variável BeforeAfter é 4021.3 
# Isso significa que após o ponto de segmentação, quando a variável 
# BeforeAfter muda de 0 para 1, a média da variável resposta (Turbidez)
# aumenta em 4021.3 unidades.

# TimeSince: O coeficiente para a variável TimeSince é -661.8895 
# Isso significa que após o ponto de segmentação, para cada unidade 
# de aumento na variável TimeSince, a média da variável resposta (Turbidez)
# diminui em -661.8895 unidades.


summary(modelo_segmentado_bc)
# Nete caso temos que transformar a escala das variáveis de novo para a escala
# original para interpretar os valores. Vê como fazer isso

# Gráfico -----------------------------------------------------------------
#slope(modelo_segmentado)
#intercept(modelo_segmentado)

#par(mfrow=c(1,1), mar=c(3,2,1,1)+.5, mgp=c(1.6,.6,0))
#plot.segmented(modelo_segmentado, lwd=2, col = "red",
#               add=T, interc=T)


# ||||Falta Terminar isso aqui 
# # Gráfico -----------------------------------------------------------------
# coef(modelo_segmentado)
# intercepto <- as.data.frame(intercept(modelo_segmentado))
# slope <-as.data.frame(slope(modelo_segmentado))
# slope$Time[1,1]
# intercepto$Est.[1] 
# ggplot(rd019)+
#   geom_point(aes(x = Ano_mes, y = Turbidez), col = "gray")+
#   geom_vline(xintercept = as.Date("2015-11-01"),
#              col = "darkred")+
#   geom_abline(intercept = 83.667,
#                 slope = -0.16293, col= "black")
# 
#   
#   theme_bw()


# # Gráfico turbidez  ----------------------------------------------------
# slope(modelo_segmentado)
# intercept(modelo_segmentado)
# modelo_segmentado$fitted.values
# 
# plot(rd019$Time, rd019$Turbidez,
#      bty="n", pch=19, col="gray",
#      xlab = "Tempo", 
#      ylab = "Turbidez" )
# abline(v = min(which(rd019$Ano_mes > as.Date("2015-10-01"))),
#        col="firebrick", lty=2 )
# text(x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
#      y = max(rd019$Turbidez),
#      "Impacto", col="firebrick", cex=1.3, pos=4 )
# 
# # Adicionar a linha de regressão
# lines(rd019$Time, modelo_segmentado$fitted.values, col="steelblue", lwd = 2 )
# 
# 
# # Gráfico BC_turbidez  ----------------------------------------------------
# 
# # Muito Loucooo!!!
# plot(rd019$Time, rd019$bc_Turbidez,
#      bty="n", pch=19, col="gray",
#      xlab = "Tempo", 
#      ylab = "Turbidez" )
# abline(v = min(which(rd019$Ano_mes > as.Date("2015-10-01"))),
#        col="firebrick", lty=2 )
# text(x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
#      y = max(rd019$bc_Turbidez),
#      "Impacto", col="firebrick", cex=1.3, pos=4 )
# 
# # Adicionar a linha de regressão
# lines(rd019$Time, modelo_segmentado_bc$fitted.values, col="steelblue", lwd = 2 )










# Diagnostico -------------------------------------------------------------
plot(modelo_segmentado$residuals)
acf(modelo_segmentado$residuals)
acf(resid( modelo_segmentado ))$acf[2]   

# Conclusão: Modelo não está perfeito, tem autocorrelação nos resíduos. Mas 
# também não parece tão mal.

plot(modelo_segmentado_bc$residuals)
acf(modelo_segmentado_bc$residuals)
acf(resid( modelo_segmentado_bc ))$acf[2]   
# Conclusão: Modelo não está bom tem autocorrelação nos resíduos



# Comparação --------------------------------------------------------------
anova(modelo_bc,modelo2_bc, modelo_segmentado, modelo_segmentado_bc, test = "Chi")
# modelo2_bc é melhor. Mas tem mais autocorrelação que o primeiro

# Modelo Segmentado Sazonalidade ------------------------------------------
# Colocar a sazonalidade no modelo 
mod2 <- lm(Turbidez ~ 
             Time +
             BeforeAfter +
             TimeSince +
             cos(2 * pi * (Time - 1)/12) +
             sin(2 * pi * (Time - 1)/12),
           data =rd019)

# Ajustar o modelo segmentado
modelo_segmentado_sazonalidade <- segmented::segmented(mod2,
                                                       seg.Z = ~ Time,
                                                       psi =  list(Time = 220))
summary(modelo_segmentado_sazonalidade)

# Diagnostico -------------------------------------------------------------
plot(modelo_segmentado_sazonalidade$residuals)
acf(modelo_segmentado_sazonalidade$residuals)
acf(resid( modelo_segmentado_sazonalidade ))$acf[2]   

# Conclusão: Melhorou mas ainda tem um pdrão ali estranho



# Colocar a sazonalidade no modelo com transformação
mod2 <- lm(bc_Turbidez ~ 
             Time +
             BeforeAfter +
             TimeSince +
             cos(2 * pi * (Time - 1)/12) +
             sin(2 * pi * (Time - 1)/12),
           data =rd019)

# Ajustar o modelo segmentado
modelo_segmentado_sazonalidade_bc <- segmented::segmented(mod2,
                                                       seg.Z = ~ Time,
                                                       psi =  list(Time = 220))



grafico <- 
  rd019 |> 
  ggplot(aes(x = Time, y = Turbidez))+
  geom_point(col = "gray", shape = 19)+
  
  labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(rd019$bc_Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = mod2$fitted.values), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(rd019$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()
grafico


# Muito Loucooo!!!
plot(rd019$Time, rd019$Turbidez,
     bty="n", pch=19, col="gray",
     xlab = "Tempo", 
     ylab = "Turbidez" )
abline(v = min(which(rd019$Ano_mes > as.Date("2015-10-01"))),
       col="firebrick", lty=2 )
text(x = min(which(rd019$Ano_mes > as.Date("2015-10-01"))) + 2,
     y = max(rd019$Turbidez),
     "Impacto", col="firebrick", cex=1.3, pos=4 )

# Adicionar a linha de regressão
lines(rd019$Time, modelo_segmentado_sazonalidade$fitted.values, col="steelblue", lwd = 2 )

# Diagnostico -------------------------------------------------------------
plot(modelo_segmentado_sazonalidade_bc$residuals)
acf(modelo_segmentado_sazonalidade_bc$residuals)
acf(resid( modelo_segmentado_sazonalidade_bc ))$acf[2]   
# Conclusão: Muita alto correlação nos primeiros lags.


# Comparação --------------------------------------------------------------
anova(modelo,
      modelo_bc,
      modelo2,
      modelo2_bc,
      modelo_segmentado,
      modelo_segmentado_bc,
      modelo_segmentado_sazonalidade,
      modelo_segmentado_sazonalidade_bc,
      test = "Chi")


# Calcular os AICs dos modelos
AIC_mod_linear <- AIC(modelo)
AIC_mod_linear_bc <- AIC(modelo_bc)
AIC_mod_linear_sazon <- AIC(modelo2)
AIC_mod_linear_sazon_bc <- AIC(modelo2_bc)
AIC_modelo_segmentado <- AIC(modelo_segmentado)
AIC_modelo_segmentado_bc <- AIC(modelo_segmentado_bc)
AIC_modelo_segmentado_sazonalidade <- AIC(modelo_segmentado_sazonalidade)
AIC_modelo_segmentado_sazonalidade_bc <- AIC(modelo_segmentado_sazonalidade_bc)

# Criar um data frame com os resultados
resultados <- data.frame(
  Modelo = c("Linear","Linear BC",
             "Linear com Sazonalidade", "Linear com Sazonalidade BC",
             "Segmentado","Segmentado bc",
             "Segmentado com Sazonalidade",
             "Segmentado com Sazonalidade bc"),
  AIC = c(AIC_mod_linear,AIC_mod_linear_bc,
          AIC_mod_linear_sazon,AIC_mod_linear_sazon_bc,
           AIC_modelo_segmentado,AIC_modelo_segmentado_bc,
          AIC_modelo_segmentado_sazonalidade,AIC_modelo_segmentado_sazonalidade_bc)
)
resultados

#                           Modelo       AIC
# 1                         Linear 4283.0679
# 2                      Linear BC  807.4328
# 3        Linear com Sazonalidade 4266.9772
# 4     Linear com Sazonalidade BC  665.7049
# 5                     Segmentado 4087.1656
# 6                  Segmentado bc  789.6537
# 7    Segmentado com Sazonalidade 4067.1642
# 8 Segmentado com Sazonalidade bc  639.9975

# Conclusão: --------------------------------------------------------------
# Melhores modelos são os transformados - 
# Segmentado com sazonalidade são os melhores dentre os não
# transformados.






# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Opção 4. ARIMA Segmentada -----------------------------------------------
# Modelo Arima ------------------------------------------------------------

# Carregar os pacotes necessários
library(forecast)
library(segmented)



# Indicadores -------------------------------------------------------------
rd019 <- 
  dados_mensais |> 
  filter(Estacao== "RD019")
rd019 <- 
  rd019 |> 
  mutate(Impact = ifelse(Ano_mes == as.Date("2015-11-01"),1,0))

# ||||||||||| Criar indicadores
# Converter em ts
dados.ts <- ts(rd019[,"Turbidez"], frequency = 12, start = min(rd019$Ano_mes))

# indicadores:
step <- as.vector(rd019$BeforeAfter)
ramp <- as.vector(rd019$TimeSince)
impact <- as.vector(rd019$Impact)


# Auto.arima( ) -----------------------------------------------------------
# Auto.rima() - selecionar parâmetros automaticamente

#--- Com indicadores Externos
fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(step,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(ramp,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)
# Com step, ramp, impact  : Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors 
# Com step, ramp:           Best model: Regression with ARIMA(1,0,2)(0,0,2)[12] errors 
# Com step,impact           Best model: Regression with ARIMA(1,0,2)(1,0,1)[12] errors 
# Com ramp, impact:         Best model: Regression with ARIMA(0,0,3)(0,0,2)[12] errors 


#--- Com indicadores Externos e log()
fit_arima_indicador <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  xreg = cbind(step,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)

fit_arima_indicador <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  xreg = cbind(ramp,impact),
                                  stepwise = FALSE,
                                  trace = TRUE)
# Com step, ramp, impact  : Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 
# Com step, ramp:           Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 
# Com step,impact           Best model: Regression with ARIMA(2,0,0)(2,1,1)[12] errors 
# Com ramp, impact:         Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 


# Selecionar o melhor modelo auto.rima()
# Modelos
m1 <- astsa::sarima(rd019$Turbidez,1,0,2,P = 0, D = 0, Q = 2, S=12,
                    exreg = cbind(step, ramp))
m2 <- astsa::sarima(rd019$Turbidez,0,0,3, P = 0, D = 0, Q = 2, S=12,
                    exreg = cbind(impact, ramp))

# Comparando
# Modelo m2 teve o menor BIC
m1$BIC; m2$BIC;

# Conclusão: No gráfico ACF os modelos mostraram baixa autocorrelação dos 
# resíduos. O teste de Ljung-Box os resíduos não apresentam
# correlação. qqplot mostra que os resíduos não possuem distribuição 
# mto normal nas pontas

# Neste caso, vamos testar logaritmizar a variável Turbidez

m4 <- astsa::sarima(log(rd019$Turbidez),1,0,2,P = 0, D = 0, Q = 2, S=12,
                    exreg = cbind(step, ramp))
m5 <- astsa::sarima(log(rd019$Turbidez),0,0,3, P = 0, D = 0, Q = 2, S=12,
                    exreg = cbind(impact, ramp))
# Comparando
# Modelo m5 teve o menor BIC
m4$BIC; m5$BIC;

# Comparando os dois melhores
# Posso comparar o BIC de dois modelos em escala diferente dar var Y?
m1$BIC; m2$BIC;
m4$BIC; m5$BIC
# ou?

m1$BIC; m2$BIC;exp(m4$BIC); exp(m5$BIC)
# Conclusao: o Modelo m2 apresentou menor BIC



#---- Sem Indicadores externos
fit_arima_indicador <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  trace = TRUE)
fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  trace = TRUE)
# Sem Indicadores: Best model: ARIMA(2,0,0)(2,1,1)[12] with drift
# Sem log: Best model: Best model: ARIMA(1,1,2)   
m6 <- astsa::sarima(log(rd019$Turbidez),2,0,0, P = 2, D = 1, Q = 1, S=12)

m1$BIC; m2$BIC;exp(m4$BIC); exp(m5$BIC); exp(m6$BIC);



#---- Com transformação de BoxCox
lambda_BC <- car::powerTransform(rd019$Turbidez)
lambda_BC
rd019$bc_Turbidez <- (((rd019$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

dados.bc <- ts(rd019[,"bc_Turbidez"], frequency = 12, start = min(rd019$Ano_mes))

fit_arima_indicador <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(ramp,impact),
                                  trace = TRUE)

# Best model: Regression with ARIMA(1,0,0)            errors



# Conclusão: Usando o ARIMA automático, o Modelo m6 (foi log) foi com melhor 
# (com menor BIC). Esse modelo usou a transformação de BoxCox para
# normalizar a distribuição dos resíduos e diminui também a autocorrelação
# no teste de Ljung-Box. Apesar disso, o gráfico ACF ficou um pouco pior 
# do que outros modelos testados anteriormente


# ARIMA: Escolher os parâmetros Manualmente ----------------------------------

# --------------
# Autocorrelação
# Esse gráfico sugere um modelo autoregressivo
# Testar o valor de autoregressão
acf(rd019$Turbidez)
acf(rd019$bc_Turbidez)

modelo1 = astsa::sarima(rd019$Turbidez, 1,0,0)
modelo2 = astsa::sarima(rd019$Turbidez, 2,0,0)
modelo3 = astsa::sarima(rd019$Turbidez, 3,0,0)

# Melhor modelo de ordem 1
modelo1$BIC;modelo2$BIC;modelo3$BIC

modelo1 = astsa::sarima(rd019$Turbidez, 1,0,0,P=0,D=0, Q=0, S =12)
modelo1.1 = astsa::sarima(rd019$Turbidez, 1,0,0,P=1,D=0, Q=0, S =12)
modelo1.2 = astsa::sarima(rd019$Turbidez, 2,0,0, P=2,D=0, Q=0, S =12)
modelo1.3 = astsa::sarima(rd019$Turbidez, 2,0,0, P=3,D=0, Q=0, S =12)
# Melhor modelo modelo1 -  1,0,0, P=0,D=0, Q=0, S =12
modelo1$BIC;modelo1.1$BIC; modelo1.2$BIC;modelo1.3$BIC

# ------- Transformação BoxCox
modelo1_bc = astsa::sarima(rd019$bc_Turbidez, 1,0,0,xreg = cbind(ramp,step))
modelo1.1_bc = astsa::sarima(rd019$bc_Turbidez, 2,0,0,xreg = cbind(ramp,step))
modelo1.2_bc = astsa::sarima(rd019$bc_Turbidez, 3,0,0,xreg = cbind(ramp,step))
# Melhor modelo de ordem 2 - tem que transformar os valores para a escala
modelo1_bc$BIC;modelo1_bc$BIC;modelo1.1_bc$BIC; modelo1.2_bc$BIC

modelo2_bc = astsa::sarima(rd019$bc_Turbidez, 2,0,0,xreg = cbind(ramp,step))
modelo2.1_bc = astsa::sarima(rd019$bc_Turbidez, 2,0,0, P=1,D=0, Q=0, S =12,xreg = cbind(ramp,step))
modelo2.2_bc = astsa::sarima(rd019$bc_Turbidez, 2,0,0, P=2,D=0, Q=0, S =12,xreg = cbind(ramp,step))
modelo2.3_bc = astsa::sarima(rd019$bc_Turbidez, 2,0,0, P=3,D=0, Q=0, S =12,xreg = cbind(ramp,step))
# Melhor modelo de ordem 2 -  2,0,0, P=3,D=0, Q=0, S =12
modelo1_bc$BIC;modelo1_bc$BIC;modelo1.1_bc$BIC; modelo1.2_bc$BIC
modelo2_bc$BIC; modelo2.1_bc$BIC;modelo2.2_bc$BIC;modelo2.3_bc$BIC



# --------------
# Diferenciacao
# Parece que a diferenciação aumenta a autocorrelação na sazonal e na normal 
# 1 diferenciação é ok
# Então d = 1 e D = 0 
rd019 |> 
  features(Turbidez, feat_acf)
rd019 |> 
  features(Turbidez, feat_acf, .period=2)

#> Conclusão: Turbidez sem transformar (p,q, d=1)( P, Q, D=0)

# Na turbidez transformada por box-cox a diferenciação diminui a autocorrelação
# Escolho portanto, d = 1. Pois a ordem 2 a correlação volta a aumentar
rd019 |> 
  features(bc_Turbidez, feat_acf)

# Diferenciação de sazonalidade de ordem 3 diminui a autocorrelação e ordem 4
# diminui ainda mais. Mas note que com 1 diferenciação aumenta. Entao D = 3 OU D = 4
# ou zero 
rd019 |> 
  features(bc_Turbidez, feat_acf, .period=1)
rd019 |> 
  features(bc_Turbidez, feat_acf, .period=2)
rd019 |> 
  features(bc_Turbidez, feat_acf, .period=3)
rd019 |> 
  features(bc_Turbidez, feat_acf, .period=4)

#> Conclusão: Turbidez transformada (p,q, d=1)( P, Q, D=0ou3ou4)

# Teste de estacionaridade
summary(urca::ur.df(rd019$bc_Turbidez))# Não estacionario
summary(urca::ur.df(rd019$Turbidez)) # É estacionário



# Modelos -----------------------------------------------------------------

# ------- Sem Transformação 
mo12 = astsa::sarima(rd019$Turbidez, 1,1,0, P= 0,D=0, Q=0, S =12)
mo13 = astsa::sarima(rd019$Turbidez, 1,1,1, P= 0,D=0, Q=0, S =12)
mo14 = astsa::sarima(rd019$Turbidez, 1,1,2, P= 0,D=0, Q=0, S =12)
mo15 = astsa::sarima(rd019$Turbidez, 1,1,3, P= 0,D=0, Q=0, S =12)


# ------- Transformação BoxCox
# Melhor modelo da transf. com autoregressao p=2 P=3
mod16 = astsa::sarima(rd019$bc_Turbidez, 2,1,0, P=3,D=0, Q=0, S =12)
mod16.1 = astsa::sarima(rd019$bc_Turbidez, 2,0,0, P=3,D=0, Q=0, S =12)
# Colocando a difenrenciação
mod17 = astsa::sarima(rd019$bc_Turbidez, 2,1,0, P=3, D=3, Q=0, S =12,
                      xreg = cbind(ramp,step))
mod18 = astsa::sarima(rd019$bc_Turbidez, 2,1,0, P=3, D=4, Q=0, S =12,
                      xreg = cbind(step, ramp))

# Escolhendo o melhor q
mod19 = astsa::sarima(rd019$bc_Turbidez, 2,1,2, P=0, D=3, Q=1, S =12)
mod20 = astsa::sarima(rd019$bc_Turbidez, 2,1,3, P=0, D=3, Q=2, S =12)
mod21 = astsa::sarima(rd019$bc_Turbidez, 2,1,0, P=0, D=3, Q=3, S =12,
                      xreg = cbind(step, ramp))
mod16$BIC; mod16.1$BIC; mod17$BIC;mod18$BIC;mod19$BIC;mod20$BIC;mod21$BIC

#> Conclusão mod16.1:  ARIMA (2,0,0) (3,0,0) [12]
#> 
mod16$BIC; mod16.1$BIC; mod17$BIC; mod22$BIC;



# Comparando com os modelos automáticos
m5$BIC; mod16.1$BIC;


# MODELOS
# Automatico sem transformar
m2 <- astsa::sarima(rd019$Turbidez,0,0,3, P = 0, D = 0, Q = 2, S=12,
                    exreg = cbind(impact, ramp))
# Manual Transformado
mod16.1 = astsa::sarima(rd019$bc_Turbidez, 2,0,0, P=3,D=0, Q=0, S =12)






# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Opção 5. ARIMA Segmentada -----------------------------------------------
# Modelo Arima ------------------------------------------------------------


# Tentativa 1: ------------------------------------------------------------
# Ajustar os melhores parâmetros para o modelo para os dados totais mas com os 
# indicadores.

# Pacotes
library(forecast)
library(segmented)

# ||||||||||| Diferenciação
# ACF dados sem diferenciação
astsa::acf2(rd019$Turbidez, max.lag= 36)

# 1 ordem
astsa::acf2(diff(rd019$Turbidez), max.lag= 36)

# View ACF/PACF plots of differenced/seasonally differenced data
astsa::acf2(diff(diff(rd019$Turbidez,12)), max.lag=24)

# ||||||||||| Criar indicadores
rd019 <- 
  dados_mensais |> 
  filter(Estacao== "RD019")

rd019 <- 
  rd019 |> 
  mutate(Impact = ifelse(Ano_mes == as.Date("2015-11-01"),1,0))

step <- as.vector(rd019$BeforeAfter)
ramp <- as.vector(rd019$TimeSince)
impact <- as.vector(rd019$Impact)

# ||||||||||| Converter em ts
dados.ts <- ts(rd019[,"Turbidez"], frequency = 12, start = min(rd019$Ano_mes))


# ||||||||||| Auto.arima( ) - Com indicadores
# O impact não fez diferença na seleção então vou usar somente dois
# indicadores (step, ramp)
mod_arima1 <- auto.arima(dados.ts,
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp),
                                  stepwise = FALSE,
                                  trace = TRUE)

mod_arima2 <- auto.arima(log(dados.ts),
                                  seasonal = TRUE,
                                  xreg = cbind(step,ramp),
                                  stepwise = FALSE,
                                  max.d = 1,
                                  max.D = 1,
                                  trace = TRUE)
# Best model: Regression with ARIMA(1,0,1)(2,1,1)[12] errors 


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


#> Estimar Parâmetros
summary(mod_arima1)
confint(mod_arima1)

#> Estimar Parâmetros em log
summary(mod_arima2)
confint(mod_arima2)

# Resultado ---------------------------------------------------------------
# O coeficiente "step" é 4150.5544. Isso significa que após a interrupção 
# (no tempo em que "step" passa de 0 para 1), espera-se que a série aumente 
# em 4150.5544 unidades em média. Esta é uma mudança imediata na série no
# momento da interrupção.

# ramp é -58.9379, após o impacto a cada mês a turbidez diminui em média
# esse valor em unidades.



# Contrafactual -----------------------------------------------------------
# Modelar os dados excluindo o pós intervenção para o contrafactual
antes_impacto <- 
  rd019 |> 
  filter(Ano_mes<= as.Date("2015-10-01"))
antes_impacto.ts <-  ts(antes_impacto[,"Turbidez"],
                        frequency = 12,
                        start = min(antes_impacto$Ano_mes))
modelo_arima_antes <- Arima(window(antes_impacto.ts, end = c(2015,10)),
                            order = c(1,0,2),
                            seasonal = list (order = c(0,0,2),
                                             period = 12))

astsa::sarima(antes_impacto.ts, 1,0,2, P=0,D=0, Q=2, S =12)


# Resumo do modelo
summary(modelo_arima_antes)

# Forecast 12 months post-intervention and convert to time series object
fc <- forecast(modelo_arima_antes, h = 48)
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
















# plot(y = rd019$Turbidez, x = rd019$Time,
#      bty="n",
#      col = gray(0.5,0.5), pch=19,
#      #xlim = c(1, 365), 
#      #ylim = c(0, 300),
#      xlab = "Tempo", 
#      ylab = "Turbidez (NTU)")
# 
# lines( rep(1:tempo_antes_interrupcao), pred1[1:tempo_antes_interrupcao], col="dodgerblue4", lwd = 3 )
# lines( rep(interrupcao:obs_final), pred1[interrupcao:obs_final], col="dodgerblue4", lwd = 3 )
# lines( rep(interrupcao:obs_final), pred1.2[interrupcao:obs_final], col="darkorange2", lwd = 3, lty = 5 ) 
# 
# text(0, -80, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
# text(219, -80, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")
# 
# # Line marking the interruption
# abline( v=219, col="darkorange2", lty=2 )







# https://otexts.com/fpp3/arima-r.html

# Time series intervention analysis is not like this. First, you have to model regular ARIMA model for preintervention period and find the order of appropriate ARIMA model. Then you have to use the order in ARIMAX model (taking whole period, pre+post intervention), including a dummy variable.
# 
# model1 = forecast::auto.arima(ts_model1, trace = TRUE, seasonal = TRUE, 
#                               stationary = FALSE, ic = c("aicc", "aic", "bic"),
#                               stepwise = TRUE, allowmean = TRUE, allowdrift = TRUE)
# 
# # ts_model1 is only for preintervention period
# # Best model: ARIMA(1,1,1)(1,0,0)[12]
# 
# model2 = TSA::arimax(ts_model2, order = c(1, 1, 1),
#                      seasonal = list(order = c(1, 0, 0), period = 12),
#                      xreg = ts_model4, 
#                      method = c("CSS-ML", "ML", "CSS"),
#                      kappa = 1e+06, xtransf = dummy, transfer = list(c(1,0)))
# I hope you understand.


# Etapas ------------------------------------------------------------------
# A abordagem de modelação para LTF/Arima requer um processo em três etapas:

# 1. Identificar o processo da série temporal antes do impacto da intervenção.
#    o tipo de (p , q , d).
# 2. Acrescentar os efeitos de intervenção nos períodos de tempo adequados.
# 3. Verificar os resíduos para garantir que têm uma distribuição normal e
#    não apresentam valores anómalos.


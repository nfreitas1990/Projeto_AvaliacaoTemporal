source("scripts/03_decomposicao.R")

# Objetivo ----------------------------------------------------------------
# Esse script tem como objetivo Avaliar a magnitude das diferenças 
# pré e pós impacto na qualidade da água da bacia do rio doce usando o método
# de regressão segmentada

# Referencia: https://ds4ps.org/pe4ps-textbook/docs/p-020-time-series.html#fig:mixedeffect


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




# Função. Regressao Segmentada -------------------------------------------

graf_regressSegmentada <- function(estacao){

# Definir a fórmula  --------------------------
  form1 <- as.formula("Turbidez ~ Time") # só com tempo
  # BoxCox:
  form2 <- as.formula("bc_Turbidez ~ Time") # só com tempo
  
# Indicador de tempo "time" por estação
  dados_mensais <- 
    dados_mensais |> 
    group_by(Estacao) |> 
    arrange(Ano_mes) |> 
    group_by(Estacao) |> 
    mutate(Time= seq(from = 1, to = n(), by = 1))

# Separar os indicadores em vetores
BeforeAfter <- dados_mensais$BeforeAfter
TimeSince <- dados_mensais$TimeSince

# Fit Linear Model -----------------------------------------------------

# rd019
dados1 <- 
  dados_mensais |> 
  filter(Estacao == estacao)

# BoxCox: Transformar
#lambda_BC <- car::powerTransform(dados1$Turbidez)
#lambda_BC
#dados1$bc_Turbidez <- (((dados1$Turbidez ^ lambda_BC$lambda) - 1) / 
#                        lambda_BC$lambda)
dados1$bc_Turbidez <- log(dados1$Turbidez)

# Indicador de intervencao para a função its_lm
intervention_start_ind <- which (dados1$Ano_mes == as.Date("2015-11-01"))

fit1 <- its_lm(data = dados1,
               form = form1,
               time_name = "Time",
               intervention_start_ind = intervention_start_ind,
               freq = 12,
               seasonality = "full",
               impact_model = "full",
               counterfactual = TRUE,
               print_summary = FALSE)

# BoxCox: 
fit2 <- its_lm(data = dados1,
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

# BoxCox: Ajeitar os dados porque o gráfico nao estava funcionando
dados_plotar_bc<- fit2$data 
dados_plotar_bc <- 
  dados_plotar_bc|> 
  mutate(time = as.Date(Ano_mes))

# Gráfico -----------------------------------------------------------------
# Regressao segmentada com Sazonalidade
grafico <- 
dados_plotar |> 
  ggplot()+
  geom_point(aes(x = Ano_mes, y = Turbidez), color = "gray")+
  geom_line(aes(y= predC, x = Ano_mes),
            color = "orange",
            size = 1)+
  
  geom_line(aes(y = pred, x = Ano_mes))+
  geom_vline(xintercept = as.Date("2015-11-01"),
             linetype = "dashed", color = "red")+
  # annotate("text",y=3000)
  theme_few()

# Salvar o gráfico em uma pasta
nome_arquivo <- paste0(estacao, "regressaoSegmentadaSazonalidade.png")
caminho_arquivo <- file.path("figure/RegressaoSegmentada", nome_arquivo)

# ggsave(filename = caminho_arquivo,
#        plot = grafico,
#        dpi = 300,
#        width = 10, 
#        height = 8,
#        units = "cm")

# Salvar o gráfico como um objeto no ambiente
 nome_objeto <- paste0(estacao, "regSegmentada")
 assign(nome_objeto, grafico, envir = .GlobalEnv)

# Salvar tabela como um objeto no ambiente
 nome_objeto <- paste0(estacao, "modelo")
 assign(nome_objeto,fit1, envir = .GlobalEnv)
  
 
grafico




# BoxCox Gráfico -----------------------------------------------------------------
# Regressao segmentada com Sazonalidade
grafico_bc <- 
  dados_plotar_bc |> 
  ggplot()+
  geom_point(aes(x = Ano_mes, y = bc_Turbidez), color = "gray")+
  geom_line(aes(y= predC, x = Ano_mes),
            color = "orange",
            size = 1)+
  
  geom_line(aes(y = pred, x = Ano_mes))+
  geom_vline(xintercept = as.Date("2015-11-01"),
             linetype = "dashed", color = "red")+
  # annotate("text",y=3000)
  theme_few()

# Salvar o gráfico em uma pasta
nome_arquivo_bc <- paste0(estacao, "bc_regressaoSegmentadaSazonalidade.png")
caminho_arquivo <- file.path("figure/RegressaoSegmentada", nome_arquivo_bc)

# ggsave(filename = caminho_arquivo,
#        plot = grafico_bc,
#        dpi = 300,
#        width = 10, 
#        height = 8,
#        units = "cm")

# Salvar o gráfico como um objeto no ambiente
nome_objeto_bc <- paste0(estacao, "bc_regSegmentada")
assign(nome_objeto_bc, grafico_bc, envir = .GlobalEnv)

# Salvar tabela como um objeto no ambiente
nome_objeto_bc <- paste0(estacao, "bc_modelo")
assign(nome_objeto_bc,fit2, envir = .GlobalEnv)
}


# |||||||||||||||||||
# Aplicar
graf_regressSegmentada("RD019")

graf_regressSegmentada("RD083")

# Automatizar - vê o que está dando problema 
 graficos <- 
   purrr::set_names(unique(dados_mensais$Estacao)) |>
   purrr::map(graf_regressSegmentada)

unique(dados_mensais$Estacao)




# Graficos -----------------------------------------------------------------
RD019regSegmentada
RD023regSegmentada
RD033regSegmentada
RD035regSegmentada
RD044regSegmentada
RD045regSegmentada
RD053regSegmentada
RD058regSegmentada
RD059regSegmentada
RD067regSegmentada
RD071regSegmentada
RD072regSegmentada
RD083regSegmentada

# Diagnosticos ------------------------------------------------------------
acf(resid(RD019modelo$model))$acf[2]
qqnorm(resid(RD019modelo$model))
qqline(resid(RD019modelo$model))
summary(RD019modelo$model) #0.275


acf(resid(RD023modelo$model))$acf[2]
qqnorm(resid(RD023modelo$model))
qqline(resid(RD023modelo$model))
summary(RD023modelo$model) #0.218


acf(resid(RD033modelo$model))$acf[2]
qqnorm(resid(RD033modelo$model))
qqline(resid(RD033modelo$model))
summary(RD033modelo$model) #0.1436


acf(resid(RD035modelo$model))$acf[2]
qqnorm(resid(RD035modelo$model))
qqline(resid(RD035modelo$model))
summary(RD035modelo$model) #0.1431


acf(resid(RD044modelo$model))$acf[2]
qqnorm(resid(RD044modelo$model))
qqline(resid(RD044modelo$model))
summary(RD044modelo$model) #0.142 


acf(resid(RD045modelo$model))$acf[2]
qqnorm(resid(RD045modelo$model))
qqline(resid(RD045modelo$model))
summary(RD045modelo$model) #0.1543


acf(resid(RD053modelo$model))$acf[2]
qqnorm(resid(RD053modelo$model))
qqline(resid(RD053modelo$model))
summary(RD053modelo$model) #0.1083 


acf(resid(RD058modelo$model))$acf[2]
qqnorm(resid(RD058modelo$model))
qqline(resid(RD058modelo$model))
summary(RD058modelo$model) #0.1162


acf(resid(RD059modelo$model))$acf[2]
qqnorm(resid(RD059modelo$model))
qqline(resid(RD059modelo$model))
summary(RD059modelo$model) #0.1237 


acf(resid(RD067modelo$model))$acf[2]
qqnorm(resid(RD067modelo$model))
qqline(resid(RD067modelo$model))
summary(RD067modelo$model) #0.1239 


acf(resid(RD071modelo$model))$acf[2]
qqnorm(resid(RD071modelo$model))
qqline(resid(RD071modelo$model))
summary(RD071modelo$model) #0.09944


acf(resid(RD072modelo$model))$acf[2]
qqnorm(resid(RD072modelo$model))
qqline(resid(RD072modelo$model))
summary(RD072modelo$model) #0.1034 


acf(resid(RD083modelo$model))$acf[2]
qqnorm(resid(RD083modelo$model))
qqline(resid(RD083modelo$model))
summary(RD083modelo$model) #0.2493 

unique(dados_mensais$Estacao)


# Conclusão Sem Box Cox- Regressão Segmentada ----------------------------------------
# Não sei ao certo como interpretar os valores do summary. Os modelos
# tem um poder de explicação muito ruim. Os gráficos são interessantes, dá 
# para ver bem o contrafactual e a série projetada, bem como os dados originais
# por baixo em pontos cinzas. Mas ainda não me convence devido a baixa 
# explicação dos modelos. Variou entre 0.09 e 0.27. 


# Então resolvi acrescentar a transformação de box cox para ver se alterava
# alguma coisa no resulatdo


# BoxCox Diagnosticos ------------------------------------------------------------
library(kableExtra)
acf(resid(RD019bc_modelo$model))$acf[2]
qqnorm(resid(RD019bc_modelo$model))
qqline(resid(RD019bc_modelo$model))
summary(RD019bc_modelo$model) #0.6735

# Tenho que voltar para a escala original
# (((2.217 * -0.1035972 + 1)) ^ (1 / -0.1035972)) # 9.742872
# (((-0.01531 * -0.1035972 + 1)) ^ (1 / -0.1035972))#2.671836


acf(resid(RD023bc_modelo$model))$acf[2]
qqnorm(resid(RD023bc_modelo$model))
qqline(resid(RD023bc_modelo$model))
summary(RD023bc_modelo$model) #0.7193


acf(resid(RD033bc_modelo$model))$acf[2]
qqnorm(resid(RD033bc_modelo$model))
qqline(resid(RD033bc_modelo$model))
summary(RD033bc_modelo$model) #0.516


acf(resid(RD035bc_modelo$model))$acf[2]
qqnorm(resid(RD035bc_modelo$model))
qqline(resid(RD035bc_modelo$model))
summary(RD035bc_modelo$model) #0.4902


acf(resid(RD044bc_modelo$model))$acf[2]
qqnorm(resid(RD044bc_modelo$model))
qqline(resid(RD044bc_modelo$model))
summary(RD044bc_modelo$model) #0.5774 


acf(resid(RD045bc_modelo$model))$acf[2]
qqnorm(resid(RD045bc_modelo$model))
qqline(resid(RD045bc_modelo$model))
summary(RD045bc_modelo$model) #0.4895 


acf(resid(RD053bc_modelo$model))$acf[2]
qqnorm(resid(RD053bc_modelo$model))
qqline(resid(RD053bc_modelo$model))
summary(RD053bc_modelo$model) #0.4638  


acf(resid(RD058bc_modelo$model))$acf[2]
qqnorm(resid(RD058bc_modelo$model))
qqline(resid(RD058bc_modelo$model))
summary(RD058bc_modelo$model) #0.4942


acf(resid(RD059bc_modelo$model))$acf[2]
qqnorm(resid(RD059bc_modelo$model))
qqline(resid(RD059bc_modelo$model))
summary(RD059bc_modelo$model) #0.5239  


acf(resid(RD067bc_modelo$model))$acf[2]
qqnorm(resid(RD067bc_modelo$model))
qqline(resid(RD067bc_modelo$model))
summary(RD067bc_modelo$model) #0.5389 


acf(resid(RD071bc_modelo$model))$acf[2]
qqnorm(resid(RD071bc_modelo$model))
qqline(resid(RD071bc_modelo$model))
summary(RD071bc_modelo$model) #0.7021


acf(resid(RD072bc_modelo$model))$acf[2]
qqnorm(resid(RD072bc_modelo$model))
qqline(resid(RD072bc_modelo$model))
summary(RD072bc_modelo$model) #0.6851 


acf(resid(RD083bc_modelo$model))$acf[2]
qqnorm(resid(RD083bc_modelo$model))
qqline(resid(RD083bc_modelo$model))
summary(RD083bc_modelo$model) #0.7259

unique(dados_mensais$Estacao)
RD083bc_modelo$model

# Graficos -----------------------------------------------------------------
RD019bc_regSegmentada
RD023bc_regSegmentada
RD033bc_regSegmentada
RD035bc_regSegmentada
RD044bc_regSegmentada
RD045bc_regSegmentada
RD053bc_regSegmentada
RD058bc_regSegmentada
RD059bc_regSegmentada
RD067bc_regSegmentada
RD071bc_regSegmentada
RD072bc_regSegmentada
RD083bc_regSegmentada


# Conclusão - Regressão Segmentada com BoxCox -----------------------------
# Os modelos aumentaram a explicação, os gráficos ficaram mais condizentes,
# pois mostra que a variação na turbidez ainda não atingiu o contrafacutal.
# E mostra que isso é pior nas estações mais próximas do impacto.
# entretanto, os gráficos de acf mostram que existe autocorrealção nos dados
# e que seria necessário um modelo autoregressivo.



# Lambda ------------------------------------------------------------------
lambda <- car::powerTransform(rd019$Turbidez)
lambda
rd019$bc_Turbidez <- (((rd019$Turbidez ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

# Coeficientes estimados na escala transformada
ano_mes <- 0.1080
time <- -3.311e
indicator <- 0.8242
lambda <- -0.07798864
# Transformação inversa da Box-Cox
original_ano_mes <- ((ano_mes * lambda + 1)^(1 / lambda) - 1) / lambda
original_time <- ((time * lambda + 1)^(1 / lambda) - 1) / lambda
original_indicator <- ((indicator * lambda + 1)^(1 / lambda) - 1) / lambda








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

# Indicador de tempo "time" por estação
dados_mensais <- 
  dados_mensais |> 
  group_by(Estacao) |> 
  arrange(Ano_mes) |> 
  group_by(Estacao) |> 
  mutate(Time= seq(from = 1, to = n(), by = 1))


# 
# Modelagem ---------------------------------------------------------------


# Funcao
regress_linear <- function(estacao){


# Selecionar uma série temporal só = estação rd019
# rd019
dados1 <- 
  dados_mensais |> 
  filter(Estacao == estacao)

# Transformar
lambda_BC <- car::powerTransform(dados1$Turbidez)
lambda_BC
dados1$bc_Turbidez <- (((dados1$Turbidez ^ lambda_BC$lambda) - 1) / 
                        lambda_BC$lambda)

# Fit model
modelo <- lm(Turbidez ~ Time + BeforeAfter + TimeSince, data =dados1)
resultado <- summary(modelo)

modelo_bc <- lm(bc_Turbidez ~ Time + BeforeAfter + TimeSince, data =dados1)
resultado_bc <- summary(modelo_bc)

modelo_sazon <- lm(Turbidez ~ 
                Time +
                BeforeAfter +
                TimeSince +
                cos(2 * pi * (Time - 1)/12) +
                sin(2 * pi * (Time - 1)/12),
              data = dados1)
resultado_sazon <- summary(modelo_sazon)

modelo_sazon_bc <- lm(bc_Turbidez ~ 
                   Time +
                   BeforeAfter +
                   TimeSince +
                   cos(2 * pi * (Time - 1)/12) +
                   sin(2 * pi * (Time - 1)/12),
                 data =dados1)
resultado_sazon_bc <- summary(modelo_sazon_bc)


# Salvar tabela como um objeto no ambiente
nome_objeto1 <- paste0(estacao, "_TABmodelo_regSegLinear")
assign(nome_objeto1, resultado, envir = .GlobalEnv)

nome_objeto2 <- paste0(estacao, "_TABmodelo_regSegLinear_bc")
assign(nome_objeto2,resultado_bc, envir = .GlobalEnv)

nome_objeto3 <- paste0(estacao, "_TABmodelo_regSegLinear_sazon")
assign(nome_objeto3, resultado_sazon, envir = .GlobalEnv)

nome_objeto4 <- paste0(estacao, "_TABmodelo_regSegLinear_sazon_bc")
assign(nome_objeto4,resultado_sazon_bc, envir = .GlobalEnv)


# Gráfico Sem sazonalidade ---------------------------------------------------
grafico_semsazon <- 
  dados1 |> 
ggplot(aes(x = Time, y = Turbidez))+
  geom_point(col = "gray", shape = 19)+
  
  labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(dados1$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(dados1$Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = modelo$fitted.values), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(dados1$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()

# Gráfico Sem sazonalidade BOXCOX ---------------------------------------------------
grafico_semsazon_bc <- 
  dados1 |> 
  ggplot(aes(x = Time, y = bc_Turbidez))+
  geom_point(col = "gray", shape = 19)+
  
  labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(dados1$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(dados1$bc_Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = modelo_bc$fitted.values), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(dados1$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()

# Gráfico Com sazonalidade ---------------------------------------------------
grafico_comsazon <- 
  dados1 |> 
  ggplot(aes(x = Time, y = Turbidez))+
  geom_point(col = "gray", shape = 19)+
    labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(dados1$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(dados1$Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = modelo_sazon$fitted.values), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(dados1$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()

# Gráfico Com sazonalidade BOXCOX ---------------------------------------------------
grafico_comsazon_bc <- 
  dados1 |> 
  ggplot(aes(x = Time, y = bc_Turbidez))+
  geom_point(col = "gray", shape = 19)+
  
  labs(x = "Tempo", y = "Turbidez") +
  annotate("text", x = min(which(dados1$Ano_mes > as.Date("2015-10-01"))) + 2,
           y = max(dados1$bc_Turbidez), label = "Impacto",
           col = "firebrick", size = 4, hjust = 0)+
  geom_line(aes(y = modelo_sazon_bc$fitted.values), col = "steelblue", size = 1)+
  geom_vline(xintercept = min(which(dados1$Ano_mes > as.Date("2015-10-01"))),
             col = "firebrick", linetype = "dashed")+
  theme_few()


# Salvar o gráfico em uma pasta
nome_arquivo <- paste0(estacao, "regSegLinear_semsazon.png")
caminho_arquivo <- file.path("figure/RegressaoSegLinear", nome_arquivo)

ggsave(filename = caminho_arquivo,
       plot = grafico_semsazon,
       dpi = 300,
       width = 10, 
       height = 8,
       units = "cm")
#
nome_arquivo_bc <- paste0(estacao, "regSegLinear_semsazonBC.png")
caminho_arquivo <- file.path("figure/RegressaoSegLinear", nome_arquivo_bc)

ggsave(filename = caminho_arquivo,
       plot = grafico_semsazon_bc,
       dpi = 300,
       width = 10, 
       height = 8,
       units = "cm")

# COM SAZON
nome_arquivo <- paste0(estacao, "regSegLinear_comsazon.png")
caminho_arquivo <- file.path("figure/RegressaoSegLinear", nome_arquivo)

ggsave(filename = caminho_arquivo,
       plot = grafico_comsazon,
       dpi = 300,
       width = 10, 
       height = 8,
       units = "cm")
# COM SAZON bc
nome_arquivo_bc <- paste0(estacao, "regSegLinear_comsazonBC.png")
caminho_arquivo <- file.path("figure/RegressaoSegLinear", nome_arquivo_bc)

ggsave(filename = caminho_arquivo,
       plot = grafico_comsazon_bc,
       dpi = 300,
       width = 10, 
       height = 8,
       units = "cm")

# Salvar Grafio como um objeto no ambiente
nome_objeto <- paste0(estacao, "_GRAFregSegLinear_semsazon")
assign(nome_objeto, grafico_semsazon, envir = .GlobalEnv)

nome2_objeto <- paste0(estacao, "_GRAFregSegLinear_semsazon_bc")
assign(nome_objeto,grafico_semsazon_bc, envir = .GlobalEnv)

nome_objeto <- paste0(estacao, "_GRAFregSegLinear_comsazon")
assign(nome_objeto, grafico_comsazon, envir = .GlobalEnv)

nome2_objeto <- paste0(estacao, "_GRAFregSegLinear_comsazon_bc")
assign(nome_objeto,grafico_comsazon_bc, envir = .GlobalEnv)

}



# Modelar -----------------------------------------------------------------
# Automatizar - vê o que está dando problema 
graficos_regressLinear <- 
  purrr::set_names(unique(dados_mensais$Estacao)) |>
  purrr::map(regress_linear)

unique(dados_mensais$Estacao)


# Graficos ----------------------------------------------------------------
RD019_GRAFregSegLinear_semsazon
RD019_GRAFregSegLinear_semsazon_bc
RD019_GRAFregSegLinear_comsazon
RD019_GRAFregSegLinear_comsazon_bc

RD023_GRAFregSegLinear_semsazon
RD023_GRAFregSegLinear_semsazon_bc
RD023_GRAFregSegLinear_comsazon
RD023_GRAFregSegLinear_comsazon_bc

RD072_GRAFregSegLinear_semsazon
RD072_GRAFregSegLinear_semsazon_bc
RD072_GRAFregSegLinear_comsazon
RD072_GRAFregSegLinear_comsazon_bc



# Modelos -----------------------------------------------------------------
RD019_TABmodelo_regSegLinear
RD019_TABmodelo_regSegLinear_bc
RD019_TABmodelo_regSegLinear_sazon
RD019_TABmodelo_regSegLinear_sazon_bc















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


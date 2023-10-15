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
trimestral_pre <- 
  turbidez_trimestral_ts_inputNAs |> 
  filter(Trimestre <= yearquarter(as.Date("2015-10-01")))|> 
  mutate(BeforeAfter = 0,
         TimeSince = 0)

trimestral_pos <- 
  turbidez_trimestral_ts_inputNAs |> 
  filter(Trimestre > yearquarter(as.Date("2015-10-01"))) |> 
  mutate(BeforeAfter = 1) |> 
  group_by(Estacao) |> 
  mutate(TimeSince = seq(from = 1, to = n(), by = 1))

dados_trimestrais <- bind_rows(trimestral_pre, trimestral_pos)




# Função. Regressao Segmentada -------------------------------------------

graf_regressSegmentada_trimestral <- function(estacao){
  
  # Definir a fórmula  --------------------------
  form1 <- as.formula("Turbidez ~ Time") # só com tempo
  # BoxCox:
  form2 <- as.formula("bc_Turbidez ~ Time") # só com tempo
  
  # Indicador de tempo "time" por estação
  dados_trimestrais <- 
    dados_trimestrais |> 
    group_by(Estacao) |> 
    arrange(Trimestre) |> 
    group_by(Estacao) |> 
    mutate(Time= seq(from = 1, to = n(), by = 1))
  
  # Separar os indicadores em vetores
  BeforeAfter <- dados_trimestrais$BeforeAfter
  TimeSince <- dados_trimestrais$TimeSince
  
  # Fit Linear Model -----------------------------------------------------
  
  # rd019
  dados1 <- 
    dados_trimestrais |> 
    filter(Estacao == estacao)
  
  # BoxCox: Transformar
  #lambda_BC <- car::powerTransform(dados1$Turbidez)
  #lambda_BC
  #dados1$bc_Turbidez <- (((dados1$Turbidez ^ lambda_BC$lambda) - 1) / 
  #                        lambda_BC$lambda)
  dados1$bc_Turbidez <- log(dados1$Turbidez)
  
  # Indicador de intervencao para a função its_lm
  intervention_start_ind <- which (dados1$Trimestre == yearquarter(as.Date("2015-11-01")))
  
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
    mutate(time = yearquarter(as.Date(Trimestre)))
  
  # BoxCox: Ajeitar os dados porque o gráfico nao estava funcionando
  dados_plotar_bc<- fit2$data 
  dados_plotar_bc <- 
    dados_plotar_bc|> 
    mutate(time = yearquarter(as.Date(Trimestre)))
  
  # Gráfico -----------------------------------------------------------------
  # Regressao segmentada com Sazonalidade
  grafico <- 
    dados_plotar |> 
    ggplot()+
    geom_point(aes(x = Trimestre, y = Turbidez), color = "gray")+
    geom_line(aes(y= predC, x = Trimestre),
              color = "orange",
              size = 1)+
    
    geom_line(aes(y = pred, x = Trimestre))+
    geom_vline(xintercept = as.Date("2015-11-01"),
               linetype = "dashed", color = "red")+
    # annotate("text",y=3000)
    theme_few()
  
  # Salvar o gráfico em uma pasta
  nome_arquivo <- paste0(estacao, "regressaoSegmentadaSazonalidade.png")
  caminho_arquivo <- file.path("figure/RegressaoSegmentada_Trimestral", nome_arquivo)
  
   ggsave(filename = caminho_arquivo,
          plot = grafico,
          dpi = 300,
          width = 10, 
          height = 8,
        units = "cm")
  
  # Salvar o gráfico como um objeto no ambiente
  nome_objeto <- paste0(estacao, "regSegmentada_trimestral")
  assign(nome_objeto, grafico, envir = .GlobalEnv)
  
  # Salvar tabela como um objeto no ambiente
  nome_objeto <- paste0(estacao, "modelo_trimestral")
  assign(nome_objeto,fit1, envir = .GlobalEnv)
  
  
  grafico
  
  
  
  
  # BoxCox Gráfico -----------------------------------------------------------------
  # Regressao segmentada com Sazonalidade
  grafico_bc <- 
    dados_plotar_bc |> 
    ggplot()+
    geom_point(aes(x = Trimestre, y = bc_Turbidez), color = "gray")+
    geom_line(aes(y= predC, x = Trimestre),
              color = "orange",
              size = 1)+
    
    geom_line(aes(y = pred, x = Trimestre))+
    geom_vline(xintercept = as.Date("2015-11-01"),
               linetype = "dashed", color = "red")+
    # annotate("text",y=3000)
    theme_few()
  
  # Salvar o gráfico em uma pasta
  nome_arquivo_bc <- paste0(estacao, "bc_regressaoSegmentadaSazonalidade.png")
  caminho_arquivo <- file.path("figure/RegressaoSegmentada_Trimestral", nome_arquivo_bc)
  
   ggsave(filename = caminho_arquivo,
          plot = grafico_bc,
          dpi = 300,
          width = 10, 
          height = 8,
          units = "cm")
  
  # Salvar o gráfico como um objeto no ambiente
  nome_objeto_bc <- paste0(estacao, "bc_regSegmentada_trimestral")
  assign(nome_objeto_bc, grafico_bc, envir = .GlobalEnv)
  
  # Salvar tabela como um objeto no ambiente
  nome_objeto_bc <- paste0(estacao, "bc_modelo_trimestral")
  assign(nome_objeto_bc,fit2, envir = .GlobalEnv)
}


# |||||||||||||||||||
# Aplicar
graf_regressSegmentada_trimestral("RD019")

graf_regressSegmentada("RD083")

# Automatizar - vê o que está dando problema 
graficos <- 
  purrr::set_names(unique(dados_trimestrais$Estacao)) |>
  purrr::map(graf_regressSegmentada_trimestral)

unique(dados_mensais$Estacao)




# Graficos -----------------------------------------------------------------
RD019regSegmentada_trimestral
RD023regSegmentada_trimestral
RD033regSegmentada_trimestral
RD035regSegmentada_trimestral
RD044regSegmentada_trimestral
RD045regSegmentada_trimestral
RD053regSegmentada_trimestral
RD058regSegmentada_trimestral
RD059regSegmentada_trimestral
RD067regSegmentada_trimestral
RD071regSegmentada_trimestral
RD072regSegmentada_trimestral
RD083regSegmentada_trimestral

# Diagnosticos ------------------------------------------------------------
acf(resid(RD019modelo_trimestral$model))$acf[2]
qqnorm(resid(RD019modelo_trimestral$model))
qqline(resid(RD019modelo_trimestral$model))
summary(RD019modelo_trimestral$model) #0.275


acf(resid(RD023modelo_trimestral$model))$acf[2]
qqnorm(resid(RD023modelo_trimestral$model))
qqline(resid(RD023modelo_trimestral$model))
summary(RD023modelo_trimestral$model) #0.218


acf(resid(RD033modelo_trimestral$model))$acf[2]
qqnorm(resid(RD033modelo_trimestral$model))
qqline(resid(RD033modelo_trimestral$model))
summary(RD033modelo_trimestral$model) #0.1436


acf(resid(RD035modelo_trimestral$model))$acf[2]
qqnorm(resid(RD035modelo_trimestral$model))
qqline(resid(RD035modelo_trimestral$model))
summary(RD035modelo_trimestral$model) #0.1431


acf(resid(RD044modelo$model))$acf[2]
qqnorm(resid(RD044modelo_trimestral$model))
qqline(resid(RD044modelo_trimestral$model))
summary(RD044modelo_trimestral$model) #0.142 


acf(resid(RD045modelo_trimestral$model))$acf[2]
qqnorm(resid(RD045modelo_trimestral$model))
qqline(resid(RD045modelo_trimestral$model))
summary(RD045modelo_trimestral$model) #0.1543


acf(resid(RD053modelo_trimestral$model))$acf[2]
qqnorm(resid(RD053modelo_trimestral$model))
qqline(resid(RD053modelo_trimestral$model))
summary(RD053modelo_trimestral$model) #0.1083 


acf(resid(RD058modelo_trimestral$model))$acf[2]
qqnorm(resid(RD058modelo_trimestral$model))
qqline(resid(RD058modelo_trimestral$model))
summary(RD058modelo_trimestral$model) #0.1162


acf(resid(RD059modelo_trimestral$model))$acf[2]
qqnorm(resid(RD059modelo_trimestral$model))
qqline(resid(RD059modelo_trimestral$model))
summary(RD059modelo_trimestral$model) #0.1237 


acf(resid(RD067modelo_trimestral$model))$acf[2]
qqnorm(resid(RD067modelo_trimestral$model))
qqline(resid(RD067modelo_trimestral$model))
summary(RD067modelo_trimestral$model) #0.1239 


acf(resid(RD071modelo_trimestral$model))$acf[2]
qqnorm(resid(RD071modelo_trimestral$model))
qqline(resid(RD071modelo_trimestral$model))
summary(RD071modelo_trimestral$model) #0.09944


acf(resid(RD072modelo_trimestral$model))$acf[2]
qqnorm(resid(RD072modelo_trimestral$model))
qqline(resid(RD072modelo_trimestral$model))
summary(RD072modelo_trimestral$model) #0.1034 


acf(resid(RD083modelo_trimestral$model))$acf[2]
qqnorm(resid(RD083modelo_trimestral$model))
qqline(resid(RD083modelo_trimestral$model))
summary(RD083modelo_trimestral$model) #0.2493 

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
acf(resid(RD019bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD019bc_modelo_trimestral$model))
qqline(resid(RD019bc_modelo_trimestral$model))
summary(RD019bc_modelo_trimestral$model) #0.6735

# Tenho que voltar para a escala original
# (((2.217 * -0.1035972 + 1)) ^ (1 / -0.1035972)) # 9.742872
# (((-0.01531 * -0.1035972 + 1)) ^ (1 / -0.1035972))#2.671836


acf(resid(RD023bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD023bc_modelo_trimestral$model))
qqline(resid(RD023bc_modelo_trimestral$model))
summary(RD023bc_modelo_trimestral$model) #0.7193


acf(resid(RD033bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD033bc_modelo_trimestral$model))
qqline(resid(RD033bc_modelo_trimestral$model))
summary(RD033bc_modelo_trimestral$model) #0.516


acf(resid(RD035bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD035bc_modelo_trimestral$model))
qqline(resid(RD035bc_modelo_trimestral$model))
summary(RD035bc_modelo_trimestral$model) #0.4902


acf(resid(RD044bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD044bc_modelo_trimestral$model))
qqline(resid(RD044bc_modelo_trimestral$model))
summary(RD044bc_modelo_trimestral$model) #0.5774 


acf(resid(RD045bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD045bc_modelo_trimestral$model))
qqline(resid(RD045bc_modelo_trimestral$model))
summary(RD045bc_modelo_trimestral$model) #0.4895 


acf(resid(RD053bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD053bc_modelo_trimestral$model))
qqline(resid(RD053bc_modelo_trimestral$model))
summary(RD053bc_modelo_trimestral$model) #0.4638  


acf(resid(RD058bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD058bc_modelo_trimestral$model))
qqline(resid(RD058bc_modelo_trimestral$model))
summary(RD058bc_modelo_trimestral$model) #0.4942


acf(resid(RD059bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD059bc_modelo_trimestral$model))
qqline(resid(RD059bc_modelo_trimestral$model))
summary(RD059bc_modelo_trimestral$model) #0.5239  


acf(resid(RD067bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD067bc_modelo_trimestral$model))
qqline(resid(RD067bc_modelo_trimestral$model))
summary(RD067bc_modelo_trimestral$model) #0.5389 


acf(resid(RD071bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD071bc_modelo_trimestral$model))
qqline(resid(RD071bc_modelo_trimestral$model))
summary(RD071bc_modelo_trimestral$model) #0.7021


acf(resid(RD072bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD072bc_modelo_trimestral$model))
qqline(resid(RD072bc_modelo_trimestral$model))
summary(RD072bc_modelo_trimestral$model) #0.6851 


acf(resid(RD083bc_modelo_trimestral$model))$acf[2]
qqnorm(resid(RD083bc_modelo_trimestral$model))
qqline(resid(RD083bc_modelo_trimestral$model))
summary(RD083bc_modelo_trimestral$model) #0.7259

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

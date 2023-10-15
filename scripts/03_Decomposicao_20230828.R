

# Objetivos ---------------------------------------------------------------
# https://stats.stackexchange.com/search?q=%5Bforecasting%5D+Missing+Value+Imputation+for+Time+Series

# 1. Colocar Dados em Frequencia Mensal ---------------------------------
#    -> Era a frequencia que eu gostaria de usar para a modelagem
#       entretanto, existem alguns meses faltantes e temos que fazer
#       muitas imputações com interpolação. Então resolvi tentar a 
#       série com frequencia trimestral


# 2. Colocar Dados em Frequencia Trimestral -------------------------------
#    -> Possui menos imputações com interpolação. Temos a maioria dos dados
#       observados mesmo na série. Portanto, resolvi levar essa frequencia
#       a diante para ver se será mais fácil modelar


# 3. Completar os Dados faltantes -----------------------------------------
#    -> As análises  (e.g. STL) só funciona com a série completa de um ano
#       para o outro. Então não podemos ter meses faltantes. Temos que 
#       completar


# 4. Autoplot -------------------------------------------------------------
#    -> Visualzar as séries


# 5. Teste de estacionaridade ---------------------------------------------
#    -> Importante pois o modelo ARIMA requer que a série seja estacionaria para
#       ser aplciado, então, vamos verificar. Se não forem estacionarias teremos
#       que diferenciar os valores.


# 6. Autocorrelação -------------------------------------------------------
#    -> Usar o gglag para avaliar a autocorrelação entre as observações


# 7. ACF ------------------------------------------------------------------
#    -> Usar o ACF para ver o gráfico de autocorrelação e estacionaridade
#       antes e após a diferenciação da série temporal que precisa ser
#       diferenciada


# 8. Diferenciação --------------------------------------------------------
#    -> Dados com autocorrelação sazonal precisa ser diferenciado sazonalmente
#       Dados com autocorrelação precisa ser diferenciado de um lag para outro



# Pacotes -----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(tsibble)
library(tidyr)
library(plotly)
library(ggthemes)
library(lubridate)
library(timetk)
library(fabletools) 
library(feasts)
library(forecast)



# Tema --------------------------------------------------------------------
meu_tema <- 
  #theme_classic()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 12),
        axis.text.y = element_text(size= 12),
        axis.ticks = element_line(color = "grey"),
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "grey", fill = NA),
        legend.position = "top",
        legend.text = element_text(size = 12))


# Dados -------------------------------------------------------------------
turbidez <- read.csv("data/0_serie_dados_turbidez_completa.csv")
turbidez


# Ajeitando os dados 
turbidez <- 
  turbidez |> 
  mutate(Data_de_Amostragem = as.Date(Data_de_Amostragem),
         Ano_mes = yearmonth(Data_de_Amostragem),
         Ano = year(Data_de_Amostragem))|>
  select(-X, -Data) 

# Olhar
head(turbidez)
glimpse(turbidez)

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Objetivo 1: Série Temporal Mensal ---------------------------------------
# Colocar a série em uma frequencia mensal

# Selecionar colunas
turbidez_mensal <- 
  turbidez |>
  select (-Ano, -Data_de_Amostragem)
head(turbidez_mensal)

# média do parâmetro por estação e data
turbidez_mensal <-
  turbidez_mensal|>
  na.omit() |> 
  group_by(Estacao, Ano_mes)|> 
  summarise(Turbidez = mean(Turbidez)) 

# Escolhendo o index para criar uma tsibble
turbidez_mensal_ts <- 
  turbidez_mensal |> 
  as_tsibble(index = Ano_mes,
             key= c(Estacao))
# Olhar
turbidez_mensal_ts |> 
  arrange(Ano_mes) |> 
  head()

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --------
# Objetivo 2: Série Temporal Trimestral ----------------------------------
# Colocar a série em uma frequencia trimestral. Eu fiz isso porque percebi
# que quando coloco na frequencia mensal tenho que imputar muitos valores
# na série. Então trabalhar nessa frequencia é uma tentativa de imputar 
# menos valores por interpolação e ao mesmo tempo não perder tantos dados
# o que acontece quando faço média para trabalhar com os dados em frequencia
# anual (onde embora não precise interpolar nenhum valor, perdemos informações
# ao fazer a média anual de Turbidez).

# Selecionar colunas
turbidez_trimestral <- 
  turbidez |>
  select (-Ano, -Data_de_Amostragem)

# média do parâmetro por estação e data
turbidez_trimestral <-
  turbidez_trimestral|>
  mutate(Trimestre = yearquarter(Ano_mes)) |>
  group_by(Estacao, Trimestre)|> 
  summarise(Turbidez = mean(Turbidez)) |> 
  na.omit() 
head(turbidez_trimestral)

# Escolhendo o index
turbidez_trimestral_ts <- 
  turbidez_trimestral |> 
  as_tsibble(index = Trimestre,
             key= c(Estacao))
head(turbidez_trimestral_ts)



# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Objetivo 3: Completar a série Temporal ------------------------------------
# A decomposição STL não funciona se tiver dados faltantes na série.
# Então, primeiro vamos tornar os valores missing que antes estavam 
# implicito na série, em explicitos (fill_gaps). E depois imputadar 
# valores no lugar dos NAs (ts_impute_vec).

# Atenção! Esse preenchimento não funcionou para a série RD011
# que só tem dados após o impacto. Portanto, vou excluir a 
# RD011 das análises

# Optei por perid= 12 - para colocar a sazonalidade anual. 
#           lamba = "auto" - para seleção automatica com base nos dados

# Mensal ------------------------------------------------------------------
# fill_gaps
turbidez_mensal_ts_NAs <-
  fill_gaps(turbidez_mensal_ts)
View(turbidez_mensal_ts_NAs)

# ts_impute_vec
table(is.na(turbidez_mensal_ts_NAs)) # 1465 NAs
View(turbidez_mensal_ts_NAs)

turbidez_mensal_ts_inputNAs <- 
  turbidez_mensal_ts_NAs |>
  filter(Estacao != "RD011") |> 
  mutate(Turbidez = ts_impute_vec(Turbidez, period = 12, lambda = "auto"))


table(is.na(turbidez_mensal_ts_inputNAs)) # sem NAs
View(turbidez_mensal_ts_inputNAs)
autoplot(turbidez_mensal_ts_inputNAs)

# Olhar a quantidade de gaps a ser preenchido - trocar estacao para ver
# individualmente
unique(turbidez_mensal_ts_NAs$Estacao)
turbidez_mensal_ts_NAs |> 
  filter(Estacao=="RD045") |> 
  autoplot()

#qual mes comeca cada serie?
turbidez_mensal_ts_NAs |> 
  filter(Estacao=="RD045") |> 
mutate(max(Ano_mes),
       min(Ano_mes))

# Trimestral --------------------------------------------------------------
# fill_gaps
turbidez_trimestral_ts_NAs <-
  fill_gaps(turbidez_trimestral_ts)

# ts_impute_vec
table(is.na(turbidez_trimestral_ts_NAs)) # 33 valores NAs
View(turbidez_trimestral_ts_NAs)

turbidez_trimestral_ts_inputNAs <- 
  turbidez_trimestral_ts_NAs |>
  filter(Estacao != "RD011") |> 
  mutate(Turbidez = ts_impute_vec(Turbidez, period = 12, lambda = 1))
View(turbidez_trimestral_ts_inputNAs)
table(is.na(turbidez_trimestral_ts_inputNAs)) # sem valores NAs




# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Objetivo 4: Autoplot ----------------------------------------------------
# Conclusão: Dá para perceber o impacto e a sazonalidade na série. A série
# mensal apresenta picos menos acentuados do que a série trimestral. Vou 
# apresentar os gráficos da série mensal pq acho mais intuivo ver o ano
# que o impacto ocorreu.
autoplot(turbidez_mensal_ts)
autoplot(turbidez_trimestral_ts)


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Objetivo 5: Teste de estacionaridade  --------------------------------------
# Pressupostos da série temporal
# - Média constante
# - Variância constante
# - Covariancia

# Testar através da:
# 1. Avaliação visual (autoplot já vimos)
# 2. Diagrama de autocorrelação (ACF)
# 3. Teste de Estacionaridade

# Modelo Arima precisa que a série seja estacionaria. Se ela não for estacionaria
# devido a heterocedástica(mudança na variancia) podemos contornar com log. Se 
# for por conta do aumento ou diminuição da tendencia, podemos contornar aplican
# do a diferença


# Vou Separar a série em bloco para fazer as avaliações. Pois espero que depois
# do impacto a série se comporte de maneira diferente, então esse comportamento
# diferente pode prejudicar as análises de pressupostos. Acho que isso não trará
# problemas, pois a vamos modelar a série antes do impacto para fazer um 
# contrafactual para a série que existe.


# Separação de Blocos Pré e Pós Impacto -----------------------------------
# Bloco Mensal
mensal_pre <- 
  turbidez_mensal_ts_inputNAs |> 
  filter(Ano_mes <= as.Date("2015-11-01")) 
  
mensal_pos <- 
  turbidez_mensal_ts_inputNAs |> 
  filter(Ano_mes > as.Date("2015-11-01")) 

mensal_prepos <- turbidez_mensal_ts_inputNAs

# Visual
autoplot(mensal_pre)
autoplot(mensal_pos)


# Bloco Trimestral
trimestral_pre <- 
  turbidez_trimestral_ts_inputNAs |> 
  filter(Trimestre <= as.Date("2015-11-01")) 

trimestral_pos <- 
  turbidez_trimestral_ts_inputNAs |> 
  filter(Trimestre > as.Date("2015-11-01")) 

trimestral_prepos <- turbidez_trimestral_ts_inputNAs

# Visual
autoplot(trimestral_pre)
autoplot(mensal_pos)



# Teste de Dickey-Fuller  -------------------------------------------------
# Teste de Dickey-Fuller (mais usado)
# H0: A série Não é Estacionária
# H1: A série é Estacionária
library(urca)


# Mensal  -----------------------------------------------------------------
# Fazendo em blocos separados (pre e pos) ou fazendo tudo junto deu o mesmo
# resultado. Todas as estações, seja com dados em bloco ou total,
# tiveram p<0.05. Ou seja, são séries estacionárias. Não precisam diferenciar


# Vetor para o teste
gravar_vetor <- 
  function(estacao){
    temp <- 
      mensal_prepos |> 
      filter(Estacao== estacao)
    temp <- as.vector(temp$Turbidez)
    }

lista <- 
  purrr::set_names(unique(mensal_prepos$Estacao)) |>
  purrr::map(gravar_vetor)

# Teste de estacionaridade
summary(urca::ur.df(lista$RD019)) 
summary(urca::ur.df(lista$RD023)) 
summary(urca::ur.df(lista$RD033))
summary(urca::ur.df(lista$RD035))
summary(urca::ur.df(lista$RD044))
summary(urca::ur.df(lista$RD045))
summary(urca::ur.df(lista$RD053))
summary(urca::ur.df(lista$RD058))
summary(urca::ur.df(lista$RD059))
summary(urca::ur.df(lista$RD067))
summary(urca::ur.df(lista$RD071))
summary(urca::ur.df(lista$RD072))
summary(urca::ur.df(lista$RD083))
# Conclusão: Todas as estações, seja com dados em bloco (pre e pos) ou total (prepos),
# tiveram p<0.05. Sendo, portanto, consideradas estacionárias.

# Trimestral  -----------------------------------------------------------------
# A resposta do teste para o Trimestral deu, nos dados totais que todas as 
# séries são estacionarias. Ao separar em bloco, todas as pós impacto são 
# estacionarias, mas nem todas as pré impacto são estacionaria, essas precisam
# de diferenciação

# Vetor para o teste
gravar_vetor <- 
  function(estacao){
    temp <- 
      trimestral_pre |> 
      filter(Estacao== estacao)
    temp <- as.vector(temp$Turbidez)
    }

lista <- 
  purrr::set_names(unique(trimestral_pre$Estacao)) |>
  purrr::map(gravar_vetor)

# Teste de estacionaridade
summary(urca::ur.df(lista$RD019)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD023)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD033)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD035)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD044)) # <0.05
summary(urca::ur.df(lista$RD045)) # <0.05
summary(urca::ur.df(lista$RD053)) # <0.05
summary(urca::ur.df(lista$RD058)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD059)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD067)) # <0.05
summary(urca::ur.df(lista$RD071)) # <0.05
summary(urca::ur.df(lista$RD072)) # > 0.05 - NAO ESTACIONARIA
summary(urca::ur.df(lista$RD083)) # > 0.05 - NAO ESTACIONARIA
# Conclusão: Para os dados totais (pre e pós) e para o bloco (pós) todas
# estações são estacionárias. Quando olhamos os dados pré impacto muda (acima).
# A reposta foi diferente dependendo da estação. p<0.05 é estacionaria
# p> 0.05 NÃo é estacionária, precisa diferenciar a série para aplicar o Arima.
# Ou seja, o parâmetro d =1 no Arima (ou maior, caso precise diferenciar mais)


# Checar o N de Diferenciação ---------------------------------------------
# Para checar a ordem da diferenciação que precisamos podemos usar 
# ndiffs {forecast}
# Não entendi pq está dando tudo zero, como se não precisasse de diferenciação
# quando a série deu que não é estacionária
forecast::ndiffs(lista$RD019)
forecast::ndiffs(lista$RD023)
forecast::ndiffs(lista$RD033)
forecast::ndiffs(trimestral_pre[trimestral_pre$Estacao == "RD019",])


# Resumo Até aqui ---------------------------------------------------------
# Entendemos através do Dickey-Fuller se a série é estacionária. Conseguimos
# Ver o parâmetro d | D do modelo ARIMA.
# Agora vamos olhar o ACF e PACF para determinar as ordens potenciais para
# o autoregressivo (p P) e a Média Móvel (q Q)

# O modelo ARIMA (p , q , d) (P , Q , D)

# Diferenciação (d | D)
# Se a série não é estacionária, se existe tendencia, d = 1 para indicar 
# que queremos fazer uma diferenciação na série para torna-la estacionária
# Se a série possui sazonalidade, precisamos de uma diferenciação sazonal,
# logo D = 1.

#> Avaliando os dados mensais, o teste dick fuller deu que a série mensal é 
#> estacionaria, mesmo separando em blocos pre e pos impacto ou tratando a 
#> série toda ao mesmo tempo. O que nos indicaria um d = 0. Entretanto, 
#> olhando os gráficos percebeos que existe sazonalidade então D = 1 ou maior.

#> Avaliando os dados trimestrais, nem todas as estações apresentam estacionaridade
#> quando usamos o bloco pre impacto. Neste caso, teríamos um d = 1 ou superior
#> e como existe sazonalidade D = 1 ou maoior.





# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Objetivo 5: Autocorrelação ---------------------------------------------------
# A primeira tentativa eu fiz com os dados totais e não conseguia ver correlação
# entre os tempos. Usando os dados separados em blocos consegui ver melhor.
# Quando faço a mesma análise com os dados pós, não consigo observar correlação

# x gglag ( ) -------------------------------------------------------------
# Mensal ------------------------------------------------------------------
gglag <- 
  function(estacao){
    mensal_pre |>
      filter(Estacao == estacao) |> 
      gg_lag(y = Turbidez, lags = 1:12,geom = "point")} 

# dados pós
 # gglag <- 
 #   function(estacao){
 #     mensal_pos |>
 #       filter(Estacao == estacao) |> 
 #       gg_lag(y = Turbidez, lags = 1:12,geom = "point")} 

grafico <- 
  purrr::set_names(unique(mensal_pre$Estacao)) |>
  purrr::map(gglag)

grafico$RD019 
grafico$RD023
grafico$RD033
grafico$RD035
grafico$RD044
grafico$RD045
grafico$RD053
grafico$RD058
grafico$RD059
grafico$RD067
grafico$RD071
grafico$RD072
grafico$RD083

#> Conclusao: Dá para ver que existe autocorrelação entre as observações de um tempo
#> para o outro. Então modelos regressivos padrão não terão o pressuposto de 
#> idenpendencia satisfeitos.

# Trimestral ------------------------------------------------------------------
gglag_tri <- 
  function(estacao){
    trimestral_prepos |>
      filter(Estacao == estacao) |> 
      gg_lag(y = Turbidez, lags = c(1:12),geom = "point")} 

grafico_trimestral <- 
  purrr::set_names(unique(trimestral_prepos$Estacao)) |>
  purrr::map(gglag_tri)

grafico_trimestral$RD019 
grafico_trimestral$RD023
grafico_trimestral$RD033
grafico_trimestral$RD035
grafico_trimestral$RD044
grafico_trimestral$RD045
grafico_trimestral$RD053
grafico_trimestral$RD058
grafico_trimestral$RD059
grafico_trimestral$RD067
grafico_trimestral$RD071
grafico_trimestral$RD072
grafico_trimestral$RD083

#> Conclusao: Ficou complicado enxergar autocorrelação nesses dados pré impacto


# x ACF ( ) -------------------------------------------------------------
# Constroir a função da autocorrelação para ver se existe padrão no
# comportamento da variável. Valores dentro do intervalo de confiança é
# igual a zero, fora do intervalo, diferente de zero.

# Mensal - Pre
acf_pre <- 
  function(estacao){mensal_pre |>
      filter(Estacao == estacao) |> 
      ACF(y = Turbidez,lag_max = 36) |> 
      autoplot() 
  }

ACF_mensal_pre <- 
  purrr::set_names(unique(mensal_pre$Estacao)) |>
  purrr::map(acf_pre)



# Mensal Pos
acf_pos <- 
  function(estacao){mensal_pos |>
      filter(Estacao == estacao) |> 
      ACF(y = Turbidez, lag_max = 48) |> 
      autoplot() 
  }

ACF_mensal_pos <- 
  purrr::set_names(unique(mensal_pre$Estacao)) |>
  purrr::map(acf_pos)

# Mensal Total
acf_prepos <- 
  function(estacao){mensal_prepos |>
      filter(Estacao == estacao) |> 
      ACF(y = Turbidez, lag_max = 36) |> 
      autoplot() 
  }

ACF_mensal_prepos <- 
  purrr::set_names(unique(mensal_prepos$Estacao)) |>
  purrr::map(acf_prepos)


ACF_mensal_prepos$RD019 # autocorrelaçao em três lags | Sazonalidade
ACF_mensal_pre$RD019 
ACF_mensal_pos$RD019 

ACF_mensal_prepos$RD023 # Só sazonalidade
ACF_mensal_pre$RD023
ACF_mensal_pos$RD023

ACF_mensal_prepos$RD033
ACF_mensal_pre$RD033
ACF_mensal_pos$RD033

ACF_mensal_prepos$RD035
ACF_mensal_pre$RD035
ACF_mensal_pos$RD035

ACF_mensal_prepos$RD044
ACF_mensal_pre$RD044
ACF_mensal_pos$RD044

ACF_mensal_prepos$RD045
ACF_mensal_pre$RD045
ACF_mensal_pos$RD045

ACF_mensal_prepos$RD053
ACF_mensal_pre$RD053
ACF_mensal_pos$RD053

ACF_mensal_prepos$RD058
ACF_mensal_pre$RD058
ACF_mensal_pos$RD058

ACF_mensal_prepos$RD059
ACF_mensal_pre$RD059
ACF_mensal_pos$RD059

ACF_mensal_prepos$RD067
ACF_mensal_pre$RD067
ACF_mensal_pos$RD067

ACF_mensal_prepos$RD071
ACF_mensal_pre$RD071
ACF_mensal_pos$RD071

ACF_mensal_prepos$RD072
ACF_mensal_pre$RD072
ACF_mensal_pos$RD072

ACF_mensal_prepos$RD083
ACF_mensal_pre$RD083
ACF_mensal_pos$RD083


# Mensal - Pre
acf_pre_2 <- 
  function(estacao){mensal_pre |>
      filter(Estacao == estacao) |> 
      mutate(Turbidez_lag = diff(Turbidez)) |> 
      ACF(y = Turbidez,lag_max = 36) |> 
      autoplot() 
  }



# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Diferenciação---------------------------------------------------------------

# ACF - com as diferenciações
# Conclusao: dados pre: A diferenciação seria melhor para todas estações,pois 
#            diminui a autocorrelação entre as observações. Somente na 
#            estação 23 que a diferenciação aumenta a autocorrelação.
#            Nos dados totais, prepos, assim como nos dados pós varia de acordo
#            com a estação
mensal_pre |> 
  features(Turbidez, feat_acf)
mensal_pos |> 
  features(Turbidez, feat_acf)
mensal_prepos |> 
  features(Turbidez, feat_acf)

# STL - com as diferenciações
# Conclusao: 
# seasonal_peak_year indicates the timing of the peaks — which month or 
# quarter contains the largest seasonal component. This tells us something
# about the nature of the seasonality
mensal_pre |> 
  features(Turbidez, feat_stl)
mensal_pos |> 
  features(Turbidez, feat_acf)
mensal_prepos |> 
  features(Turbidez, feat_acf)





# Resumo Até Aqui ---------------------------------------------------------
# As séries possuem autocorrelação e apresentam sazonalidade. Neste caso, 
# regressões padrões não seriam as mais indicadas para construir os modelos




# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ----------
# Decomposição STL --------------------------------------------------------
# Separei a decomposição em pré e pós impacto. A decomposição foi feita com 
# os dados separados. Mas o gráfico foi feito em conjunto, com as duas análises
# pré e pós impacto para facilitar a comparação antes e depois 

# Nome das Estações
nome_estacoes <- unique(turbidez_mensal_ts_inputNAs$Estacao)


# |||||||||||||||||||||||||||||||||||||||||||||||||||| --------------------
# Sazonalidade Antes vs Depois --------------------------------------------
# Gráficos de Sazonalidade - Decomposição STL feita com dados pré e pós 
# impacto. Pré e Pós impacto foi feito separado e posteriormente foram
# unidas em um único gráfico
stl_season <- function(estacao, cor) {
  stl_components_antes <- turbidez_mensal_ts_inputNAs %>%
    filter(Estacao == estacao) %>%
    filter(Ano_mes <= as.Date("2015-10-01")) |> 
    na.omit() %>%
    model(STL(Turbidez ~ season(12) + trend())) %>%
    components()
  stl_components_depois <- turbidez_mensal_ts_inputNAs %>%
    filter(Estacao == estacao) %>%
    filter(Ano_mes > as.Date("2015-10-01")) |> 
    na.omit() %>%
    model(STL(Turbidez ~ season(12) + trend())) %>%
    components()
  stl_components <- bind_rows(stl_components_antes,stl_components_depois)
  
  # trecho novo para add smooth
  stl_components <- 
    stl_components |> 
    mutate(Periodo = if_else(Ano_mes <= as.Date("2015-10-31"), "antes","depois"))
  unique(stl_components$Periodo)
  
  plot <- stl_components |> 
    ggplot(aes(y = season_12, x = as.Date(Ano_mes), color = Estacao))+
    geom_line()+
    scale_x_date(date_labels = "%Y",
                 date_breaks = "24 month",
                 limits = as.Date(c("1998-01-01", "2022-12-31")))+
    scale_y_continuous(limits = c( -800, 2800),
                       breaks = seq(-800, 2800, 400),
                       labels = seq(-800, 2800, 400))+
    geom_vline(xintercept = as.Date("2015-09-01"),
               linetype = "dashed", color = "red")+
    # geom_smooth(aes(y = season_12, x = as.Date(Ano_mes),
    #                 group = Periodo,
    #                 color = Periodo),
    #             method = "glm")+
    scale_color_manual(values = cor)+
    meu_tema+
    theme(axis.text.y = element_text(size= 12),
          axis.text.x = element_text(size= 12),
          legend.position = "none")+
    xlab("Time")+
    ylab("Season")
  
  #facet_wrap(~ Estacao, ncol = 4)+
  #ggraph::scale_color_viridis(discrete = TRUE)
  
  # Salvar o gráfico em uma pasta
  #nome_arquivo <- paste0(estacao, "AntesDepois_stl_season.png")
  #caminho_arquivo <- file.path("figure/STL_season", nome_arquivo)
  
  # ggsave(filename = caminho_arquivo,
  #        plot = plot,
  #        dpi = 300,
  #        width = 10, 
  #        height = 8,
  #        units = "cm")
  # 
  # Salvar o gráfico como um objeto no ambiente
   nome_objeto <- paste0(estacao, "_stl_plot")
   assign(nome_objeto, plot, envir = .GlobalEnv)
  
  # # Salvar tabela como um objeto no ambiente
   nome_objeto <- paste0(estacao, "_stl")
   assign(nome_objeto,stl_components , envir = .GlobalEnv)
  # 
  return(stl_components)}

# Gráficos de Maneira MANUAL para ajustar as cores e compatibilizar com os 
# outros gráficos;
stl_season("RD019","#e38900")
stl_season("RD023","#c39900")
stl_season("RD033","#93a400")
stl_season("RD035","#52b300")
stl_season("RD044","#00bb54")
stl_season("RD045","#00c094")
stl_season("RD053","#00bfc4")
stl_season("RD058","#00bbda")
stl_season("RD059","#00acfc")
stl_season("RD067","#8b93ff")
stl_season("RD071","#d575fe")
stl_season("RD072","#f962dd")
stl_season("RD083","#f061a7")

# Rodar para todas as estações automático
# Neste caso não consegui ajustar a cor
# graf_stl_sazon_trend <- purrr::map(nome_estacoes,stl_sazon_trend)
# graf_stl_sazon_trend


# |||||||||||||||||||||||||||||||||||||||||||||||||||| --------------------
# Tendencia Antes vs Depois -----------------------------------------------
# Aqui está ajustado para apenas um estação.
# Aqui tenho que trocar manualmente as estações. 
# Fiz isso para conseguir colocar a cor de cada estação conforme os outros
# gráficos
# Gráficos de Tendência - Decomposição STL feita com dados pré e pós 
# impacto. Pré e Pós impacto foi feito separado e posteriormente foram
# unidas em um único gráfico

nome_estacoes <- unique(turbidez_mensal_ts_inputNAs$Estacao)

stl_trend <- function(estacao, cor) {
  stl_components_antes <- turbidez_mensal_ts_inputNAs %>%
    filter(Estacao == estacao) %>%
    filter(Ano_mes <= as.Date("2015-10-01")) |> 
    na.omit() %>%
    model(STL(Turbidez ~ season(12) + trend())) %>%
    components()
  stl_components_depois <- turbidez_mensal_ts_inputNAs %>%
    filter(Estacao == estacao) %>%
    filter(Ano_mes > as.Date("2015-10-01")) |> 
    na.omit() %>%
    model(STL(Turbidez ~ season(12) + trend())) %>%
    components()
  stl_components <- bind_rows(stl_components_antes,stl_components_depois)
  
  # trecho novo para add smooth
  stl_components <- 
    stl_components |> 
    mutate(Periodo = if_else(Ano_mes <= as.Date("2015-10-31"), "antes","depois"))
  unique(stl_components$Periodo)
  
  plot <- stl_components |> 
    ggplot(aes(y = trend, x = as.Date(Ano_mes), color = Estacao))+
    geom_line()+
    scale_x_date(date_labels = "%Y",
                 date_breaks = "24 month",
                 limits = as.Date(c("1998-01-01", "2022-12-31")))+
    scale_y_continuous(limits = c( -800, 2800),
                       breaks = seq(-800, 2800, 400),
                       labels = seq(-800, 2800, 400))+
    geom_vline(xintercept = as.Date("2015-09-01"),
               linetype = "dashed", color = "red")+
    geom_smooth(aes(y = trend, x = as.Date(Ano_mes),
                    group = Periodo,
                    color = Periodo),
                method = "glm")+
    scale_color_manual(values = c("gray","gray",cor))+
    meu_tema+
    theme(axis.text.y = element_text(size= 12),
          axis.text.x = element_text(size= 12),
          legend.position = "none")+
    xlab("Time")+
    ylab("Trend")
  
  #facet_wrap(~ Estacao, ncol = 4)+
  #ggraph::scale_color_viridis(discrete = TRUE)
  
  # Salvar o gráfico em uma pasta
  nome_arquivo <- paste0(estacao, "AntesDepois_stl_trend.png")
  caminho_arquivo <- file.path("figure/STL_trend", nome_arquivo)
  
  ggsave(filename = caminho_arquivo,
         plot = plot,
         dpi = 300,
         width = 10, 
         height = 8,
         units = "cm")
  
  # Salvar o gráfico como um objeto no ambiente
  # nome_objeto <- paste0(estacao, "_stl_plot")
  # assign(nome_objeto, plot, envir = .GlobalEnv)
  
  # # Salvar tabela como um objeto no ambiente
  # nome_objeto <- paste0(estacao, "_stl")
  # assign(nome_objeto,stl_components , envir = .GlobalEnv)
  # 
  return(stl_components)}

stl_trend("RD019","#e38900")
stl_trend("RD023","#c39900")
stl_trend("RD033","#93a400")
stl_trend("RD035","#52b300")
stl_trend("RD044","#00bb54")
stl_trend("RD045","#00c094")
stl_trend("RD053","#00bfc4")
stl_trend("RD058","#00bbda")
stl_trend("RD059","#00acfc")
stl_trend("RD067","#8b93ff")
stl_trend("RD071","#d575fe")
stl_trend("RD072","#f962dd")
stl_trend("RD083","#f061a7")



# Força Sazonalidade ------------------------------------------------------
forca_depois <- 
  turbidez_mensal_ts_inputNAs |> 
  # filter(Estacao == "RD019") |> 
  filter(Ano_mes > as.Date("2015-11-01")) |> 
  na.omit() |> 
  features(Turbidez, feat_stl) |> 
  mutate(status = "depois")
forca_antes <- 
  turbidez_mensal_ts_inputNAs |> 
  # filter(Estacao == "RD019") |> 
  filter(Ano_mes <= as.Date("2015-11-01")) |> 
  na.omit() |> 
  features(Turbidez, feat_stl) |> 
  mutate(status = "antes")

forca_juntos <- bind_rows(forca_antes,forca_depois)

forca_juntos |> 
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Estacao)) +
  geom_point()+
  facet_wrap(vars(status))

forca_juntos |>
  ggplot(aes(x = status, y = trend_strength))+
  geom_boxplot()



# Forecast ----------------------------------------------------------------
# feat_spectral will compute the (Shannon) spectral entropy of a time series,
# which is a measure of how easy the series is to forecast. A series which has
# strong trend and seasonality (and so is easy to forecast) will have entropy 
# close to 0. A series that is very noisy (and so is difficult to forecast) 
# will have entropy close to 1.
forca_depois <- 
  turbidez_mensal_ts_inputNAs |> 
  # filter(Estacao == "RD019") |> 
  filter(Ano_mes > as.Date("2015-10-01")) |> 
  na.omit() |> 
  features(Turbidez, feat_spectral) |> 
  mutate(status = "depois")
forca_antes <- 
  turbidez_mensal_ts_inputNAs |> 
  # filter(Estacao == "RD019") |> 
  filter(Ano_mes <= as.Date("2015-10-01")) |> 
  na.omit() |> 
  features(Turbidez, feat_spectral) |> 
  mutate(status = "antes")

forca_juntos <- bind_rows(forca_antes,forca_depois)

forca_juntos |> 
  ggplot(aes(x = status, y = spectral_entropy)) +
  geom_boxplot()+
  geom_point(aes(color= Estacao))



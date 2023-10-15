

# Carregamento de Scripts dependentes -------------------------------------
source("scripts/01_carregamento_selecao_estacoes_parametrosFQ.R")



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

# Dados -------------------------------------------------------------------
dados_monitoramento <- read.csv("dados_monitoramento.csv")



# Tema --------------------------------------------------------------------
meu_tema <- theme_classic()+
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

# Monitoramento -----------------------------------------------------------
# Dados totais
dados_monitoramento_turbidez <- 
  dados_monitoramento |> 
  select(Estacao,Data_de_Amostragem, Ano, Turbidez) 
head(dados_monitoramento_turbidez)

# Gráfico exploratório
  dados_monitoramento_turbidez |> 
    mutate(Data = as.Date(Data_de_Amostragem))  |>
    ggplot() +
    geom_line(aes(x = Data, y = Turbidez, color = "série")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")+
    facet_wrap(~Estacao)+
    theme_classic()
    

# # Criar tsibble  
# head(dados_monitoramento_turbidez)
# dados_turbidez <- ts(dados_monitoramento_turbidez[4])  
# plot(dados_turbidez)
# 
# # Desse jeito ele entende cada coluna como uma série diferente
# dados_monitoramento_tb <- ts(dados_monitoramento_turbidez)
# class(dados_monitoramento_tb)
# head(dados_monitoramento_tb)
# 
# # Visualização
# plot(dados_monitoramento_tb)


# Turbidez ----------------------------------------------------------------
dados_turbidez <- dados_monitoramento |> 
  select(Estacao,Data_de_Amostragem, Ano, Turbidez)



# Estação:  ---------------------------------------------------------------
 table(dados_turbidez$Estacao)
# RD011 RD019 RD023 RD033 RD035 RD044 RD045 RD053 RD058 RD059 RD067 RD071 RD072 RD083 
#  66   165   163   165   158   157   165   163   156   165   163    98   121   122



# Trabalhar as Datas ------------------------------------------------------
# 1. Colocar uma coluna de ano-mes para plotar o gráfico de autoplot
head(dados_turbidez)
str(dados_turbidez)

# Separando coluna com ano e mes
dados_turbidez <- 
  dados_turbidez |> 
  mutate(Data_de_Amostragem =
           as.Date(Data_de_Amostragem,
                   tryFormats = "%Y-%m-%d"),
         Data = yearmonth(Data_de_Amostragem))

plot(dados_turbidez)

# Analise: Séries Temporais -----------------------------------------------

# 1. Avisar que é série Temporal: Colocando tibble

# Criar uma tibble 

# Selecionar colunas
dados_turbidez_tb <- 
  dados_turbidez |>
  select (-Ano, -Data_de_Amostragem)

# colocar no formato longo
dados_turbidez_tb <- 
  dados_turbidez_tb |> 
  pivot_longer(cols = "Turbidez",
               names_to = "Parametros")

# Visualizar
View(dados_turbidez_tb)
table(is.na(dados_turbidez_tb))

# média do parâmetro por estação e data
dados_turbidez_tb <-
  dados_turbidez_tb|>
  group_by(Estacao, Data, Parametros)|> 
  summarise(value = mean(value)) |> 
  na.omit()

# Escolhendo o index
dados_turbidez_tb <- 
  dados_turbidez_tb |> 
  as_tsibble(index = Data,
             key= c(Parametros, Estacao))

str(dados_turbidez_tb)
glimpse(dados_turbidez_tb)
class(dados_turbidez_tb)


# Análise Descritiva ------------------------------------------------------
summary(dados_turbidez_tb$value)
boxplot(value ~ Data, dados_turbidez_tb)


# Gráfico por estacao--------------------------------------------------------
head(dados_turbidez)
figura_estacao <- 
dados_turbidez |> 
ggplot()+
  geom_line(aes(Data_de_Amostragem, Turbidez, color = Estacao))+
  facet_wrap(~ Estacao, ncol = 4,scales = "free_y")+
  scale_x_date(date_labels = "%Y",
               date_breaks = "3 year")+
  xlab("Período")+
  ylab("Turbidez")+
  geom_vline(xintercept = as.Date("2015-01-01"),
             linetype = "dashed", color = "red")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        axis.ticks = element_line(color = "grey"),
        axis.ticks.length= unit(.2, "cm"),
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey90",
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "grey", fill = NA),
        legend.position = "none")
figura_estacao
# #ggsave(path = "figure/",
#        filename = "figura_todasestacaoes_scalefree.tiff",
#        plot = figura_estacao,
#        device = "tiff",
#        dpi = 500,
#        width = 9.00,
#        height = 6.5,
#        units = "in")


# Grafico Estação Sobreposta ----------------------------------------------
figura_estacao_sobreposta <- 
  dados_turbidez |> 
  ggplot()+
  geom_line(aes(Data_de_Amostragem, Turbidez, color = Estacao))+
  #facet_wrap(~ Estacao, ncol = 4)+
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year")+
  xlab("Período")+
  ylab("Turbidez")+
  geom_vline(xintercept = as.Date("2015-01-01"),
             linetype = "dashed", color = "red")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        axis.ticks = element_line(color = "grey"),
        axis.ticks.length= unit(.2, "cm"),
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "white",
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "grey", fill = NA),
        legend.position = "right")
figura_estacao_sobreposta
# ggsave(path = "figure/",
#        filename = "figura_estacao_sobreposta.tiff",
#        plot = figura_estacao_sobreposta,
#        device = "tiff",
#        dpi = 500)







# Autoplot ----------------------------------------------------------------
autoplot_todas <-   
  autoplot(dados_turbidez_tb)+
  theme_classic()+
  facet_wrap(~ Estacao)+
  xlab("Período")+
  ylab("Turbidez")+
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        axis.ticks = element_line(color = "grey"),
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey90",
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "grey", fill = NA),
        legend.position = "none")

autoplot_todas

#ggsave(path = "figure/",
#       filename = "autoplot_todasestacaoes.jpg",
 #      plot = autoplot_todas,
 #      device = "jpeg",
#       dpi = 300)



# Seleção das Estações: Gráfico exploratório

explorar_longotempo <- 
  function(estacao){
 tabela <-  
   dados_turbidez |>
   filter(Estacao== estacao) |>
   select(Estacao,Data_de_Amostragem,Turbidez) |> 
   mutate(Data_de_Amostragem = as.Date(Data_de_Amostragem)) |> 
   mutate(Data = yearmonth(Data_de_Amostragem))

 tabela_pivot <- 
   tabela |> 
   pivot_longer(cols = "Turbidez",
                names_to = "parametros")

 grafico <- tabela_pivot |> 
  as_tsibble(index = Data,
             key= parametros)
print(grafico)

grafico_exportar <- 
  grafico |> 
  feasts::autoplot(col= "darkblue")+
  geom_vline(xintercept = 2015, color = "red", linetype = "dashed")+
  xlab("Período")+
  ylab("Turbidez")+
  theme_clean()
print(grafico_exportar)
}

# Fazer para todas as estações
# explorar_longotempo("RD011") # deve ser eliminada por não ter dados pretéritos
# explorar_longotempo("RD019")
# explorar_longotempo("RD023")
# explorar_longotempo("RD033")
# explorar_longotempo("RD035")
# explorar_longotempo("RD044")
# explorar_longotempo("RD045")
# explorar_longotempo("RD053")
# explorar_longotempo("RD058")
# explorar_longotempo("RD059")
# explorar_longotempo("RD067")
# explorar_longotempo("RD071")
# explorar_longotempo("RD072")
# explorar_longotempo("RD083")



# Dados Antes vs Depois ---------------------------------------------------
dados_turbidez_antes <- 
  dados_turbidez_tb |> 
  filter(Data < as.Date("2015-01-01"))

dados_turbidez_depois <- 
  dados_turbidez_tb |> 
  filter(Data >= as.Date("2015-09-01"))

autoplot(dados_turbidez_antes)+
  xlab("Período")+
  ylab("Turbidez")+
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        axis.ticks = element_line(color = "grey"),
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey90",
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "grey", fill = NA),
        legend.position = "none")


autoplot(dados_turbidez_depois)



# Valores de turbidez -----------------------------------------------------
head(dados_turbidez_depois)
str(dados_turbidez)

# Estações com maiores valores --------------------------------------------
turbidezMedia_depois <- 
dados_turbidez |>
  filter(Data_de_Amostragem >= as.Date("2015-09-01")) |> 
  group_by(Estacao, Ano) |>
  summarise(Turbidez_media = mean(Turbidez)) |> 
  
  ggplot(aes(Ano, Turbidez_media, fill= Estacao))+
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = 2015:2022,
                   labels = c("2015",
                              "2016",
                              "2017",
                              "2018",
                              "2019",
                              "2020",
                              "2021",
                              "2022"))+
  
  facet_wrap(~Estacao, ncol = 3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
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
        legend.position = "none")+
  xlab("Ano")+
  ylab("Turbidez Média (NTU)")
  
turbidezMedia_depois 
#ggsave(path = "figure/",
#       filename = "figura_turbidezMedia_depois.tiff",
#       plot = turbidezMedia_depois,
#       device = "tiff",
#       dpi = 500)


# Estaçoes Maiores valors de turbidez apos rompimento ---------------------
dados_turbidez |>
  filter(Data_de_Amostragem >= as.Date("2015-09-01")) |> 
  group_by(Estacao, Ano) |>
  summarise(Turbidez_media = mean(Turbidez)) |>
  arrange(desc(Turbidez_media)) 



# Comparação Antes Depois:Barras ------------------------------------------
turbidezMedia_estacao <-   
dados_turbidez |>
  mutate(Periodo = 
           case_when(
             Data_de_Amostragem < as.Date("2015-09-01") ~ "Antes",
             Data_de_Amostragem >= as.Date("2015-09-01") ~ "Depois")) |> 
  na.omit(Turbidez) |> 
  group_by(Estacao, Periodo) |> 
  summarise(Turbidez_media = mean(Turbidez)) |> 
  filter(Estacao != "RD011") |> 

  ggplot(aes(Estacao, Turbidez_media, fill = Periodo))+
  geom_bar(stat = "identity",
           position = "dodge",
           alpha = 0.7)+
  
  scale_fill_manual(values = c("Antes" = "#0066cc",
                               "Depois" = "#b60234"))+
  labs(fill = "Período") +
  theme_classic()+
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
        legend.text = element_text(size = 12))+
  xlab("Estações")+
  ylab("Turbidez Média (NTU)")
turbidezMedia_estacao  

#ggsave(path = "figure/",
#       filename = "figura_turbidezMedia_estacao.tiff",
#       plot = turbidezMedia_estacao,
#       device = "tiff",
#       dpi = 500)


# Série Trimestral --------------------------------------------------------
# Selecionar colunas
dados_turbidez_trimestral <- 
  dados_turbidez |>
  select (-Ano, -Data_de_Amostragem)

# colocar no formato longo
dados_turbidez_trimestral <- 
  dados_turbidez_trimestral |> 
  pivot_longer(cols = "Turbidez",
               names_to = "Parametros")

# média do parâmetro por estação e data
dados_turbidez_trimestral <-
  dados_turbidez_trimestral|>
  mutate(trimestre = yearquarter(Data)) |>
  select(-Parametros) |> 
  group_by(Estacao, trimestre)|> 
  summarise(value = mean(value)) |> 
  na.omit() 

# Escolhendo o index
dados_turbidez_trimestral_ts <- 
  dados_turbidez_trimestral |> 
  as_tsibble(index = trimestre,
             key= c(Estacao))
head(dados_turbidez_trimestral_ts)

# Completando buracos no tempo 
dados_turbidez_trimestral_ts <-
  fill_gaps(dados_turbidez_trimestral_ts)
View(dados_turbidez_trimestral_ts)

# Série Anual --------------------------------------------------------
# Selecionar colunas
dados_turbidez_anual <- 
  dados_turbidez |>
  select (-Data, -Data_de_Amostragem)

# colocar no formato longo
dados_turbidez_anual <- 
  dados_turbidez_anual|> 
  pivot_longer(cols = "Turbidez",
               names_to = "Parametros")

# média do parâmetro por estação e data
dados_turbidez_anual <-
  dados_turbidez_anual|>
  select(-Parametros) |> 
  na.omit() |> 
  group_by(Estacao, Ano)|> 
  summarise(value = mean(value)) 
  

# Escolhendo o index
dados_turbidez_anual_ts <- 
  dados_turbidez_anual |> 
  as_tsibble(index = Ano,
             key= c(Estacao))
head(dados_turbidez_anual_ts)


# Série Mensal --------------------------------------------------------
# Selecionar colunas
dados_turbidez_mensal <- 
  dados_turbidez |>
  select (-Ano, -Data_de_Amostragem)

# colocar no formato longo
dados_turbidez_mensal <- 
  dados_turbidez_mensal|> 
  pivot_longer(cols = "Turbidez",
               names_to = "Parametros")

# média do parâmetro por estação e data
dados_turbidez_mensal <-
  dados_turbidez_mensal|>
  select(-Parametros) |> 
  na.omit() |> 
  group_by(Estacao, Data)|> 
  summarise(value = mean(value)) 


# Escolhendo o index
dados_turbidez_mensal_ts <- 
  dados_turbidez_mensal |> 
  as_tsibble(index = Data,
             key= c(Estacao))
head(dados_turbidez_mensal_ts)




# Série Seca e Cheia --------------------------------------------------------
# Selecionar colunas
dados_turbidez_seca_cheia <- 
  dados_turbidez |>
  select (-Ano, -Data_de_Amostragem)

# colocar no formato longo
dados_turbidez_seca_cheia <- 
  dados_turbidez_seca_cheia |> 
  pivot_longer(cols = "Turbidez",
               names_to = "Parametros")

dados_turbidez_seca_cheia <- 
dados_turbidez_seca_cheia |> 
  mutate(mes = month(Data),
         Ano = year(Data)) |> 
  mutate(Periodo = case_when(mes == 10|
                             mes == 11|
                             mes == 12|
                             mes == 1|
                             mes == 2|
                             mes == 3 ~ "Chuvoso",
                             mes == 4|
                             mes == 5|
                             mes == 6|
                             mes==7|
                             mes==8|
                             mes==9 ~ "Seco")) |> 
  select(-mes,-Parametros) |> 
  group_by(Estacao,Ano,Periodo) |> 
  summarise(Turbidez_media = mean(value))

dados_turbidez_seca_cheia



# Exportar Serie Turbidez ---------------------------------------------------------
write.csv(dados_turbidez,"data/0_serie_dados_turbidez_completa.csv")




# Grafico: visualização da série 
turbidezMedia_periodo <- 
  dados_turbidez_seca_cheia |> 
  #mutate(Ano = as.Date.numeric(Ano, origin = "1997-01-01")) |> 
  ggplot() +
  geom_line(aes(x = Ano,
                y = Turbidez_media,
                color = Estacao))+
  #scale_x_date(date_labels = "%Y",
  #             date_breaks = "1 year")+
  scale_x_continuous(breaks = seq(1997, 2023, by = 2))+
  scale_y_continuous(breaks = seq(0, 6000, by = 400))+
  facet_grid(~Periodo)+
  xlab("Ano")+
  ylab("Turbidez Média (NTU)")+
   meu_tema+
  theme(axis.title.x = element_text(size=10),
        panel.grid.major.x = element_line(color = "grey90",
                                          linetype = "dashed",
                                          linewidth = 0.01),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01))+
 
  geom_vline(xintercept = 2015,
             linetype = "dashed", color = "red")
turbidezMedia_periodo

# # save
#  ggsave(path = "figure/",
#         filename = "turbidezMedia_periodo_SecaChuva.tiff",
#         plot = turbidezMedia_periodo,
#         device = "tiff",
#         dpi = 500,
#         width = 25,
#         height = 15,
#         unit = "cm")


# Transformar em série Temporal
# Escolhendo o index
 dados_turbidez_seca_cheia_ts <- 
   dados_turbidez_seca_cheia |> 
   as_tsibble(index = Ano,
              key= c(Estacao, Periodo))
 head(dados_turbidez_seca_cheia_ts)
View(dados_turbidez_seca_cheia_ts)



# Turbidez - Max ----------------------------------------------------------
turbidez_MaxMinMean <- 
dados_monitoramento_turbidez |> 
  mutate(Ano_mes = yearmonth(as.Date(Data_de_Amostragem))) |> 
  group_by(Estacao, Ano_mes) |> 
  summarise(Maximo = max(Turbidez),
            Minimo = min(Turbidez),
            Mediana = median(Turbidez),
            Media = mean(Turbidez)) 
turbidez_wider <- 
  turbidez_MaxMinMean|> 
  pivot_wider(names_from = Ano_mes, values_from = c(Maximo, Minimo, Mediana,Media))

#Exportar
#writexl::write_xlsx(turbidez_wider,path = "dados_turbidez_MaxMinMed.xlsx")

tabela_turbidez_Max_Med_Min <- 
dados_monitoramento_turbidez |> 
  mutate(mes = month(Data_de_Amostragem  ),
         Ano = year(Data_de_Amostragem  )) |> 
  mutate(Periodo = case_when(mes == 10|
                               mes == 11|
                               mes == 12|
                               mes == 1|
                               mes == 2|
                               mes == 3 ~ "Chuvoso",
                             mes == 4|
                               mes == 5|
                               mes == 6|
                               mes==7|
                               mes==8|
                               mes==9 ~ "Seco")) |> 
  group_by(Estacao, Ano, Periodo) |> 
  summarise(Maximo = max(Turbidez),
            Minimo = min(Turbidez),
            Mediana = median(Turbidez),
            Media = mean(Turbidez)) 

tabela_turbidez_Max_Med_Min_seca <- 
tabela_turbidez_Max_Med_Min|> 
  filter(Periodo == "Seco") |> 
  pivot_wider(names_from = Estacao, values_from = c(Maximo, Minimo, Mediana,Media))

tabela_turbidez_Max_Med_Min_chuva <- 
  tabela_turbidez_Max_Med_Min|> 
  filter(Periodo == "Chuvoso") |> 
  pivot_wider(names_from = Estacao, values_from = c(Maximo, Minimo, Mediana,Media))

#writexl::write_xlsx(tabela_turbidez_Max_Med_Min_chuva,
#                    path = "tabela_turbidez_Max_Med_Min_chuva.xlsx")

#writexl::write_xlsx(tabela_turbidez_Max_Med_Min_seca,
#                    path = "tabela_turbidez_Max_Med_Min_seca.xlsx")


tabela_turbidez_Max_Med_Min |> 
  filter(Estacao == "RD072") |> 
  ggplot(aes())+
  geom_line(aes(x = Ano, y = Maximo),color = "blue")+
  geom_line(aes(x= Ano, y = Minimo))+
  geom_point(aes(x= Ano, y = Media))+
  geom_vline(xintercept = 2015,
             linetype = "dashed",
             color = "red")+
    facet_wrap(~Estacao+ Periodo)















# x -----------------------------------------------------------------------


# x -----------------------------------------------------------------------


# x -----------------------------------------------------------------------


# x -----------------------------------------------------------------------


# x -----------------------------------------------------------------------


# Passar isso para o proximo Script ---------------------------------------







# Completando buracos no tempo --------------------------------------------
dados_turbidez_trimestral_ts <-
  fill_gaps(dados_turbidez_trimestral_ts)

dados_turbidez_mensal_ts <-
  fill_gaps(dados_turbidez_mensal_ts)

dados_turbidez_anual_ts <-
  fill_gaps(dados_turbidez_anual_ts,
            .start = 1996, .end = 2022)

dados_turbidez_seca_cheia_ts <- 
  fill_gaps(dados_turbidez_seca_cheia_ts, 
            .start = 1996, .end = 2022)

View(dados_turbidez_seca_cheia_ts)


# Preencher NAs -----------------------------------------------------------

# Série Mensal ------------------------------------------------------------
dados_turbidez_mensal_ts_inputNA <- 
dados_turbidez_mensal_ts |>
  mutate(value = ts_impute_vec(value, period = 365)) # input da media anual

#opção2: input da média anterior e seguinte
dados_turbidez_mensal_ts_inputNA <- 
  dados_turbidez_mensal_ts |>
mutate(value = ts_impute_vec(value, period = 1, lambda = 1))

#opção3: input através de um polinomio
dados_turbidez_mensal_ts_inputNA <- 
  dados_turbidez_mensal_ts |>
  mutate(value = ts_impute_vec(value, period = 1, lambda = 2))

# Optar pelo polinômio



# Série Seca_Chuva --------------------------------------------------------
table(is.na(dados_turbidez_seca_cheia_ts))
View(dados_turbidez_seca_cheia_ts)

# Remover estação RD011
dados_turbidez_seca_cheia_ts <- 
  dados_turbidez_seca_cheia_ts |> 
  filter(Estacao != "RD011")


# RD072
# Completar os dados NAs da seca e chuva
# usar como base 
# Para cada estação teremos q completar de um jeito
dados_turbidez_seca_cheia_ts |>
filter(Estacao == "RD072") |>
filter (Ano > 2008) |> 
  View()
mutate(Turbidez_media = ts_impute_vec(Turbidez_media, period = ))


# Falta:fazer o input das outras estações ---------------------------------











# Parei aquiiiii!!!!!! ----------------------------------------------------




# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Decomposição ------------------------------------------------------------
library(feasts)

# Serie Mensal ------------------------------------------------------------
# RD072: Decomposição ---------------------------------------------------

# 1. Separar os dados antes e depois do impacto
# Formatando o dado depois
dados_turbidez_mensal_ts_inputNA_RD072_depois <-
  dados_turbidez_mensal_ts_inputNA |>
  filter(Data >= as.Date("2015-09-01"))|>
  filter(Estacao == "RD072")

# Formatando antes do impacto
dados_turbidez_mensal_ts_inputNA_RD072_antes<-
  dados_turbidez_mensal_ts_inputNA |>
  filter(Data < as.Date("2015-09-01")) |>
  filter(Estacao == "RD072")

table(is.na(dados_turbidez_mensal_ts_inputNA_RD072_antes))
dados_turbidez_mensal_ts_inputNA_RD072_antes

# 2. Decomposição
# Decomposição usando o método STL

autoplot(dados_turbidez_mensal_ts_inputNA_RD072_antes)














# Selecionar melhor modelo
modelo = forecast::auto.arima(turbidez_mensal_ts_inputNA_RD072_depois,
                              trace=T)
modelo = forecast::auto.arima(turbidez_mensal_ts_inputNA_RD072_antes,
                              trace=T)



stl_components_total <-
  dados_turbidez_mensal_ts_inputNA |>
  filter(Estacao == "RD072") |> 
  na.omit() |> 
  model(
    STL(value ~ season()+ trend())) |> 
  components()



# Autoplot
graf_RD072_autoplot <- 
  autoplot(stl_components_total)+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 10),
        axis.text.y = element_text(size= 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA))

# OU: Sazonalidade
stl_components_total |> 
  ACF(remainder) |>  #erros
  autoplot(lag_max = 68)
gg_season(stl_components_total)




stl_components_total$Data <- as.Date(stl_components_total$Data)
graf_RD072_autoplot <-
  stl_components_total |>  
  ggplot()+
  geom_line(aes(Data, season_12))+
  scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
    theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 10),
        axis.text.y = element_text(size= 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA))

#ggsave(path = "figure/",
#        filename = "graf_RD072_autoplot_total.tiff",
 #       plot = graf_RD072_autoplot,
 #       device = "tiff",
  #      dpi = 500)

# Depois
stl_componets_depois <- 
  dados_turbidez_mensal_ts_inputNA |>
  filter(Data >= as.Date("2015-09-01")) |> 
  filter(Estacao == "RD072") |> 
  na.omit() |> 
  model(
    STL(value ~ season(12)+ trend())) |> 
  components()
autoplot(stl_componets_depois)


# Antes
stl_componets_antes <- 
  dados_turbidez_mensal_ts_inputNA |>
  filter(Data < as.Date("2015-09-01")) |> 
  filter(Estacao == "RD072") |> 
  na.omit() |> 
  model(
    STL(value ~ season(12)+ trend())) |> 
  components()
autoplot(stl_componets_antes)


stl_componets_antes$Data <- as.Date(stl_componets_antes$Data)

# ANTES !!!!!!!!!!!
graf_RD072_stl_sazonalidade_antes <- 
stl_componets_antes |> 
ggplot()+
  geom_line(aes(Data, season_12))+
  scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
  facet_wrap(~Estacao)+

theme(axis.text.x = element_text(angle = 90,
                                 vjust = 0.4,
                                 size= 10),
      axis.text.y = element_text(size= 12),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x =  element_line(color = "grey90",
                                         linetype = "dashed",
                                         linewidth = 0.01),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "grey"),
      axis.ticks = element_line(color = "grey"),
      panel.background = element_rect(fill = "white",
                                      color = "black"),
      strip.background = element_rect(colour = "grey",
                                      fill = "grey"),
      strip.text.x.top = element_text(colour = "white",
                                      face = "bold"),
      panel.border = element_rect(color = "grey", fill = NA))+
  xlab("Data")+
  ylab("Sazonalidade")

# DEPOIS !!!!!!!!!!!
stl_componets_depois$Data <- as.Date(stl_componets_depois$Data)

graf_RD072_stl_sazonalidade_depois <- 
stl_componets_depois |> 
  ggplot()+
  geom_line(aes(Data, season_12))+
  scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
  facet_wrap(~Estacao)+
  
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 10),
        axis.text.y = element_text(size= 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA))+
  xlab("Data")+
  ylab("Sazonalidade")



# ggsave(path = "figure/",
#        filename = "graf_RD072_stl_sazonalidade_depois.tiff",
#        plot = graf_RD072_stl_sazonalidade_depois,
#        device = "tiff",
#        dpi = 500)
# 
# ggsave(path = "figure/",
#        filename = "graf_RD072_stl_sazonalidade_antes.tiff",
#        plot = graf_RD072_stl_sazonalidade_antes,
#        device = "tiff",
#        dpi = 500)

###### TENDENCIA
# ANTES !!!!!!!!!!!

graf_RD072_stl_tendencia_antes <- 
  stl_componets_antes |> 
  ggplot()+
  geom_line(aes(Data, trend))+
  scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
  facet_wrap(~Estacao)+
  
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 10),
        axis.text.y = element_text(size= 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA))+
  xlab("Data")+
  ylab("Tendência")

# DEPOIS !!!!!!!!!!!
stl_componets_depois$Data <- as.Date(stl_componets_depois$Data)

graf_RD072_stl_tendencia_depois <- 
  stl_componets_depois |> 
  ggplot()+
  geom_line(aes(Data, trend))+
  scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
  facet_wrap(~Estacao)+
  
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   size= 10),
        axis.text.y = element_text(size= 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x =  element_line(color = "grey90",
                                           linetype = "dashed",
                                           linewidth = 0.01),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        panel.background = element_rect(fill = "white",
                                        color = "black"),
        strip.background = element_rect(colour = "grey",
                                        fill = "grey"),
        strip.text.x.top = element_text(colour = "white",
                                        face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA))+
  xlab("Data")+
  ylab("Tendência")

graf_RD072_stl_tendencia_depois


 ggsave(path = "figure/",
        filename = "graf_RD072_stl_tendencia_depois.tiff",
        plot = graf_RD072_stl_tendencia_depois,
        device = "tiff",
        dpi = 500)
 
 ggsave(path = "figure/",
        filename = "graf_RD072_stl_tendencia_antes.tiff",
        plot = graf_RD072_stl_tendencia_antes,
        device = "tiff",
        dpi = 500)


 ###### VALORES
 # ANTES !!!!!!!!!!!
 
 graf_RD072_stl_value_antes <- 
   stl_componets_antes |> 
   ggplot()+
   geom_line(aes(Data, value))+
   scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
   facet_wrap(~Estacao)+
   
   theme(axis.text.x = element_text(angle = 90,
                                    vjust = 0.4,
                                    size= 10),
         axis.text.y = element_text(size= 12),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x =  element_line(color = "grey90",
                                            linetype = "dashed",
                                            linewidth = 0.01),
         panel.grid.minor.y = element_blank(),
         axis.line = element_line(color = "grey"),
         axis.ticks = element_line(color = "grey"),
         panel.background = element_rect(fill = "white",
                                         color = "black"),
         strip.background = element_rect(colour = "grey",
                                         fill = "grey"),
         strip.text.x.top = element_text(colour = "white",
                                         face = "bold"),
         panel.border = element_rect(color = "grey", fill = NA))+
   xlab("Data")+
   ylab("Valores de Turbidez")
 
 # DEPOIS !!!!!!!!!!!
 stl_componets_depois$Data <- as.Date(stl_componets_depois$Data)
 
 graf_RD072_stl_value_depois <- 
   stl_componets_depois |> 
   ggplot()+
   geom_line(aes(Data, value))+
   scale_x_date(date_labels = "%Y %b", date_breaks = "3 month")+
   facet_wrap(~Estacao)+
   
   theme(axis.text.x = element_text(angle = 90,
                                    vjust = 0.4,
                                    size= 10),
         axis.text.y = element_text(size= 12),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x =  element_line(color = "grey90",
                                            linetype = "dashed",
                                            linewidth = 0.01),
         panel.grid.minor.y = element_blank(),
         axis.line = element_line(color = "grey"),
         axis.ticks = element_line(color = "grey"),
         panel.background = element_rect(fill = "white",
                                         color = "black"),
         strip.background = element_rect(colour = "grey",
                                         fill = "grey"),
         strip.text.x.top = element_text(colour = "white",
                                         face = "bold"),
         panel.border = element_rect(color = "grey", fill = NA))+
   xlab("Data")+
   ylab("Valores de Turbidez")
 
 graf_RD072_stl_tendencia_depois
 
 
 ggsave(path = "figure/",
        filename = "graf_RD072_stl_value_antes.tiff",
        plot = graf_RD072_stl_value_antes,
        device = "tiff",
        dpi = 500)
 
 ggsave(path = "figure/",
        filename = "graf_RD072_stl_value_depois.tiff",
        plot = graf_RD072_stl_value_depois,
        device = "tiff",
        dpi = 500)
 
 
 

 
 
 
 
 


# Autocorrelação ----------------------------------------------------------
dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD011") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD019") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD023") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD033") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD035") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD044") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD053") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD058") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD059") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD067") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD071") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD072") |> 
  feasts::ACF(y = value) |> 
  autoplot()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD083") |> 
  feasts::ACF(y = value) |> 
  autoplot()


# Sazonalidade ------------------------------------------------------------
dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD011") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD019") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD023") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD033") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD035") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD044") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD053") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD058") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD059") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD067") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD071") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |>
  filter(Estacao == "RD072") |> 
  feasts::gg_season()

dados_turbidez_trimestral_ts |> 
  filter(Estacao == "RD083") |> 
  feasts::gg_season()

#todos juntos
dados_turbidez_trimestral_ts |> 
  feasts::gg_season()

# Completar Valor NA ------------------------------------------------------
table(is.na(turbidez_ts_trimestral$value))
turbidez_ts_trimestral


concentracao_sazonal = ts_impute_vec(concentracao, period = 365)


# Decomposição ------------------------------------------------------------
stl_componets <- 
  dados_turbidez_anual_ts |>
  filter(Estacao == "RD072") |> 
  na.omit() |> 
  model(
    STL(value ~ season(4)+ trend())) |> 
  components()
autoplot(stl_componets)

dados_turbidez_mensal_ts %>%
  plot_time_series_boxplot(Data, value, .period = "1 month")


## Grafico EXEMPLO
ggplot(df, aes(Data)) +
  geom_line(aes(y = Tendencia), color = "blue", size = 1) +
  geom_line(aes(y = Sazonalidade), color = "red", size = 1) +
  geom_line(aes(y = Residuos), color = "green", size = 1) +
  xlab("Data") +
  ylab("Valor") +
  ggtitle("Decomposição STL")

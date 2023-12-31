
# Pacotes -----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(tsibble)
library(tidyr)
library(tmap)
library(ggplot2)
library(ggtech)

# Temas -------------------------------------------------------------------
devtools::install_github("ricardo-bion/ggtech", 
                         dependencies=TRUE)

# Leitura -----------------------------------------------------------------


dados_1997_2001 <- 
  readxl::read_excel("data/Dados_monitoramento1997_2001.xls", sheet = "Doce")

dados_2002_2006 <- 
  readxl::read_excel("data/Dados_monitoramento2002_2006.xls", sheet = "Doce")

dados_2007_2011 <- 
  readxl::read_excel("data/Dados_monitoramento2007_2011.xls", sheet = "Doce")

dados_2012_2016 <- 
  readxl::read_excel("data/Dados_monitoramento2012_2016.xls", sheet = "Doce")

dados_2017 <- 
  readxl::read_excel("data/Dados_monitoramento2017.xlsx", sheet = "Doce")

dados_2018_1 <- 
  readxl::read_excel("data/Dados_monitoramento2018_1trimestre.xlsx", sheet = "Doce")

dados_2018_2 <- 
  readxl::read_excel("data/Dados_monitoramento2018_2trimestre.xlsx", sheet = "Doce")

dados_2018_3 <- 
  readxl::read_excel("data/Dados_monitoramento2018_3trimestre.xlsx", sheet = "Doce")

dados_2018_4 <- 
  readxl::read_excel("data/Dados_monitoramento2018_4trimestre.xlsx", sheet = "Doce")

dados_2019_1 <- 
  readxl::read_excel("data/Dados_monitoramento2019_1trimestre.xlsx", sheet = "Doce")

dados_2019_2 <- 
  readxl::read_excel("data/Dados_monitoramento2019_2trimestre.xls", sheet = "Doce")

dados_2019_3 <- 
  readxl::read_excel("data/Dados_monitoramento2019_3trimestre.xlsx", sheet = "Doce")

dados_2019_4 <- 
  readxl::read_excel("data/Dados_monitoramento2019_4trimestre.xlsx", sheet = "Doce")

# Dados que tenho que filtrar pela estação

dados_2020_1 <- 
  readxl::read_excel("data/Dados_monitoramento2020_1trimestre.xlsx")

dados_2020_2 <- 
  readxl::read_excel("data/Dados_monitoramento2020_2trimestre.xlsx")

dados_2020_3 <- 
  readxl::read_excel("data/Dados_monitoramento2020_3_4_trimestre.xls")

dados_2021 <- 
  readxl::read_excel("data/Dados_monitoramento2021_1_2_trimestre.xls")

dados_2021_2 <- 
  readxl::read_excel("data/Dados_monitoramento2021_IGAM_3_4_trimestre.xls")

dados_2022 <- 
  readxl::read_excel("data/Dados_monitoramento2022_1_2trimestre.xls")


# Unir dados empilhando -------------------------------------------------------------

# Checando se os nomes são iguais entre as planilhas
identical(names(dados_1997_2001), names(dados_2002_2006))

# Junção
dados_monitoramento <- rbind(dados_1997_2001,dados_2002_2006,
                             dados_2007_2011,dados_2012_2016,
                             dados_2017,dados_2018_1,dados_2018_2,
                             dados_2018_3,dados_2018_4,dados_2019_1,
                             dados_2019_2,dados_2019_3,dados_2019_4,
                             dados_2020_1,dados_2020_2,dados_2020_3,
                             dados_2021,dados_2021_2,dados_2022)


# Ajeitando as datas ------------------------------------------------------
dados_monitoramento <- dados_monitoramento |> 
  mutate(Data_de_Amostragem = ymd(Data_de_Amostragem),
         Ano = year(Data_de_Amostragem),
         Mes = month(Data_de_Amostragem)) 

# Filtrando as estações do rio Doce --------------------------------------------
dados_monitoramento <- dados_monitoramento |> 
  filter(stringr::str_starts(Estacao,"RD"))

# Tem Anos NA?
table(is.na(dados_monitoramento$Ano)) 
table(is.na(dados_monitoramento$Latitude)) 



# Completar Estações com NAs ----------------------------------------------
dados_monitoramento$Latitude <- as.numeric(dados_monitoramento$Latitude)
dados_monitoramento$Longitude <- as.numeric(dados_monitoramento$Longitude)

# EXTRAIR COORDENADAS
coordenadas <- dados_2019_1 |> 
  filter(stringr::str_starts(Estacao,"RD")) |> 
  select(Estacao, Latitude, Longitude) 

coordenadas <- coordenadas |>
  group_by(Estacao, Latitude, Longitude) |> 
  rename(lat = Latitude, long= Longitude) |> 
  summarise(Estacao = unique(Estacao)) 
  

# JUNTAR COORDENADAS COM TABELA
dados_monitoramento <- left_join(dados_monitoramento, coordenadas, by = "Estacao")


# Tem Na?
dados_monitoramento |> 
  filter(Estacao == "RD001") |> 
  summarise(sum(is.na(Latitude)))
# nova coluna
dados_monitoramento |> 
  filter(Estacao == "RD001") |> 
  summarise(sum(is.na(lat)))



# Excluindo latitudes faltantes ---------------------------------------------
colnames(dados_monitoramento)

dados_monitoramento <- 
  dados_monitoramento |> 
  mutate(Latitude = lat,
         Longitude = long) |> 
  select(-lat, -long)

dados_monitoramento <- 
  dados_monitoramento |> 
  drop_na(Latitude, Longitude)


table(is.na(dados_monitoramento$Longitude))
table(dados_monitoramento$Estado)



# Quantos Anos temos na base? ---------------------------------------------
table(dados_monitoramento$Ano) # 1997 - 2022

dados_monitoramento |> 
  group_by(Ano) |> 
  summarise(Ano = unique(Ano)) |> 
  summarise(n())

# Ano de 1997
dados_monitoramento |> 
  group_by(Ano) |> 
  summarise(Ano = unique(Ano)) |> 
  slice_min(Ano)

# Até 2022
dados_monitoramento |> 
group_by(Ano) |> 
  summarise(Ano = unique(Ano)) |> 
  slice_max(Ano)


# Quantas Estações --------------------------------------------------------
table(dados_monitoramento$Estacao) # Estações que não são do rio doce

# 65 estações
dados_monitoramento |> 
  group_by(Estacao) |> 
  summarise(unique(Estacao)) |> 
  summarise(n())


# Quantos parametros sao monitorados --------------------------------------
str(dados_monitoramento)
colnames(dados_monitoramento)

# 194 parâmetros são monitorados
212-18





# Importar Shapes ---------------------------------------------------------
# Bacia
bacia_mg <- raster::shapefile("shape/riodoceMG.shp")
bacia <- maptools::readShapePoly("shape/Limite Bacia Rio Doce - ENGECORPS - BHRD.shp")
# Trecho Afetado
trechoafet <- raster::shapefile("shape/Trecho Afetado - IGAM - BHRD.shp")
trechoafet <- trechoafet |> 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326)
trechoafet <- sf::st_set_crs(trechoafet, 4326)

# Ponto ruptura
pontorup <- maptools::readShapeSpatial("shape/Barragens_Rejeito_Samarco.shp")
pontorup_df <- as.data.frame(pontorup)
pontorup <- pontorup_df |> 
  mutate(long= -43.4671,lat= -20.2065 ) |> 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

uhe <- raster::shapefile("shape/Usinas Hidreletricas 3 - BHRD.shp")


# Mapa Padrão ------------------------------------------------------------
mapa_basico <- 
  tm_shape(bacia)+
  tm_fill(col = "#69b3a2", lwd = 2, alpha = 0.3)+
  tm_shape(bacia_mg)+
  tm_fill(col="#69b3a2",
          lwd = 2,
          alpha = 0.5)+
  tm_graticules(lines = FALSE)+
  tm_compass(type = "4star",
             position = c("left","top"),
             size = 1)+
  tm_scale_bar(breaks = c(0, 20, 40), 
               position = c("center"))+
  tm_shape(trechoafet)+
  tm_lines(col = "#FAA228",
           lwd = 2,
           alpha = 0.65)+
  tm_shape(pontorup)+
  tm_dots(col = "#DC143C",
          title = "Barragem",
          size =1,
          alpha = 1,
          shape = 2)+
  tm_text(text = "Nome",
          size = 0.75,
          just = "top",
          xmod = 1,
          ymod = -1.5,
          shadow = F,
          col = "#DC143C",
          fontface = "bold")+
  tm_layout(legend.title.size = 1,
            legend.title.fontface = "bold",
            legend.position = c("right", "bottom"),
            legend.just = "right",
            bg.color = "#f5f5f2",
            frame = F,
            legend.text.color = "#22211d",
            legend.title.fontfamily = "Ubuntu Regular",
            legend.format = "center")
mapa_basico


# Transformar dados em classe sf ------------------------------------------
# Colocando em WGG84
dados_monitoramento_sf <- 
  dados_monitoramento |>
  select(Estacao, Latitude, Longitude) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326)
bacia <- bacia |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Mapa: Estações de Monitoramento -----------------------------------------
estacao_monitoramento_total <- 
  mapa_basico+
  tm_shape(dados_monitoramento_sf)+
    tm_dots(size = 0.5, col = "#1685f5")+
  tm_shape(filter(dados_monitoramento_sf,
                    Estacao == "RD011"|
                    Estacao == "RD071"|
                    Estacao == "RD072"|
                    Estacao == "RD019"|
                    Estacao == "RD023"|
                    Estacao == "RD035"|
                    Estacao == "RD033"|
                    Estacao == "RD083"|
                    Estacao == "RD044"|
                    Estacao == "RD045"|
                    Estacao == "RD053"|
                    Estacao == "RD058"|
                    Estacao == "RD059"|
                    Estacao == "RD067"))+
  tm_dots(size = 0.5, col = "#edde5a")
  
estacao_monitoramento_total

# tmap_save(estacao_monitoramento_total,
#           "maps/estacao_monitoramento_total.png",
#           width= 20,
#           height = 15,
#           units = "cm")


# Selecionando Estações no trecho afetado ------------------------------------
dados_monitoramento<- dados_monitoramento |> 
  filter(Estacao == "RD011"|
         Estacao == "RD071"|
         Estacao == "RD072"|
         Estacao == "RD019"|
         Estacao == "RD023"|
         Estacao == "RD035"|
         Estacao == "RD033"|
         Estacao == "RD083"|
         Estacao == "RD044"|
         Estacao == "RD045"|
         Estacao == "RD053"|
         Estacao == "RD058"|
         Estacao == "RD059"|
         Estacao == "RD067")


# Quantas estações no trecho afetado? -------------------------------------
dados_monitoramento |> 
  group_by(Estacao) |> 
  summarise(unique(Estacao)) |> 
  summarise(n())


# Checando estaçoes por ano: Todas possuem dados?
tab <- table(dados_monitoramento$Estacao, dados_monitoramento$Ano) 
tab |>  
  kbl() |> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
  column_spec(20, 
              border_left = 20)

t(tab) |> 
kbl() %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Pré-Impacto", 1, 18, label_row_css = "background-color: #4b7299; color: #fff;") |> 
  row_spec(1:18, col = "white",background = "#72ade8") |> 
  pack_rows("Pós-Impacto", 19, 26, label_row_css = "background-color: #99544b; color: #fff;") |>
   row_spec(19:26, col = "white",background = "#f08475") |> 
  column_spec(c(2:15), border_left = TRUE, border_right = TRUE) |> 

# save_kable("tab_estacao_por_ano.jpeg")




# Selecionamento Parametros -----------------------------------------------
# Segundo Ibama: 
# Alumínio; Bário; Cálcio; 
# Chumbo; Cobalto; Cobre; Cromo; Estanho; 
# Ferro; Magnésio; Manganês; Níquel; Potássio;
# Sódio; Condutividade, Flureto, Fósforo Total,
# Sólidos Totais Dissolvidos, Sólidos Suspensos,
# Sólidos Totais, Turbidez e Cloro Residual Total.
table(dados_monitoramento$Bacia_Hidrografica )
table(dados_monitoramento$`Bacia_Relatorio_(Rede_Basica)`)
table(dados_monitoramento$Classe_de_Enquadramento)
table(dados_monitoramento$Curso_Dagua)
table(dados_monitoramento$Descricao)
table(dados_monitoramento$Estado)
table(dados_monitoramento$Instituicao)
table(dados_monitoramento$Municipios)
table(dados_monitoramento$Projeto)
table(dados_monitoramento$Sub_Bacia)
table(dados_monitoramento$Tipo_Corpo_Dagua)
table(dados_monitoramento$UPGRH)



# Colunas que podem ser excluidas
# Bacia_Hidrografica: todos rio doce
# `Bacia_Relatorio_(Rede_Basica)`>: todos rio doce
# Classe_de_Enquadramento: todos Classe 2
# Estado: minas gerais
# Instituição: CETEC/ IGAM
# Projeto
# Subbacia
# Tipo_Corpo_Dagua: tudo lótico
# UPGRH

View(dados_monitoramento)

dados_monitoramento <- dados_monitoramento %>% 
  select( 
         Estacao, Data_de_Amostragem,Ano, Hora_de_Amostragem, Latitude, Longitude,
         Aluminio_dissolvido,
         Aluminio_total,
         Bario_total,
         Calcio_total,
         Chumbo_total,
         Cobre_dissolvido,
         Cobre_total,
         Cromo_hexavalente,
         Cromo_total,
         Cromo_trivalente,
         Estanho_total,
         Ferro_dissolvido,
         Ferro_total,
         Magnesio_total,
         Manganes_total,
         Manganes_dissolvido,
         Niquel_total,
         Potassio_total,
         Potassio_dissolvido,
         Sodio_dissolvido,
         Sodio_total,
         Condutividade_eletrica_in_loco,
         Condutividade_eletrica_laboratorio,
         Fluoreto_ionizado,
         Fosforo_total,
         Solidos_dissolvidos_totais,
         Solidos_em_suspensao_totais,
         Solidos_totais,
         Turbidez
         ) 

dados_monitoramento%>% 
  kableExtra::kbl() %>%
  kableExtra::kable_paper("hover", full_width = F)



# Summary -----------------------------------------------------------------
summary(dados_monitoramento)


# Checando informaçoes disponiveis ----------------------------------------
dados_monitoramento$Ano <- as.factor(dados_monitoramento$Ano)

nome <- dados_monitoramento |> 
  summarise(nome=unique(Estacao)) |> 
  arrange(nome) 
nome <- as.vector(nome)

# Turbidez
boxplot_por_estacao <- function(nome){
tab <- dados_monitoramento |>
  select(Estacao, Turbidez, Ano) |>
  drop_na(Turbidez) |>
  filter(Estacao == nome) 

grafico <- tab |> 
  ggplot()+
  geom_boxplot(aes(y= Turbidez, x = Ano))+
  
  scale_x_discrete(breaks = seq(1997,2022, 1))+
  coord_flip()+
  ggthemes::theme_clean()
  
ggsave(path = "figure/Turbidez",filename = paste0(nome,".jpg"),
         plot = grafico, device = "jpeg", dpi = 300)}

 #for(i in nome){purrr::map_chr(i, boxplot_por_estacao)}


  

# Mapa interativo: Onde estão as estações mais impactadas -----------------
# Mapa: Estações de Monitoramento -----------------------------------------
dados_monitoramento_sf <- 
  dados_monitoramento |>
  select(Estacao, Latitude, Longitude) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326)
  
tmap_mode("plot")
tmap_mode("view")
estacao_monitoramento_impactada <- 
  mapa_basico+
tm_shape(dados_monitoramento_sf)+
  tm_dots(size = 1, col = "#1685f5")+
  tm_text("Estacao",
          size=0.7,
          col="black",
          xmod = -1,
          ymod = 0.5)+
tm_shape(uhe)+
    tm_dots(size = 0.7,shape = 3,
            col="white")+
    tm_text("nome", 
            size=0.7,
            col="white",
            xmod = 2,
            ymod = -0.5)
  
estacao_monitoramento_impactada

# tmap_save(estacao_monitoramento_impactada,
#           "maps/estacao_monitoramento_impactada.png",
#           width= 20,
#           height = 15,
#           units = "cm")




# Exportar dados ----------------------------------------
# write.csv(dados_monitoramento,"dados_monitoramento.csv")







# Projeto: Impactos do rompimento de barragem de minério na qualidade da água da Bacia do Rio Doce (Minas Gerais, Brasil)
:book:[Relatório](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/blob/main/docs/2023-10-02_TCC_NataliaFreitasdeSouza.pdf)
⚙️[Script]()  
![image]()

## Objetivo: 
Esse projeto foi elaborado para a conclusão do curso MBA em Data Science e Analytics da USP/ESALQ.
O objetivo consiste em avaliar o impacto proveniente do rompimento da barragem de rejeito de minério na bacia do rio Doce.
 
## Dados:
Foram compilados dados temporais de 13 estações de monitoramento do IGAM, localizadas ao longo dos trechos de rio afetados
pelo rompimento da barragem, que contam com uma série temporal de 26 anos de registros de turbidez. 
[Pasta de Dados Utilizados](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/tree/main/data)

## Metodologia:
#### Análise Descritiva
 - Análise Gráfica dos valores médios anuais de turbidez no período de 1998-2022, em cada estação de monitoramento, foi utilizada para evidenciar o impacto decorrente do rompimento da barragem de Fundão na qualidade da água da bacia do rio Doce;
 - Os dados foram separados em período seco (abril – setembro) e chuvoso (outubro – março) para que os valores máximos e médias anuais pudessem ser comparadas ao longo dos anos tendo em vista o comportamento sazonal esperado para o parâmetro de turbidez;
 - Avaliação do comportamento da série temporal quanto a tendência temporal e sazonalidade foi avaliado pelo método “Seasonal Trend Loess” [STL];

#### Avaliação do Impacto
 - Aplicação do modelo de regressão linear ITS (Séries Temporais Interrompidas ), com método de mínimos quadrados ordinários (OLS), inclui uma variável que representa a mudança de nível e outra para a mudança de inclinação após o impacto, podendo ser formalizada pela expressão: Yt  = β0 + β1 · tempo + β2 · intervenção + β3 · tempo desde a intervenção; Além da variável tempo e intervenção foi utilizado, ainda, o termo sazonal de Fourier, que consistem em pares de funções seno e cosseno de diferentes frequências, para controlar os padrões sazonais na série temporal; 
 - Quantificação do efeito do rompimento através do método de Cohen’s d, comparando os valores de turbidez do período pós-impacto com os seus valores contrafactuais, baseados em modelos. Os valores contrafactuais são previsões do modelo na hipótese de não ocorrência do impacto, portanto, são valores que permanecem com a tendência existente na série de turbidez pré-impacto;

#### Avaliação do Modelo utilizado
A série de turbidez foi ajustada para ser utilizada em frequência mensal e foi logaritmizada para a estabilização das variâncias e normalização dos resíduos. A complementação dos valores faltantes foi realizada através de uma interpolação linear dos valores com ajuste de sazonalidade anual. Nesta abordagem, primeiro uma decomposição STL foi calculada e, em seguida, os valores ajustados para a sazonalidade anual foram interpolados linearmente. A série foi avaliada quanto a sua estacionaridade através do teste de Dickey-Fuller e os resíduos foram avaliados quanto a normalidade através de análise gráfica e quanto a autocorrelação através da função de autocorrelação ACF/PACF e pelo teste Ljung-Box. 

## Análises Utilizadas:
Análises de Séries Temporais:
- Regressão ITS (Séries Temporais Interrompidas)
- Método de decomposição “Seasonal Trend Loess” [STL]
- Autocorrelação ACF/PACF
- Teste Ljung-Box
- Arima (em andamento)


## Resultados:


## Conclusão
  

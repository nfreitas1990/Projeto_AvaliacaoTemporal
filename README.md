# Projeto: Impactos do rompimento de barragem de minério na qualidade da água da Bacia do Rio Doce (Minas Gerais, Brasil)
:book:[Relatório](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/blob/main/docs/2023-10-02_TCC_NataliaFreitasdeSouza.pdf)
⚙️[Script](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/tree/main/scripts)  


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

  Estações de Monitoramento utilizadas
![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/d830f469-591b-4627-8ab0-992579914bd3)

Figura 1. Estações de monitoramento, localizadas no trecho afetado pelo rompimento da barragem, que foram selecionadas para análise das séries temporais de turbidez.Fonte: Resultados originais da pesquisa.

  
  A análise gráfica da série temporal mostrou aumento abrupto nos valores de turbidez em todas as estações de monitoramento após o rompimento da barragem de Fundão, que ocorreu em novembro de 2015 (Figura 2A).
Em todas as estações, o período pós-rompimento foi marcado pelo aumento na turbidez nos primeiros anos, principalmente em 2015 e 2016, seguido pela redução gradual nos anos posteriores a 2017 (Figura 2B).

![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/deb529fe-79a1-4004-bc26-f7828c6e3234)

Figura 2. Turbidez média anual [NTU] para as estações localizadas na trajetória da lama da barragem de Fundão, Minas Gerais. A linha pontilhada vermelha indica o ano de rompimento da barragem (2015). Fonte: Resultados originais da pesquisa.

  
  Após o rompimento, o período chuvoso (outubro – março) apresentou valores excessivos de turbidez (> 400 NTU), atingindo valores até 10 vezes maiores do que os observados pré-rompimento (Tabela 1; Figura 3).
Em contrapartida, o período seco (abril – setembro) apresentou valores máximos de turbidez similares aos observados no período pré-rompimento (<100 NTU) (Tabela 1; Figura 3).
Os resultados de turbidez apresentados reiteram os estudos que apontam para um efeito negativo substancial do rompimento na qualidade da água da bacia do Rio Doce (Rudorff et al., 2018; Santana et al., 2021; da Silva et al., 2022).
Em 2022, sete anos após o impacto, o período chuvoso ainda apresenta valores de turbidez máxima maiores do que os que ocorriam em período anterior e, que ultrapassam em pelo menos três vezes o limite de 100 NTU preconizado na legislação vigente (Tabela 1; Conama, 2005), o que evidencia a permanência do impacto. 

Tabela 1. Valores máximos de turbidez antes e após o rompimento da barragem para cada estação de monitoramento no período chuvoso (outubro – março) e seco (abril – setembro). Os círculos representam valores de turbidez de até 100 NTU (verde); até 400 NTU (amarelo); acima de 400 NTU (vermelho).

![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/e04a8cc1-353e-4803-8e99-0cffbb15f6bb)


![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/1ec69a48-32ec-4d9a-9a51-537e71186046)
Figura 3. Turbidez Média anual (NTU), no período seco e chuvoso, para as estações localizadas nos trechos impactados pelo rompimento da barragem de Fundão, Minas Gerais. A linha pontilhada vermelha indica o ano de rompimento da barragem (2015). Fonte: Resultados originais da pesquisa.


  A decomposição STL evidenciou uma leve tendência decrescente na série de turbidez no período pré-rompimento em todas as estações de monitoramento (Figura 4). Este padrão foi alterado no período pós-rompimento, quando a série passou a apresentar forte tendência decrescente para todas as estações ao longo dos anos (Figura 4). 


![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/5b6662b8-db6c-4463-a038-a49bf5389530)
Figura 4. Componente Tendência da decomposição STL para cada estação de monitoramento.Fonte: Resultados originais da pesquisa.


  Reiterando os resultados da análise descritiva, a decomposição STL evidenciou, ainda, a sazonalidade da série temporal de turbidez em todas as estações de monitoramento, com aumento dos valores de turbidez nos meses chuvosos (outubro – março) e redução nos meses secos (abril – setembro) (Figura 5). A sazonalidade é característica tanto do período pré quanto do pós-rompimento, entretanto, o período pós-rompimento foi marcado pelo aumento na magnitude da sazonalidade, com diferenças acentuadas entre os períodos secos e chuvosos nos anos seguintes ao rompimento e, redução gradual da magnitude com o passar dos anos (Figura 5). 

 ![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/05ef3281-9789-4e6d-9ea7-4d65c21b26bb)
Figura 5. Componente Sazonalidade da decomposição STL para cada estação de monitoramento. Fonte: Resultados originais da pesquisa.


  O modelo global da análise ITS foi estatisticamente significativo para todas as estações estudadas (Apêndice). O coeficiente de determinação variou entre 0,45 – 0,60 dependendo da estação (p-valor <0,001) (Apêndice; Tabela 2). O coeficiente de intervenção mostrou mudança de nível significativa na série de turbidez no período pós-impacto para todas as estações, com aumento de 150% a 1800% comparado aos valores de turbidez pré-impacto – o que equivale a valores em média entre 2,5–19 vezes maiores no período pós impacto comparado ao pré-impacto (Tabela 2). O coeficiente tempo desde o impacto mostrou mudança de inclinação significativa no período pós-impacto apenas para quatro estações (RD019, RD023, RD033 e RD035), com redução entre 1,79% e 2,0% nos valores de turbidez a cada mês. 

Tabela 2. Resumo dos modelos ITS para cada estação. Valores dos parâmetros betas, que estavam em escala logarítmica, foram transformados por (exp (beta) -1) * 100 para obter a mudança percentual de Y por unidade de X (veja Apêndice para dados brutos).Fonte: Resultados originais da pesquisa.
![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/495ed1b1-6d16-4a77-9b13-53b3d99e967a)
![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/883eeedd-7e1e-48c3-8261-55bf1889314d)


  No cenário contrafactual, a série pós-impacto divergiu dos valores de turbidez previstos na ausência de impacto (Tabela 3; Figura 6). Apesar de manter as oscilações sazonais esperadas, os valores de turbidez apresentaram diferenças quanto a magnitude dos valores. A magnitude do efeito do rompimento da barragem na qualidade da água foi classificada como alta para todas as estações, com exceção da estação RD053 e RD067, que foram classificadas como moderada (Tabela 3). Para a maioria das estações, os valores de turbidez divergiram mais do que um desvio padrão dos valores contrafactuais previstos pelo modelo. As estações RD071, RD072 e RD083 divergiram mais do que dois desvios padrão, indicativo de que o rompimento da barragem teve um efeito substancial nos valores de turbidez de todas as séries, mas, principalmente, das estações que estão localizadas mais próximas ao ponto de impacto. 

Tabela 3. Magnitude do efeito do rompimento da barragem na qualidade da água através do método Cohen’s d. Magnitude classificada como desprezível quando distância Cohen’s d <0,2; baixa entre 0,2-0,3; moderada entre 0,5-0,8; e alta quando > 0,8.Fonte: Resultados originais da pesquisa.
![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/57278759-2516-440e-a710-ee3133fec3c7)

![image](https://github.com/nfreitas1990/Projeto_AvaliacaoTemporal/assets/28782509/a3cfdf10-f355-47ad-82de-4398e510f01b)
Figura 6. Série temporal de turbidez em frequência trimestral com valores preditos em escala transformada por Box Cox. Em laranja, a série contrafactual. Fonte: Resultados originais da pesquisa.

## Conclusão
O estudo avaliou o impacto temporal decorrente do rompimento da barragem de Fundão na qualidade da água da bacia do rio Doce. A princípio, era esperado (i) alterações abruptas nos valores de turbidez logo após a ocorrência do impacto, principalmente nos períodos chuvosos; (ii) interferência do impacto na tendência e na magnitude da sazonalidade; e (iii) efeito de grande magnitude nos valores de turbidez, principalmente nas estações próximas ao impacto. Os resultados corroboraram totalmente a previsão (i) e (iii) e parcialmente a previsão (ii). Em relação as previsões (i) e (iii), todas as análises evidenciaram o efeito catastrófico que o rompimento provocou na qualidade da água, principalmente no período chuvoso. Em contrapartida, a previsão (ii) foi parcialmente corroborada, pois, embora a decomposição STL tenha evidenciado a forte tendencia decrescente na série de turbidez após o impacto, conforme previsto inicialmente, modelo ITS não reiterou totalmente este resultado. O ITS apesar de ter sido eficiente para verificar a mudança repentina nos valores de turbidez, não teve o mesmo desempenho com o efeito a longo prazo. Embora o modelo ITS tenha apresentado bom ajuste, explicando boa parte da variabilidade dos dados, poderia ser aperfeiçoado para considerar os efeitos autorregressivos, através da aplicação do método ARIMA, e os efeitos da precipitação, o que pode melhorar consideravelmente as suas previsões. Neste sentido, novas abordagens devem ser exploradas para que o efeito a longo prazo possa ser melhor ajustado, possibilitando prognósticos dos valores de turbidez, o que subsidiaria estratégias de gestão, manejo e restauração do ecossistema  

## Análises Futuras
- Adicionar covariáveis no modelo
- Utilizar a metodologia Arima para prognostico de retorno dos valores de turbidez aos observados antes do impacto
- Utilizar a metodologia Arima para comparar com a metodologia OLS utilizada para os modelos de séries temporais interrompidas

Modelo de oferta de pescado
================

## Objetivo

Analisar as características da oferta do pescado no litoral de São
Paulo, no período 2008-2019 propondo um modelo econométrico.

## Metodologia

  - Revisão bibliográfica sobre modelos econométricos de oferta de bens.
  - Regressão de quantidades e preços do pescado.

## Justificativa

Compreender a oferta do pescado no estado de São Paulo sob uma
perspectiva econométrica e quantitativa.

## Revisão bibliográfica

A oferta de um bem é resultado da tomada de decisão de seu produtor em
relação à quantidade produzida a um determinado nível de preço deste
bem. Essa decisão é feita com base nos custos e tecnologia usada para
produção.

Na oferta de mercado reflete a quantidade de bens que todos os
produtores de um mercado estão dispostos a produzir e vender ao preço de
mercado.

Em geral, a relação entre quantidade ofertada e preço de um bem é
positiva. Um produtor é incentivado a produzir mais de seu bem uma vez
que preços maiores proporcionam maiores rendimentos. Além dos custos e
técnologias já citados, outros fatores como tributação/subsídio e
sazonalidade também são importantes para determinação da quantidade
ofetada de um bem.

## Modelo teórico

A teoria da oferta expressa a quantidade de um bem ofertada em função de
seu preço.

\[Q_{pescado} = f(P_{pescado})\]

Em que \(Q\) é a quantidade e \(P\) é o preço do *quilo* do pescado em
um dado período de tempo.

## Modelo econométrico

A proposta de modelagem econométrica da oferta de pescado no período
2008-2019 é feita por meio do método de Mínimos Quadrados Ordinários
(MQO), onde através da minimização dos quadrados dos resíduos se obtém a
variância dos dados. O modelo é logaritmizado para obtenção do
coeficiente de elasticidade.

\[\ln{Q_{pescado}} = \beta_{1} + \beta_{2} * \ln{P_{pescado}} + u\]

Onde \(\ln{Q_{pescado}}\) é a variável dependente, \(\ln{P_{pescado}}\)
a explicativa. \(\beta\) são os parâmetros do modelo e \(u\) é o termo
de erro aleatório. Segundo Gujarati (2000) os termos de erro aleatório
\(u\) devem seguir o os pressupostos:

  - \(E(u \mid x) = 0\);
  - Homocedasticidade (\(Var(u \mid X) = \sigma^{2}\) constante);
  - Ausência de correlação
    (\(Cor(u_i, u_j) = 0 \;\;\;\; \forall \; i \ne j\)) e
  - \(Cov(u_i \mid X_i) = 0\).

O coeficiente de determinação \(R^2\) mede o ajustamento da equação de
regressão aos dados amostrais e varia entre \(0\) e \(1\). Com ele
busca-se compreender a proporção da variação da variável dependente
\(Y\) que é explocado pelos regressores \(X\). O coeficiente de
determinação \(R^2\) ajustado mede a aderencia do modelo em relação aos
dados considerando o número de variáveis e o tamanho da amostra.

A estatística \(F\) testa se as variáveis explicativas \(X\) do modelo
tem, de fato, efeito sobre a variável dependente \(Y\). Sob hipótese
nula (\(H_0\)) nenhum dos regressores \(X\) afeta a variável dependente
\(Y\). Na hipótese alternativa (\(H_1\)), pelo menos um dos regressores
são relevantes para explicar \(Y\) sob o grau de liberdade e nível de
significãncia adequado. Para cada variável, isoladamente, um teste \(t\)
de *student* é realizado com níveis de significância (\(p-value\)) de
1%, 5% e 10%.

### Elasticidade da oferta

A elasticidade-preço da oferta é um indicador de sensibilidade da
quantidade ofertada em relação às variações nos preços de um bem. Ela
mede a variação na quantidade da oferta que ocorre quando há uma
variação no preço de venda de um bem. Uma curva de oferta pode ser
considerada mais inelástica quanto menor for o impacto das variações de
preço sobre a quantidade ofertada deste bem.

### Dados

Os dados utilizados são do Propesq.

<details>

<summary> Ver Código </summary>

``` r
library(tidyverse)
library(kableExtra)
library(lubridate)

propesq <- "../data/propesq.rds" %>% read_rds()

#BETSsearch(description = "IPCA", lang = 'pt')
ipca_ts <- BETS::BETSget(code = "433")

ipca <- tibble(ipca = as.numeric(ipca_ts) / 100,
               periodo = seq(ymd('1980-01-01'), ymd('2020-03-01'), by = 'month')
               )

base <- c(100)
base_100 <- c()

for(i in 1:length(ipca$ipca)) {
  
  base_100 <- (1 + ipca$ipca[i]) * base[i]
  
  base <- c(base, base_100)
}

ipca <- ipca %>% mutate(base = base[-1])

corvina <- propesq %>% 
  filter(valor_estimado > 0 & pescado == "Corvina" & ano > 2007) %>%
  left_join(., ipca, by="periodo")

ipca_jun2018 <- ipca %>% filter(periodo == as.Date("2018-06-01")) %>% pull(base)

corvina <- corvina %>% 
  mutate(valor_estimado_real = valor_estimado * ipca_jun2018 / base)


qtd <- corvina %>% 
  #filter(ano > 2015) %>% 
  group_by(pescado, periodo) %>%
  mutate(kg = sum(kg)) %>%
  ggplot() +
  geom_line(aes(x=periodo, y=kg/1000), color = "darkblue") +
  labs(y = "",
       x="",
       title = "Produção (ton.)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,500, 100),
                     labels = paste0(seq(0,500, 100),"t"),
                     limits = c(0,500)) +
  theme_bw()


kg <- corvina %>% 
  #filter(ano > 2015) %>% 
  group_by(pescado, periodo) %>%
  mutate(kg = sum(kg))

rt <- corvina %>% 
  #filter(ano > 2015) %>% 
  group_by(pescado, periodo) %>%
  mutate(valor_estimado_real = sum(valor_estimado_real),
         valor_estimado_nominal = sum(valor_estimado)) %>%
  ggplot() +
  geom_line(aes(x=periodo, y=valor_estimado_real/1000), color = "darkred") +
  geom_line(aes(x = periodo, y = valor_estimado_nominal/1000), color = "darkred", alpha=0.5) +
  labs(y = "",
       x="",
       title = "Valor estimado (R$/mil)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,2500, 250),
                     labels = paste0("R$", seq(0,2500, 250)),
                     limits = c(0,2500)) +
  theme_bw()


preco <- corvina %>%
  group_by(pescado, periodo) %>%
  mutate(valor_estimado_real = sum(valor_estimado_real),
         valor_estimado = sum(valor_estimado),
         kg = sum(kg),
         preco_real = valor_estimado_real / kg,
         preco_nominal = valor_estimado / kg) %>%
  ggplot() +
  geom_line(aes(x = periodo, y = preco_real), color = "darkgreen") +
  geom_line(aes(x = periodo, y = preco_nominal), color = "darkred", alpha=0.5) +
  labs(y = "",
       x="",
       title = "Valor unitário (R$/Kg)") + 
  scale_y_continuous(breaks = seq(0, 9.5, 1),
                     labels = paste0("R$", seq(0, 9.5, 1), ",00"),
                     limits = c(0, 9.5)) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  theme_bw()
```

</details>

![](modelo_mqo_simples_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

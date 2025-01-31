Manipulando tabela estraída da Propesq
================

<style>
div.hidecode + pre {display: none}
</style>

### Setup

**Bibliotecas**

``` r
library(tidyverse)
library(patchwork)
```

**Dataset**

``` r
propesq <- "../data/propesq.rds" %>%
  read_rds() %>%
  mutate(periodo = sprintf("%d-%02d-01", ano, mes) %>% lubridate::ymd())
```

**Tema para gráfico**

``` r
tema <- function(titulo="", nota="", facet) {
  
  t <- list(
            theme(axis.text.x = element_text(angle=90)),
            facet_wrap(facets=facet, scales="free_y", ncol = 1),
            labs(title = titulo,
                 caption = nota) 
        )
  
  return(t)

}
```

### Municípios

<details>

<summary> Ver Código </summary>

``` r

plot.kg <- propesq %>% 
  group_by(municipio, periodo) %>%
  summarise(kg = sum(kg)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = periodo, y = kg/1000, color = factor(municipio)), show.legend = F) + 
  tema(titulo = "Kg", nota = "Em milhares", facet="municipio")

plot.vlr <- propesq %>% 
  group_by(municipio, periodo) %>%
  summarise(valor_estimado = sum(valor_estimado)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x = periodo, y = valor_estimado/1000, color = factor(municipio)), show.legend = F) +
  tema(titulo = "R$", nota = "Em milhares", facet="municipio")

plot.dscg <- propesq %>% 
  group_by(municipio, periodo) %>%
  summarise(qtd_descargas = sum(qtd_descargas)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x=periodo, y=qtd_descargas, color=factor(municipio)), show.legend = F) +
  tema(titulo = "Descargas", facet="municipio")

plot.un_prd <- propesq %>% 
  group_by(municipio, periodo) %>%
  summarise(qtd_unidades_produtivas = sum(qtd_unidades_produtivas)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x=periodo, y=qtd_unidades_produtivas, color=factor(municipio)), show.legend = F) +
  tema(titulo = "Un. Produtivas", facet="municipio") 
```

</details>

<img src="exemplo_manipulando_dados_propesq_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

### Nível taxonômico

<details>

<summary> Ver Código </summary>

``` r
plot.preco <- propesq %>%
  na.omit(nivel_taxonomico) %>%
  filter(nivel_taxonomico != "Equinodermo") %>%
  group_by(nivel_taxonomico, periodo) %>%
  summarise(preco = sum(valor_estimado) / sum(kg)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x=periodo, y=preco, color=factor(nivel_taxonomico)), show.legend = F) +
  tema(titulo = "Preço", facet="nivel_taxonomico") 

plot.kg <- propesq %>%
  na.omit(nivel_taxonomico) %>%
  filter(nivel_taxonomico != "Equinodermo") %>%
  group_by(nivel_taxonomico, periodo) %>%
  summarise(kg = sum(kg)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x=periodo, y=kg/1000, color=factor(nivel_taxonomico)), show.legend = F) +
  tema(titulo = "Kg", facet="nivel_taxonomico") 

plot.produtividade_barco <-  propesq %>%
  na.omit(nivel_taxonomico) %>%
  filter(nivel_taxonomico != "Equinodermo") %>%
  group_by(nivel_taxonomico, periodo) %>%
  summarise(produtividade = sum(kg) / sum(qtd_unidades_produtivas)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x=periodo, y=produtividade, color=factor(nivel_taxonomico)), show.legend = F) +
  tema(titulo = "Produtividade", facet="nivel_taxonomico") 
```

</details>

<img src="exemplo_manipulando_dados_propesq_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

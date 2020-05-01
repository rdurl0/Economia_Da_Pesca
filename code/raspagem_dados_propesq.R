library(RSelenium)
library(XML)
library(tidyverse)

tabela_final <- function(ano, my_dir = tempdir(), delete_old = TRUE) {
  
  # my_dir: onde o .csv será salvo ---------------------------------------------
  eCaps <- list(
    chromeOptions = 
      list(prefs = list(
        "profile.default_content_settings.popups" = 0L,
        "download.prompt_for_download" = FALSE,
        "download.default_directory" = my_dir
           )
      )
  )
  
  if(delete_old == TRUE) {
    
    my_dir %>% 
      list.files(pattern = "relatorio30") %>%
      map(~paste0(my_dir, "\\", .x)) %>%
      map(~unlink(.x))
    
    cat("\nArquivos antigos foram apagados\n\n")
    
  } else {
    
    cat("\nArquivos antigos NÃO foram apagados\n\n")
    
  }
  
  # Carrega RSelenium ----------------------------------------------------------
  driver <- rsDriver(
    browser=c("chrome"),
    chromever="81.0.4044.69",
    port = sample.int(9999, 1),
    extraCapabilities = eCaps
  )
  
  # Acessando o site -----------------------------------------------------------
  remDr <- driver[["client"]]
  
  remDr$navigate("http://www.propesq.pesca.sp.gov.br/usuarioexterno/")
  
  # Seleciona caixa de país ----------------------------------------------------
  remDr$findElement(using = "css", ".radio > label:nth-child(1)")$clickElement()
  
  # Seleciona o Estado ---------------------------------------------------------
  busca_estado <- remDr$findElement(using = "xpath", '//*[@id="entidade_usuarioexterno_codEstado"]')
  busca_estado$clickElement()
  busca_estado_sp <- busca_estado$findChildElement(using = 'xpath', '//*[@id=":p"]/div')$clickElement()
  
  # Busca o perfil -------------------------------------------------------------
  busca_perfil <- remDr$findElement(using = 'xpath', '//*[@id="entidade_usuarioexterno_indPerfil"]')
  busca_perfil$clickElement()
  busca_perfil_outros <- busca_perfil$findChildElement(using = 'xpath', '//*[@id=":13"]/div')$clickElement()
  
  # Acessa o relatório ---------------------------------------------------------
  acessar_relatorios <- remDr$findElement(using = 'xpath', '//*[@id="formulario-usuarioexterno"]/div[7]/button')
  acessar_relatorios$clickElement()
  
  Sys.sleep(10)
  
  # Página seguinte ------------------------------------------------------------
  
  # Agrupadores ----------------------------------------------------------------
  # 0 Ano
  # 1 mes
  # 2 municipio
  # 3 aparelho_de_pesca
  # 4 nivel_taxonomico
  # 5 pescado
  0:5 %>%
    map(~sprintf('//*[@id="coluna%s"]', .x)) %>%
    map(~remDr$findElement(using = 'xpath', .x)$clickElement())
  
  # Variáveis ------------------------------------------------------------------
  # 0: kg_periodo
  # 1: n_de_descargas_do_periodo
  # 2: n_de_unidades_produtivas
  # 3: valor_estimado_no_periodo
  0:3 %>%
    map(~sprintf('//*[@id="variavel%s"]', .x)) %>%
    map(~remDr$findElement(using = 'xpath', .x)$clickElement())
  
  # Tabela Final ---------------------------------------------------------------
  dt_ini <- paste0("01/", ano)
  dt_fim <- paste0("12/", ano)
  
  data_inicial <- remDr$findElement(using = "id", 'dataInicial')
  data_inicial$clearElement()
  data_inicial$sendKeysToElement(list(dt_ini))
  
  data_final <- remDr$findElement(using = "id", 'dataFinal')
  data_final$clearElement()
  data_final$sendKeysToElement(list(dt_fim))
  
  gera_tbl_final <- remDr$findElement(using = 'xpath', '//*[@id="acaoCSV"]')
  gera_tbl_final$clickElement()
  
  Sys.sleep(20)
  
  remDr$close()
  
  # clean data -----------------------------------------------------------------
  tbl_path <- list.files(my_dir, pattern="relatorio30") %>% paste0(my_dir, "\\", .) 
  
  cat(paste("Arquivo:\n", list.files(my_dir, pattern="relatorio30")[which.max(file.mtime(tbl_path))],
            "\nAno\n:", ano,
            "\nFoi baixado no diretório:\n", my_dir, 
            "\nProcessando dados...\n"))
  
  tbl <- read_delim(
    tbl_path[which.max(file.mtime(tbl_path))],
    ";",
    escape_double = FALSE,
    locale = locale(encoding = "Latin1"),
    trim_ws = TRUE
  ) 
  
  names(tbl) <- c(
    "ano",
    "mes",
    "municipio",
    "aparelho_de_pesca",
    "nivel_taxonomico",
    "pescado",
    "kg",
    "qtd_descargas",
    "qtd_unidades_produtivas",
    "valor_estimado"
  )
  
  tbl <- tbl %>%
    mutate_at(
      c("ano", "mes", "qtd_descargas", "qtd_unidades_produtivas"),
      ~ as.integer(.)
    ) %>%
    mutate_at(
      c("kg","valor_estimado"), 
      ~ str_remove_all(.,"\\.|\\$|[:blank:]|[:alpha:]") %>%
        str_replace(",", ".") %>% 
        as.numeric() 
    )
  
  return(tbl)
  
}

# Loop para baixar todas tbls --------------------------------------------------
propesq <- tibble(
  ano = integer(),
  mes = integer(),
  municipio = character(),
  aparelho_de_pesca = character(),
  nivel_taxonomico = character(),
  pescado = character(),
  kg = double(),
  qtd_descargas = integer(),
  qtd_unidades_produtivas = integer(),
  valor_estimado = double()
)

L <- c()

valida <- tibble(ano = integer(),
                 `Linhas (propesq)` = integer(),
                 `Linhas (tbl)` = integer())

for(i in 1998:2020) {
  
  tbl <- tabela_final(i, delete_old = FALSE)
  
  L <- c(L,nrow(tbl))
  
  propesq <- bind_rows(propesq, tbl)
  
  valida <- propesq %>% 
    group_by(ano) %>%
    summarise(`Linhas (propesq)` = n()) %>%
    mutate(`Linhas (tbl)` = L)
  
  print(valida)
  
  Sys.sleep(5)
}

propesq <- mutate(periodo = sprintf("%d-%02d-01", ano, mes) %>% lubridate::ymd())

#write_rds(propesq, "./data/propesq.rds")

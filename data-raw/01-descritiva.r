print(1+1)

devtools::load_all()

da_cposg_sem_movs <- readr::read_rds("data-raw/da_cposg_sem_movs.rds") |>
  dplyr::group_by(orgao_julgador) |>
  dplyr::mutate(n_cam = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::filter(n_cam > 100) |>
  dplyr::mutate(orgao_julgador = stringr::str_replace(
    orgao_julgador, "^([0-9]ª)", "0\\1"
  ))

aux_decisoes <- da_cposg_sem_movs |>
  tidyr::unnest(decisoes, names_sep = "_") |>
  dplyr::arrange(dplyr::desc(decisoes_data)) |>
  dplyr::filter(
    decisoes_data >= "2019-01-01",
    decisoes_data < "2023-01-01",
    !is.na(decisoes_decisao)
  ) |>
  dplyr::distinct(file, .keep_all = TRUE)

readr::write_rds(aux_decisoes, "data-raw/rds/aux_decisoes.rds")

# composição -------------
aux_composicao <- da_cposg_sem_movs |>
  dplyr::select(file, processo, composicao) |>
  dplyr::filter(purrr::map_lgl(composicao, \(x) nrow(x) > 0, .progress = TRUE)) |>
  tidyr::unnest(composicao) |>
  dplyr::mutate(magistrado = stringr::str_remove_all(magistrado, " \\(.*"))

readr::write_rds(aux_composicao, "data-raw/rds/aux_composicao.rds")



# No tempo ---------



# Decisão ---------

regex_parcialmente_provido <- "parcial|em parte"
regex_provido <- "recurso provido|deram(-lhe)? provimento|provimento ao recurso|acolheram o recurso"
regex_negaram <- "negaram|mantiveram, na [ií]ntegra|n[aã]o provimento|nego provimento|mantiveram o julgado"
regex_nao_conhecido <- "n[aã]o conhec"
regex_extin <- "punibili"

rx_unan <- stringr::regex("v,u,|v ?\\. ?u|vu\\.?$|v\\.\\.u|\\(vu\\)| vu\\. |un[aâ]nim[ei]", TRUE)
rx_maioria <- stringr::regex("maioria", TRUE)

# TODO continuar classificador
aux_decisoes_cat <- aux_decisoes |>
  dplyr::select(file, dec = decisoes_decisao, decision = decisoes_decisao) |>
  dplyr::mutate(
    decision = stringr::str_to_lower(decision),
    decision = dplyr::case_when(
      stringr::str_detect(decision, regex_parcialmente_provido) ~ "Parcialmente",
      stringr::str_detect(decision, regex_negaram) ~ "Negaram",
      stringr::str_detect(decision, regex_provido) ~ "Provido",
      stringr::str_detect(decision, regex_nao_conhecido) ~ "Não conhecido",
      stringr::str_detect(decision, regex_extin) ~ "Punibilidade Extinta",
      .default = "Outros"
    )
  ) |>
  dplyr::mutate(unanimidade = dplyr::case_when(
    stringr::str_detect(dec, rx_unan) ~ "Unânime",
    stringr::str_detect(dec, rx_maioria) ~ "Maioria",
    .default = "Sem informação"
  ))

readr::write_rds(aux_decisoes_cat, "data-raw/rds/aux_decisoes_cat.rds")

aux_decisoes_cat |>
  dplyr::filter(decision %in% c("Negaram", "Provido", "Parcialmente")) |>
  dplyr::count(decision) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::arrange(dplyr::desc(prop))

aux_decisoes_cat |>
  dplyr::inner_join(da_cposg_sem_movs, "file") |>
  dplyr::filter(decision %in% c("Negaram", "Provido", "Parcialmente")) |>
  dplyr::count(orgao_julgador, decision) |>
  dplyr::group_by(orgao_julgador) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::arrange(orgao_julgador) |>
  dplyr::ungroup()

# aux_decisoes_cat |>
#   dplyr::filter(decision == "Outros") |>
#   dplyr::filter(
#     !stringr::str_detect(dec, stringr::regex("dilig|prejudic|desist", TRUE))
#   ) |>
#   with(sample(dec, 10))


# Partes ---------

rx_mp <- stringr::regex("minist[eé]", TRUE)
aux_partes <- da_cposg_sem_movs |>
  dplyr::select(file, processo, partes) |>
  tidyr::unnest(partes, names_sep = "_") |>
  dplyr::mutate(mp = stringr::str_detect(partes_nome, rx_mp)) |>
  dplyr::filter(mp) |>
  dplyr::mutate(polo_mp = dplyr::case_when(
    partes_parte %in% c("Apelado") ~ "Passivo",
    partes_parte %in% c("Apelante") ~ "Ativo",
    partes_parte %in% c("AptoApte|ApteApdo") ~ "Ambos"
  )) |>
  dplyr::filter(!is.na(polo_mp)) |>
  dplyr::select(file, processo, polo_mp)

readr::write_rds(aux_partes, "data-raw/rds/aux_partes.rds")



?ggplot2::label_both

aux_partes |>
  dplyr::count(partes_parte, partes_papel, sort = TRUE) |>
  print(n = 100)


aux_partes |>
  dplyr::filter(!mp) |>
  dplyr::count(partes_nome, sort = TRUE) |>
  print(n = 100)


da_cposg_sem_movs |>
  dplyr::glimpse()

da_cposg_sem_movs |>
  dplyr::count(orgao_julgador, sort = TRUE) |>
  print(n = 100)

da_composicao |>
  dplyr::count(magistrado, sort = TRUE)

da_composicao |>
  dplyr::count(participacao)

count_revisor <- da_composicao |>
  dplyr::filter(participacao == "Revisor") |>
  dplyr::count(magistrado, sort = TRUE)

da_composicao |>
  dplyr::filter(participacao == "Relator") |>
  dplyr::count(magistrado, sort = TRUE) |>
  dplyr::inner_join(count_revisor, "magistrado")


# Comarcas ----

muni_sf <- geobr::read_municipality("SP")

depara <- abjMaps::d_sf_tjsp$sf[[1]] |>
  dplyr::as_tibble() |>
  dplyr::transmute(
    code_muni = as.numeric(as.character(cd_geocmu)),
    comarca
  )

comarca_sf <- muni_sf |>
  dplyr::inner_join(depara, "code_muni") |>
  dplyr::group_by(comarca) |>
  dplyr::summarise(n_muni = dplyr::n()) |>
  sf::st_simplify(dTolerance = 200)

readr::write_rds(comarca_sf, "data-raw/rds/comarca_sf.rds")

aux_comarca_count <- da_cposg_sem_movs |>
  dplyr::count(origem, sort = TRUE) |>
  dplyr::mutate(
    comarca = stringr::str_extract(origem, "(?<=Comarca d[eao] )[^/]+"),
    comarca = stringr::str_squish(comarca)
  ) |>
  dplyr::count(origem, comarca, sort = TRUE) |>
  dplyr::mutate(uf = "SP") |>
  munifacil::limpar_colunas(comarca, uf) |>
  dplyr::mutate(muni_join = dplyr::case_when(
    muni_join == "mogi-mirim" ~ "mogi mirim",
    muni_join == "mogi-guacu" ~ "mogi guacu",
    muni_join == "ipaucu" ~ "ipaussu",
    muni_join == "fd aruja" ~ "aruja",
    muni_join == "santa barbara d oeste" ~ "santa barbara doeste",
    muni_join == "foro de ouroeste" ~ "ouroeste",
    .default = muni_join
  )) |>
  munifacil::incluir_codigo_ibge() |>
  dplyr::select(origem, comarca, id_municipio)

aux_comarca <- da_cposg_sem_movs |>
  dplyr::inner_join(
    dplyr::select(aux_decisoes, file, decisoes_data),
    "file"
  ) |>
  dplyr::left_join(aux_comarca_count, "origem") |>
  dplyr::mutate(code_muni = as.numeric(id_municipio)) |>
  dplyr::select(-comarca) |>
  dplyr::left_join(depara, "code_muni") |>
  dplyr::select(file, comarca, decisoes_data)

readr::write_rds(aux_comarca, "data-raw/rds/aux_comarca.rds")


# Magistrados -----

re_formacao <- "(?<=Bacharel (em|pela) )([^(]+)\\(([0-9]{4})\\)"

aux_magistrado <- readxl::read_excel("data-raw/xlsx/desembargadores_criminal.xlsx") |>
  janitor::clean_names() |>
  purrr::set_names(stringr::str_replace_all, "_[a-z]{2}_", "_") |>
  dplyr::transmute(
    nome_full = nome,
    nome = clean_nm(nome),
    formacao_parsed = stringr::str_match_all(formacao, re_formacao),
    faculdade = purrr::map_chr(formacao_parsed, \(x) x[, 3][1]),
    faculdade = stringr::str_squish(faculdade),
    faculdade = stringr::str_remove(faculdade, "Faculdade de "),
    faculdade = stringr::str_replace_all(faculdade, "d[aeo]", "pela"),
    faculdade = dplyr::case_when(
      faculdade == "Direito pela PUC-SP" ~ "PUC-SP",
      faculdade == "Direito pela FMU" ~ "FMU",
      stringr::str_detect(faculdade, "[Ss]antos") ~ "Unisantos",
      stringr::str_detect(faculdade, "FDSBC") ~ "FDSBC",
      stringr::str_detect(faculdade, "Mack") ~ "Mackenzie",
      stringr::str_detect(faculdade, "USP") ~ "USP",
      stringr::str_detect(faculdade, "[Cc]ampinas") ~ "PUC-Campinas",
      TRUE ~ "Outra"
    ),

    # Pós graduação
    tem_pos = dplyr::case_when(
      stringr::str_detect(formacao, ";") ~ "Sim", TRUE ~ "Não"
    ),

    # Tempo de formacao
    ano_formado = purrr::map_chr(formacao_parsed, ~.x[,4][1]),
    tempo_form = as.integer(idade_ano(ano_formado)),

    # Tempo de tribunal
    tempo_2inst = as.integer(idade_ano(ingresso_tribunal)),

    dt_nasc = as.Date(data_nascimento),
    idade = as.numeric(Sys.Date() - dt_nasc) / 365.242,
    local_nascimento = local_nascimento,
    capital = dplyr::case_when(
      local_nascimento == "São Paulo - SP" ~ "Capital",
      TRUE ~ "Fora da Capital"
    ),
    # Origem da carreira
    origem = dplyr::case_when(
      stringr::str_detect(origem, "Magistr") ~ "Magistratura",
      stringr::str_detect(origem, "Advoc") ~ "Advocacia",
      stringr::str_detect(origem, "Min") ~ "MP"
    )
  ) |>
  tidyr::separate(local_nascimento, c("municipio", "uf"), sep = " ?- ?") |>
  munifacil::limpar_colunas(municipio, uf) |>
  dplyr::mutate(muni_join = dplyr::case_when(
    muni_join == "monte santo" ~ "monte santo de minas",
    .default = muni_join
  )) |>
  munifacil::incluir_codigo_ibge() |>
  dplyr::select(
    -c(manual, atencao, dplyr::starts_with("existia")),
    -c(muni_join, uf_join, formacao_parsed)
  ) |>
  dplyr::mutate(nome = dplyr::case_when(
    nome == "ABEN COUTINHO" ~ "PAIVA COUTINHO",
    nome == "ADALBERTO FILHO" ~ "CAMARGO FILHO",
    nome == "AGUINALDO FILHO" ~ "FREITAS FILHO",
    nome == "AMARO FILHO" ~ "AMARO THOME",
    nome == "ANTONIO ANDRADE" ~ "MACHADO ANDRADE",
    nome == "ANTONIO CASTELLO" ~ "ALVARO CASTELLO",
    nome == "ANTONIO OLIVEIRA" ~ "SERGIO COELHO",
    nome == "ANTONIO RIBEIRO" ~ "TRISTAO RIBEIRO",
    nome == "ANTONIO SAMPAIO" ~ "ALMEIDA SAMPAIO",
    nome == "CAMILO ALMEIDA" ~ "CAMILO LELLIS",
    nome == "CARLOS MANAS" ~ "VICO MANAS",
    nome == "CASSIANO ROCHA" ~ "ZORZI ROCHA",
    nome == "DINIZ CRUZ" ~ "DINIZ FERNANDO",
    nome == "EUVALDO FILHO" ~ "EUVALDO CHAIB",
    nome == "FABIO LEITAO" ~ "POCAS LEITAO",
    nome == "FRANCISCO SOUZA" ~ "FRANCISCO ORLANDO",
    nome == "GERALDO FRANCO" ~ "PINHEIRO FRANCO",
    nome == "GERALDO SILVEIRA" ~ "GERALDO WOHLERS",
    nome == "GILBERTO GARCIA" ~ "LEME GARCIA",
    nome == "JOSE COGAN" ~ "DAMIAO COGAN",
    nome == "JOSE MANFRE" ~ "ENCINAS MANFRE",
    nome == "JOSE NERY" ~ "SOUZA NERY",
    nome == "JOSE NETO" ~ "DE SANTOS",
    nome == "JOSE PEREIRA" ~ "BORGES PEREIRA",
    nome == "LAERTE SAMPAIO" ~ "LAERTE MARRONE",
    nome == "LUIS NETO" ~ "LUIS MELLO",
    nome == "LUIZ GONCALVES" ~ "FIGUEIREDO GONCALVES",
    nome == "LUIZ NETO" ~ "TOLOZA NETO",
    nome == "LUIZ ROCHA" ~ "OTAVIO ROCHA",
    nome == "LUIZ SIQUEIRA" ~ "AUGUSTO SIQUEIRA",
    nome == "MARCOS SILVA" ~ "MARCOS CORREA",
    nome == "MARIA ALMEIDA" ~ "RACHID ALMEIDA",
    nome == "NILO PERPETUO" ~ "CARDOSO PERPETUO",
    nome == "NILSON SOUZA" ~ "XAVIER SOUZA",
    nome == "PERICLES JUNIOR" ~ "PERICLES PIZA",
    nome == "REINALDO CARVALHO" ~ "REINALDO CINTRA",
    nome == "ROBERTO CARVALHO" ~ "FRANCA CARVALHO",
    nome == "ROBERTO NETO" ~ "GRASSI NETO",
    nome == "ROBERTO SOLIMENE" ~ "COSTABILE SOLIMENE",
    nome == "RONALDO SILVA" ~ "MOREIRA SILVA",
    nome == "WALDIR JUNIOR" ~ "NUEVO CAMPOS",
    .default = nome
  ))

readr::write_rds(aux_magistrado, "data-raw/rds/aux_magistrado.rds")

depara_relator <- aux_composicao |>
  dplyr::distinct(magistrado) |>
  dplyr::mutate(
    nome = clean_nm(magistrado)
  )

readr::write_rds(depara_relator, "data-raw/rds/depara_relator.rds")

aux_composicao |>
  dplyr::count(magistrado) |>
  dplyr::mutate(
    nome = clean_nm(magistrado)
  ) |>
  dplyr::anti_join(aux_magistrado, "nome") |>
  dplyr::arrange(dplyr::desc(n)) |>
  knitr::kable()


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
  )

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



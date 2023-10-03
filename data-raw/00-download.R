## Download raw data from TJSP site

# asjdjakhd
download_year <- function(ano) {
  path <- stringr::str_glue("data-raw/cjsg/{ano}")
  fs::dir_create(path)
  tjsp::baixar_cjsg(
    diretorio = path,
    classe = "417",
    inicio = stringr::str_glue("01/01/{ano}"),
    fim = stringr::str_glue("31/12/{ano}")
  )
}

download_year(2022)
download_year(2021)
download_year(2020)
download_year(2019)


download_day <- function(date) {
  path <- stringr::str_glue("data-raw/cjsg/{date}")
  if (!fs::dir_exists(path)) {
    usethis::ui_info("Downloading {date}...")
    fs::dir_create(path)
    tjsp::baixar_cjsg(
      diretorio = path,
      classe = "417",
      inicio = format(date, "%d/%m/%Y"),
      fim = format(date, "%d/%m/%Y")
    )
  }
}

ja_foi <- fs::dir_ls("data-raw/cjsg", recurse = TRUE, type = "file") |>
  dirname()

tamanho_zero <- fs::dir_ls("data-raw/cjsg", recurse = TRUE, type = "file") |>
  fs::file_info() |>
  dplyr::filter(size == 0) |>
  with(path) |>
  dirname()

fs::dir_ls("data-raw/cjsg") |>
  setdiff(ja_foi) |>
  append(tamanho_zero) |>
  purrr::walk(
    \(x) if (fs::dir_exists(x)) fs::dir_delete(x),
    .progress = TRUE
  )

safe_download_day <- purrr::possibly(download_day)
todos_dias <- seq(as.Date("2019-01-01"), as.Date("2023-08-31"), by = "day")
purrr::walk(todos_dias, safe_download_day, .progress = TRUE)

all_files <- fs::dir_ls(
  "data-raw/cjsg", recurse = TRUE, type = "file"
)
length(all_files)

da_cjsg <- purrr::map(all_files, tjsp::tjsp_ler_cjsg, .progress = TRUE) |>
  purrr::list_rbind(names_to = "file")

readr::write_rds(da_cjsg, "data-raw/da_cjsg.rds")
arrow::write_parquet(da_cjsg, "data-raw/da_cjsg.parquet")

da_cjsg |>
  dplyr::mutate(ano = lubridate::year(data_publicacao)) |>
  dplyr::count(ano)

da_cjsg |>
  dplyr::mutate(ano = lubridate::year(data_julgamento)) |>
  dplyr::count(ano)

da_cjsg |>
  dplyr::mutate(anomes = lubridate::floor_date(data_julgamento, "month")) |>
  dplyr::count(anomes) |>
  ggplot2::ggplot(ggplot2::aes(anomes, n)) +
  ggplot2::geom_line(linewidth = 2) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::labs(x = NULL, y = NULL, title = "Quantidade de julgamentos por mês") +
  ggplot2::theme_minimal()

da_cjsg |>
  dplyr::mutate(anomes = lubridate::floor_date(data_julgamento, "month")) |>
  dplyr::count(anomes) |>
  dplyr::mutate(anomes = tsibble::yearmonth(anomes)) |>
  tsibble::as_tsibble(index = anomes) |>
  tsibble::fill_gaps() |>
  feasts::gg_season(
    y = n, pal = viridis::viridis(5, begin = .1, end = .8),
    linewidth = 2, polar = TRUE
  ) +
  ggplot2::theme_minimal()


# download cpopg ----
da_cjsg <- readr::read_rds("data-raw/da_cjsg.rds")

id_processos <- unique(da_cjsg$processo)
readr::write_rds(id_processos, "data-raw/id_processos.rds")

length(id_processos)
fs::dir_create("data-raw/cposg")

download_cpopg <- function(id) {
  #path <- stringr::str_glue("data-raw/cpopg/{id}.html")
  if (!fs::file_exists(path)) {
    usethis::ui_info("Downloading {id}...")
    tjsp::baixar_cpopg(id, path)
  }
}

tictoc::tic()
purrr::walk(
  sample(id_processos, 1000),
  \(x) tjsp::tjsp_baixar_cposg(x, "data-raw/cposg"),
  .progress = TRUE
)
tictoc::toc()

# parse cpopg ----
arqs_cposg <- fs::dir_ls("data-raw/cposg")
safe <- purrr::possibly(lex::tjsp_cposg_parse, tibble::tibble(erro = "erro"))
fs::dir_create("data-raw/cposg_chunks")
length(arqs_cposg)

safe(arqs_cposg[1000]) |>
  dplyr::glimpse()

len_chunk <- 9000
chunks <- split(arqs_cposg, ceiling(seq_along(arqs_cposg) / len_chunk))
length(chunks)

purrr::iwalk(chunks, \(ch, id_chunk) {
  usethis::ui_info("Chunk {id_chunk}...")
  f_chunk <- glue::glue(
    "data-raw/cposg_chunks/{sprintf('%02d', as.numeric(id_chunk))}.rds"
  )
  if (!file.exists(f_chunk)) {
    suppressMessages({
      res_chunk <- purrr::map(ch, safe, .progress = TRUE) |>
        purrr::list_rbind(names_to = "file")
    })
    readr::write_rds(res_chunk, f_chunk)
  }
})

da_cposg_sem_movs <- fs::dir_ls("data-raw/cposg_chunks") |>
  purrr::map(\(x) {
    readr::read_rds(x) |>
      dplyr::select(-movimentacoes)
  }, .progress= TRUE) |>
  purrr::list_rbind()
readr::write_rds(da_cposg_sem_movs, "data-raw/da_cposg_sem_movs.rds")

dplyr::glimpse(da_cposg_sem_movs)


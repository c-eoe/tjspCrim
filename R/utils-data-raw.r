idade <- function(data_nascimento) {
  tod <- Sys.Date()
  nasc <- as.Date(as.numeric(data_nascimento), origin = "1900-01-01")
  as.numeric(tod - nasc) / 365.242
}

idade_ano <- function(data_nascimento) {
  ano <- lubridate::year(Sys.Date())
  ano - as.numeric(stringr::str_extract(data_nascimento, "[0-9]{4}"))
}

clean_nm <- function(nome) {
  nm <- nome |>
    stringr::str_to_upper() |>
    abjutils::rm_accent() |>
    stringr::str_squish()
  f <- stringr::str_extract(nm, "^[A-Z]+")
  l <- stringr::str_extract(nm, "[A-Z]+$")
  paste(f, l)
}
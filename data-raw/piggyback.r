
print(1+1)

piggyback::pb_new_release(tag = "rds")


piggyback::pb_upload(
  "data-raw/rds/da_cjsg.rds",
  tag = "rds",
  overwrite = TRUE
)
piggyback::pb_upload(
  "data-raw/rds/da_cposg_sem_movs.rds",
  tag = "rds"
)

piggyback::pb_upload(
  "data-raw/rds/aux_partes.rds",
  tag = "rds"
)
piggyback::pb_upload(
  "data-raw/rds/aux_decisoes_cat.rds",
  tag = "rds"
)

piggyback::pb_upload(
  "data-raw/rds/aux_decisoes.rds",
  tag = "rds"
)
piggyback::pb_upload(
  "data-raw/rds/aux_composicao.rds",
  tag = "rds"
)

piggyback::pb_new_release(tag = "parquet")

piggyback::pb_upload(
  "data-raw/parquet/da_cjsg.parquet",
  tag = "parquet"
)

piggyback::pb_release_delete(tag = "rds_cposg_full")
piggyback::pb_releases()
piggyback::pb_new_release(tag = "rds_cposg_full")

lista_cposg_chunks <- fs::dir_ls("data-raw/cposg_chunks")

purrr::walk(
  lista_cposg_chunks,
  \(x) piggyback::pb_upload(x, tag = "rds_cposg_full", overwrite = TRUE),
  .progress = TRUE
)

piggyback::pb_upload(
  "data-raw/cpopg_chunk_01.zip",
  tag = "html"
)


# TODO: Escrever código para gerar chunks HTML
piggyback::pb_new_release(tag = "html")

# path dependence
# dados?
# -- processo de tomada de decisão


lista_cjsg <- fs::dir_ls("data-raw/cjsg")

piggyback::pb_upload(
  "data-raw/cjsg.zip",
  tag = "html"
)


lista_cposg_chunks <- fs::dir_ls("data-raw/cposg_chunks")
arqs_cposg <- fs::dir_ls("data-raw/cposg")
len_chunk <- 9000
chunks <- split(arqs_cposg, ceiling(seq_along(arqs_cposg) / len_chunk))

purrr::iwalk(
  chunks, \(x, y) {
    zip::zip(
      zipfile = sprintf("../cposg_chunks_html/cposg_chunk_%02d.zip", as.numeric(y)),
      files = basename(x),
      include_directories = FALSE
    )
  },
  .progress = TRUE
)

cposg_chunks_html <- fs::dir_ls("data-raw/cposg_chunks_html")
purrr::walk(
  cposg_chunks_html[-1],
  \(x) piggyback::pb_upload(x, tag = "html"),
  .progress = TRUE
)

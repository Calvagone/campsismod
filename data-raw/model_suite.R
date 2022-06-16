
pattern <- "[^.*\\.R]$"

model_suite <- list()
categories <- c("literature", "nonmem", "other", "pd", "pk", "tmdd")

for (category in categories) {
  model_suite[[category]] <- list()
  for (model in list.files(path=paste0("data-raw/model_suite/", category), pattern=pattern)) {
    model_suite[[category]][[model]] <- campsismod::read.campsis(file=paste0("data-raw/model_suite/", category, "/", model))
  }
}

usethis::use_data(model_suite, overwrite=TRUE)

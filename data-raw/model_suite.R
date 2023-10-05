
pattern <- "[^.*\\.R]$"

model_suite <- list()
categories <- c("pk", "pd", "tmdd", "nonmem", "literature", "testing")

for (category in categories) {
  model_suite[[category]] <- list()
  
  if (category != "testing") {
    for (model in list.files(path=paste0("data-raw/model_suite/", category), pattern=pattern)) {
      model_suite[[category]][[model]] <- campsismod::read.campsis(file=paste0("data-raw/model_suite/", category, "/", model))
    }
  } else {
    subCategories <- c("nonmem", "other", "pk")
    for (subCategory in subCategories) {
      model_suite[[category]][[subCategory]] <- list()
      
      for (model in list.files(path=paste0("data-raw/model_suite/", category, "/", subCategory), pattern=pattern)) {
        model_suite[[category]][[subCategory]][[model]] <- campsismod::read.campsis(file=paste0("data-raw/model_suite/", category, "/", subCategory, "/", model))
      }
    }
  }
}

usethis::use_data(model_suite, overwrite=TRUE)
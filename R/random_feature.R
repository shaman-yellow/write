random_feature <-
  function(
           feature = "songci",
           number = 10,
           suffix = ".scel.dic"
           ){
    db = paste0(.wdic, "/", feature, suffix)
    db = read_dic(db)
    item = dplyr::slice(db, sample(1:nrow(db), number))
    return(item)
  }
random_all <-
  function(
           number = 10,
           suffix = ".scel.dic"
           ){
    features <- list.files(.wdic) %>%
      strsplit(split = suffix) %>%
      unlist()
    items <- pbapply::pblapply(features, random_feature) %>%
      data.table::rbindlist() %>%
      dplyr::slice(sample(1:nrow(.), number))
    return(items)
  }

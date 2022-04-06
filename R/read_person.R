read_person <-
  function(
           name = "SP",
           type = "person",
           path = paste0(.wsave, "/", type),
           vim = T
           ){
    name <- substitute(name)
    name <- as.character(name)
    if(type %in% .wmeta$parent_id == F){
      .wmeta <<- dplyr::bind_rows(.wmeta, c(feature = "zone", parent_id = type))
    }
    if(file.exists(path) == F){
      dir.create(path)
    }
    file <- paste0(path, "/", name, ".tsv")
    if(file.exists(file) == T){
      if(vim == T)
        read_tsv(file) %>%
          as_tibble() %>%
          print(n = nrow(.))
    }else{
      meta <- dplyr::filter(.wmeta, parent_id == type)
      meta <- dplyr::select(meta, feature)
      data <- dplyr::mutate(meta, depict = "")
      write_tsv(data, file)
      if(vim == T)
        read_tsv(file) %>%
          as_tibble() %>%
          print(n = nrow(.))
    }
  }

load_into <-
  function(
           list,
           exclude = c("outline", "scenes", "annotation")
           ){
    df <- data.table::rbindlist(list, idcol = T, fill = T) %>%
      dplyr::rename(z_from = .id) %>%
      dplyr::filter(!(type %in% exclude)) %>%
      dplyr::as_tibble()
    list <- by_group_as_list(df, "type")
    pbapply::pblapply(list, base_load_into)
    cat("Done\n")
  }
base_load_into <-
  function(
           df
           ){
    list <- by_group_as_list(df, "tag")
    lapply(list, base2_merge)
    return()
  }
base2_merge <- 
  function(
           df
           ){
    type <- df[1,][["type"]]
    tag <- df[1,][["tag"]]
    file <- paste0(.wsave, "/", type, "/", tag, ".tsv")
    if(file.exists(file)){
      origin <- read_tsv(file)
      if("z_from" %in% colnames(origin))
        origin <- dplyr::select(origin, !z_from)
    }else{
      get_unit(name = tag, type = type, vim = F)
      origin <- read_tsv(file)
    }
    df <- dplyr::select(df, feature, depict, z_from)
    ## -------------------------------------- 
    ## merge
    df <- merge(df, origin, by = c("feature", "depict"), all = T, sort = F)
    ## collate
    list <- by_group_as_list(df, "feature")
    list <- lapply(list, remove_nn)
    list <- lapply(list, distinct,
                   depict,
                   .keep_all = T)
    df <- data.table::rbindlist(list)
    df <- summarise_all(df, na_as_blank)
    write_tsv(df, file)
  }
remove_nn <- 
  function(
           df
           ){
    if(nrow(df) >= 2){
      df <- dplyr::filter(df, is.na(depict)==F & (depict != ""))
      return(df)
    }else{
      return(df)
    }
  }
is.nn <- 
  function(
           x
           ){
    if(is.na(x)){
      return(T)
    }else if(x == ""){
      return(T)
    }else{
      return(F)
    }
  }

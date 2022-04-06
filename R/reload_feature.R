reload_feature <-
  function(
           type = "person",
           path = paste0(.wsave, "/", type),
           rewrite_meta = T
           ){
    all_file = list.files(path, full.names = T)
    tags <- lapply(all_file, read_tsv)
    names(tags) = all_file
    tags_df <- data.table::rbindlist(tags, fill = T)
    ## slice .wmeta
    meta <- dplyr::filter(.wmeta, parent_id == type)
    other_meta <- dplyr::filter(.wmeta, parent_id != type)
    ## all feature
    feature_set <- unique(tags_df[["feature"]], meta[["feature"]])
    ## -------------------------------------- 
    ## re load tags
    assign("envir_meta", environment(), envir = parent.env(environment()))
    cat("loading...\n")
    tags <- pbapply::pblapply(tags, add_more_feature)
    ## re write tags
    cat("re-write...\n")
    pbapply::pbmapply(write_tsv,
                      tags,
                      names(tags))
    ## -------------------------------------- 
    ## re load .wmeta
    df <- meta
    set <- feature_set[which(feature_set %in% df$feature == F)]
    lapply(set, meta_add_feature)
    ## redefine
    .wmeta <<- dplyr::bind_rows(df, other_meta)
    if(rewrite_meta == T){
      write_tsv(.wmeta, file = paste0(.wtool, "/", "all_features.tsv"))
    }
    cat("Done\n")
  }
add_more_feature <-
  function(
           df,
           feature_set = get("feature_set", envir = get("envir_meta"))
           ){
    set <- feature_set[which(feature_set %in% df$feature == F)]
    assign("envir_sub", environment(), envir = parent.env(environment()))
    lapply(set, add_feature)
    return(df)
  }
add_feature <-
  function(
           feature,
           envir = get("envir_sub"),
           df = get("df", envir = envir)
           ){
    if(feature %in% df$feature == F){
      df <- dplyr::bind_rows(df, c(feature = feature, depict = ""))
      assign("df", df, envir = envir)
    }
  }
meta_add_feature <-
  function(
           feature,
           df = get("df", envir = get("envir_meta")),
           type = get("type", envir = get("envir_meta"))
           ){
    df <- dplyr::bind_rows(df, c(feature = feature, parent_id = type))
    assign("df", df, envir = get("envir_meta"))
  }

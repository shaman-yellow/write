rm_feature <-
  function(
           name,
           type = "person",
           path = paste0(.wsave, "/", type)
           ){
    all_file = list.files(path, full.names = T)
    tags <- lapply(all_file, read_tsv)
    names(tags) = all_file
    ## --------------------------------------
    ## rm
    tags <- lapply(tags, rm_line,
                   name = name)
    ## --------------------------------------
    ## re write tags
    cat("re-write...\n")
    pbapply::pbmapply(write_tsv,
                      tags,
                      names(tags))
    ## --------------------------------------
    .wmeta <<- dplyr::filter(.wmeta, feature != name & parent_id != type)
    ## re load .wmeta
    cat("Done\n")
  }
rm_line <-
  function(
           df,
           name,
           class = "feature"
           ){
    df <- dplyr::filter_at(df, class, any_vars(. != name))
    return(df)
  }

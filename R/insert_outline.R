insert_outline <-
  function(
           filepath,
           feature_anno = NULL,
           type = "outline",
           tag = "main"
           ){
    meta <- dplyr::filter(.wmeta, parent_id == type)
    lapply(meta$feature, catapp_anno,
           feature_anno = feature_anno,
           after_line = "\n",
           type = paste0("[", type, "]"),
           tag = tag,
           file = filepath)
    return()
  }


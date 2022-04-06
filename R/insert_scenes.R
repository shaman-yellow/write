insert_scenes <-
  function(
           filepath,
           type = "scenes",
           tag = "backgroud"
           ){
    meta <- dplyr::filter(.wmeta, parent_id == type)
    lapply(meta$feature, catapp_anno,
           type = paste0("[", type, "]"),
           tag = tag,
           file = filepath)
    return()
  }
catapp_anno <-
  function(
           feature,
           feature_anno = NULL,
           after_line = NULL,
           type,
           tag,
           file,
           sep = ": "
           ){
    feature = paste0("(", feature, ":", feature_anno, ")")
    cat(paste(type, tag, feature, sep = sep), "\n",
        file = file, append = T, sep = "")
    if(is.null(after_line) == F){
      cat(after_line, file = file, append = T, sep = "")
    }
  }

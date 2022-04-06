read_dic <-
  function(
           filepath
           ){
    features <- data.table::fread(filepath, sep = " ", header = F)
    features <- dplyr::select(features, 2)
    return(features)
  }

read_tsv <-
  function(
           filepath,
           ...
           ){
    file <- data.table::fread(filepath, sep = "\t", header = T, ...)
    file <- dplyr::summarise_all(file, na_as_blank)
    return(file)
  }
na_as_blank <-
  function(
           x
           ){
    ifelse(is.na(x)==T, "", x)
  }
write_tsv <-
  function(
           df,
           file
           ){
    write.table(df, file = file, sep = "\t", quote = F,
                col.names = T, row.names = F)
  }

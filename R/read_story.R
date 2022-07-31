read_story <-
  function(
           filepath = .wsave,
           ...
           ){
    path <- list.files(filepath, pattern = "ch(.*).md$", full.names = T)
    system(paste0("cp ", paste(path, collapse = " "), " -t ", filepath, "/backup"))
    list <- pbapply::pblapply(path, base_read_ch,
                              pretty = T)
    names(list) <- path
    list <- lapply(list, dplyr::as_tibble)
    return(list)
  }
base_read_ch <-
  function(
           path,
           pretty = F
           ){
    ch <- data.table::fread(path, sep = NULL, header = F, fill = T)
    if(pretty == T){
      ch <- dplyr::mutate(ch, V1 = unlist(lapply(V1, pretty_format)))
      n <- nrow(ch)
      all <- ch$V1
      ch$V1 <- mapply(mutate_pretty_format,
                      all,
                      c(all[2:n], ""),
                      SIMPLIFY = F) %>% unlist()
      write.table(ch, file = path, sep = "",
                  quote = F, col.names = F, row.names = F)
    }
    ch <- dplyr::slice(ch, which(grepl("^\\[.{1,30}\\]", ch[,1])))
    options(warn = -1)
    ch <- tidyr::separate(ch, col = "V1", sep = ": ",
                          into = c("type", "tag", "feature"), remove = T)
    ch <- tidyr::separate(ch, col = "feature", sep = ":",
                          into = c("feature", "depict"), remove = T)
    options(warn = 0)
    ch <- dplyr::summarise_all(ch, na_as_blank)
    ch <- dplyr::summarise_all(ch, quote_remove)
    return(ch)
  }
quote_remove <-
  function(
           x
           ){
    gsub("^\\[|\\]$|^\\{|\\}$|^\\(|\\)$", "", x)
  }
pretty_format <- 
  function(
           line
           ){
    ## -------------------------------------- 
    ## -------------------------------------- 
    ## -------------------------------------- 
    pattern = "^[\u4e00-\u9fa5]|^\u3002|^\uff1f|^\uff01|^\uff0c|^\u3001|^\uff1b|^\uff1a|^\u201c|^\u201d|^\u2018|^\u2019|^\uff08|^\uff09|^\u300a|^\u300b|^\u3008|^\u3009|^\u3010|^\u3011|^\u300e|^\u300f|^\u300c|^\u300d|^\ufe43|^\ufe44|^\u3014|^\u3015|^\u2026|^\u2014|^\uff5e|^\ufe4f|^\uffe5"
    ## -------------------------------------- 
    ## -------------------------------------- 
    ## -------------------------------------- 
    ## -------------------------------------- 
    if(grepl(pattern, line)){
      ## -------------------------------------- 
      ## -------------------------------------- 
      ## -------------------------------------- 
      pattern = "\u3002$|\uff1f$|\uff01$|\uff0c$|\u3001$|\uff1b$|\uff1a$|\u201c$|\u201d$|\u2018$|\u2019$|\uff08$|\uff09$|\u300a$|\u300b$|\u3008$|\u3009$|\u3010$|\u3011$|\u300e$|\u300f$|\u300c$|\u300d$|\ufe43$|\ufe44$|\u3014$|\u3015$|\u2026$|\u2014$|\uff5e$|\ufe4f$|\uffe5$|[\u4e00-\u9fa5]$"
      ## -------------------------------------- 
      ## -------------------------------------- 
      ## -------------------------------------- 
      if(grepl(pattern, line)){
        line <- gsub("$", "  ", line)
      }
    }
    return(line)
  }
mutate_pretty_format <- 
  function(
           row,
           next_row
           ){
    if(row == ""){
      if(grepl("^\\[|^&|^$", next_row) == F)
        row <- "&emsp;&emsp;"
    }
    return(row)
  }

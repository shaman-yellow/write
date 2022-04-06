new_ch <-
  function(
           name = "auto",
           path = .wsave
           ){
    if(name == "auto"){
      seq <- get_latest_ch()
      if(is.infinite(seq) == T){
        seq = 0
      }
      name <- paste0("ch", seq + 1, ".md")
    }
    file = paste0(.wsave, "/", name)
    cat("", file = file)
    ## -------------------------------------- 
    ## insert outline
    dir_line <- paste0(.wsave, "/", "outline")
    ## get number of outline file
    seq <- get_latest_ch(dir_line)
    ## taget file
    cpf_line <- paste0(dir_line, "/", "line", seq, ".md")
    ## read file
    cpf_line <- base_read_ch(cpf_line)
    ## inaweer
    insert_outline(file, cpf_line$depict)
    ## -------------------------------------- 
    ## insert scences describe
    insert_scenes(file)
    cat("Done\n")
  }
get_latest_ch <-
  function(
           path = .wsave
           ){
    files <- list.files(path,
                        pattern="^[^E].*\\..{1,5}$",
                        full.names = F)
    seqs <- sapply(files, seq_match)
    if(length(seqs) == 0)
      return(Inf)
    seqs <- max(sort(seqs))
    return(seqs)
  }
seq_match <-
  function(
           string
           ){
    seq <- str_extract_all(string, "[0-9]", simplify = T)
    seq <- paste(seq, collapse = "")
    seq <- as.numeric(seq)
    return(seq)
  }

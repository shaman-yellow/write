get_line <- 
  function(
           path = paste0(.wsave, "/", "outline")
           ){
    seq <- get_latest_ch(path = path)
    name <- paste0("line", seq, ".md")
    file = paste0(path, "/", name)
    system(paste0("vim ", file))
  }

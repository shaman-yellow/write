new_line <-
  function(
           name = "auto",
           path = paste0(.wsave, "/", "outline")
           ){
    if(name == "auto"){
      seq <- get_latest_ch(path = path)
      if(is.infinite(seq) == T){
        seq = 0
      }
      name <- paste0("line", seq + 1, ".md")
    }
    file = paste0(path, "/", name)
    cat("", file = file)
    insert_outline(file)
    cat("Done\n")
  }

lmain <- 
  function(
           path = .wmain_line
           ){
    base_read_ch(path) %>%
      list %>%
      load_into
  }

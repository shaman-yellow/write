get_status <- 
  function(
           path = .wsave
           ){
    list <- read_story(path) %>%
      lapply(., dplyr::filter,
             type == "annotation",
             tag == "status")
    return(list)
  }

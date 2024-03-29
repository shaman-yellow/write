by_group_as_list <-
  function(
           df,
           colnames
           ){
    assign("envir_meta", environment(), envir = parent.env(environment()))
    vector <- unique(df[[colnames]])
    list <- lapply(vector, by_group_as_list_select,
                   colNames = colnames)
    names(list) <- vector
    return(list)
  }
by_group_as_list_select <- 
  function(
           KEY,
           df = get("df", envir = get("envir_meta")),
           colNames
           ){
    df <- df[which(df[[colNames]] == KEY), ]
    return(df)
  }

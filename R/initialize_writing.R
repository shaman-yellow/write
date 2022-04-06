initialize_writing <-
  function(
           story = "rise", 
           path = paste0(.wstory, "/", story)
           ){
    cat("load story", story, "\n")
    if(file.exists(path) == F){
      dir.create(path)
      dir.create(paste0(path, "/backup"))
    }
    .wsave <<- path
    .wmeta <<- read_tsv(paste0(.wtool, "/", "all_features.tsv"))
    .wmain_line <<- paste0(path, "/", "outline", "/", "main_line.md")
    tag <- unique(.wmeta$parent_id)
    cat("create tag dir\n")
    sapply(tag, create_tag_dir)
  }
create_tag_dir <-
  function(tag){
    dir = paste0(.wsave, "/", tag)
    if(file.exists(dir) == F){
      dir.create(dir)
    }
  }

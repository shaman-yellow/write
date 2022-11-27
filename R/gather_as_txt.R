gather_as_txt <- 
  function(
           path = paste0(.wsave, "/", "chapter_previous")
           ){
    file <- list.files(path, pattern = "*.md$")
    df <- data.frame(file = file)
    df <- dplyr::mutate(df, seq = stringr::str_extract(file, "[0-9]{1,}"),
                        seq = as.numeric(seq),
                        new_file = gsub("\\.md$", ".txt", file))
    df <- dplyr::arrange(df, seq)
    tmp <- paste0(path, "/tmp")
    dir.create(tmp)
    content <-
      apply(df, 1,
            function(vec){
              target <- paste0(tmp, "/", vec[["new_file"]])
              system(paste0("pandoc ",
                            path, "/", vec[["file"]],
                            " -o ", target))
              sig <- paste0("\n\n--- chapter ", vec[["seq"]], " ---\n\n")
              c(sig, readLines(target))
            })
    system(paste0("rm -r ", tmp))
    content <- gsub("\\\\$", "\n", unlist(content))
    content <- paste0(gsub("^  $", "   ", content), collapse = "")
    content <- gsub("------", "——", content)
    content <- gsub("\\.\\.\\.\\.\\.\\.", "……", content)
    content <- gsub("……\n\\s*……", "…………", content)
    content <- gsub("…………\\s", "…………\n", content)
    writeLines(content, paste0(path, "/gather.txt"))
    message()
  }


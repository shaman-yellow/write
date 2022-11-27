library(ggsci)
library(grid)

pdf("Cover page.pdf", width = 5, height = 7)
grid.text(label = paste0("Palette of ggsci"),
          y = 0.6,
          gp = gpar(fontsize = 50, fontfamily = "Times"))
grid.text(label = "Org. by Yellow",
          y = 0.485,
          gp = gpar(fontsize = 15, fontfamily = "Times"))
grid.text(label = date(),
          y = 0.4,
          gp = gpar(fontsize = 20, fontfamily = "Times"))
dev.off()

pal.col <- ls("package:ggsci", pattern = "^pal_")
  mapply(pal.col, 1:length(pal.col),
         FUN = function(fun.name, n){
           fun <- match.fun(fun.name)
           col <- fun()(100) %>%
              .[!is.na(.)]
            ## -------------------------------------- 
            pdf(paste0(n, "_", fun.name, ".pdf"), width = 5, height = 7)
            scales::show_col(col)
            grid.text(label = fun.name, x = 0.5, y = 0.9,
                      gp = gpar(fontsize = 40, fontfamily = "Times"))
            dev.off()
})
pdf.con <- list.files(pattern = "^[0-9]")
pdf.con <- c("Cover page.pdf", pdf.con)



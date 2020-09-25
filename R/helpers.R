#' Collects csv from different realizations and collects them in csv files compatible with mrremind structure.
#'
#' @param output_folder directory where the output files are saved
#' @return saves csv files with all EDGE-transport scenarios collected
#' @author Marianna Rottoli
#' @import data.table


collectScens <- function(output_folder){
  directories <- list.dirs(path = output_folder, full.names = TRUE, recursive = TRUE)[grepl("level_2", list.dirs(path = output_folder, full.names = TRUE, recursive = TRUE))]
  filetypes <- c(".csv", ".cs4r")

  for (filetype in filetypes) {
    files <- lapply(directories, list.files, filetype, full.names = TRUE)
    patterns=gsub(paste0(".*level_2/(.+)",filetype,".*"), "\\1", unlist(files))
    
    for (pattern in patterns) {
      out=NULL
      for (i in seq(1:length(files))) {
        dat = fread(files[[i]][grepl(pattern,files[[i]])])
        out = rbind (out, dat)
      }
      fwrite(out,file =paste0(output_folder, "/mrremindData/", pattern, filetype), row.names=FALSE, quote= FALSE)
    }
  }

  }

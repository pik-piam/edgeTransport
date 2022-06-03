#' Collects csv from different realizations and collects them in csv files compatible with mrremind structure.
#'
#' @param scen_folder directory where all scenario-specific results are already saved
#' @param output_folder directory where the output files are to be saved, if set to NULL the same as scen_folder is selected
#' @return saves csv files with all EDGE-transport scenarios collected
#' @author Marianna Rottoli
#' @import data.table
#' @export


collectScens <- function(scen_folder, output_folder = NULL){
  directories <- list.dirs(path = scen_folder, full.names = TRUE, recursive = TRUE)[grepl("level_2", list.dirs(path = scen_folder, full.names = TRUE, recursive = TRUE))]
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

      if(is.null(output_folder)) output_folder = paste0(scen_folder, "/mrremindData")

      outpath <- function(fname){
        path <- file.path(output_folder)
        if(!dir.exists(path)){
          dir.create(path, recursive = T)
        }
        return(file.path(path, fname))
      }

      fwrite(out, file = outpath(paste0(pattern, filetype)), row.names = FALSE, quote= FALSE)
    }
  }

}

#' Compare REMIND outputs for different runs
#'
#' @param runs list of the form `scen`=<folder>, where `scen` is used to identify the run
#'   and folder is the output folder of the run.
#' @param pdf_path path to the PDF file to be produced.
#' @author Alois Dirnaichner
#' @import data.table
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot.new text
#' @export


compareOutputs <- function(runs, pdf_path){
  scen <- all_regi <- tall <- value <- i.value <- V1 <- NULL
  fe2es <- rbindlist(lapply(names(runs), function(scen){
    fread(file.path(runs[[scen]], "level_2/fe2es.cs4r"))[, scen := scen]
  }))

  fetech <- rbindlist(lapply(names(runs), function(scen){
    fread(file.path(runs[[scen]], "level_2/fe_demand_tech.cs4r"))[, scen := scen]
  }))

  capcost <- rbindlist(lapply(names(runs), function(scen){
    fread(file.path(runs[[scen]], "level_2/esCapCost.cs4r"))[, scen := scen]
  }))


  pdf(file=pdf_path, onefile = T)
  for(regi in unique(fe2es$all_regi)){
    plot.new()
    text(x=0, y=1, labels=regi, font=4)
    fe2es_plot <- fe2es[all_regi == regi & tall >= 2010 & tall <= 2100]
    pt <- ggplot(fe2es_plot) +
      geom_line(aes(x=tall, y=value, linetype=scen)) +
      facet_wrap(~all_teEs) +
      labs(x="year", y="trillion pkm/TWa or tkm/TWa")
    print(pt)

    fetech_plot <- fetech[all_regi == regi & tall >= 2010 & tall <= 2100]
    fetech_plot <- fetech_plot[fe2es_plot, on=c("tall", "all_regi", "GDP_scenario", "EDGE_scenario", "all_teEs", "scen")][
    , sum(value * i.value), by=c("tall", "all_regi", "GDP_scenario", "EDGE_scenario", "all_in", "scen")]

    pt <- ggplot(fetech_plot) +
      geom_line(aes(x=tall, y=V1, linetype=scen)) +
      facet_wrap(~all_in) +
      labs(x="year", y="trillion pkm/tkm")
    print(pt)

    capcost_plot <- capcost[all_regi == regi & tall >= 2010 & tall <= 2100]
    pt <- ggplot(capcost_plot) +
      geom_line(aes(x=tall, y=value, linetype=scen)) +
      facet_wrap(~all_teEs) +
      labs(x="year", y="US$2005/pkm or US$2005/tkm")
    print(pt)
  }
  dev.off()

}

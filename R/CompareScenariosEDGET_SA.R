CompareScenariosEDGET_SA <- function(outputfolder,hist,listofruns,y_bar,regionchoice){
  md_template = paste0(outputfolder,"/","CompScen_SA.Rmd")
  #ship and run the file in the output folder
  file.copy(system.file("Rmd", "CompScen_SA.Rmd", package = "edgeTransport"),
  outputfolder, overwrite = T)
  params <- list(hist = hist, listofruns = listofruns, y_bar = y_bar, regionchoice=regionchoice)
  render(md_template, output_format="html_document",params = params)
}



#' Copy and render the compareScenario markdown file.
#'
#' @param target path to markdown file to-be-created. The HTML version will reside just next to the file.
#' @param hist the historical MIF file
#' @param listofruns a list of EDGE-T output folders
#' @param y_bar a series of year to-be-highlighted in the plots
#' @param regionchoice a region to be highlighted in the plots
#' @author Johanna Hoppe, Alois Dirnaichner
#' @importFrom rmarkdown render
#' @export

CompareScenariosEDGET_SA <- function(outputFormat, target, hist, listofruns, y_bar, yrs, regionchoice){
  #ship and run the file in the output folder
  file.copy(
    system.file("Rmd", "CompScen_SA.Rmd", package = "edgeTransport"),
    target, overwrite = T)
  YAMLparams <- list(hist = hist, mif = mif, y_bar = y_bar, yrs=yrs, regionchoice=regionchoice)
  
  render(file.path(target,"CompScen_SA.Rmd"), output_format=outputFormat, params = YAMLparams)
}



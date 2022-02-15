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

CompareScenariosEDGET_SA <- function(target, hist, listofruns, y_bar, regionchoice){
  #ship and run the file in the output folder
  file.copy(
    system.file("Rmd", "CompScen_SA.Rmd", package = "edgeTransport"),
    target, overwrite = T)
  params <- list(hist = hist, listofruns = listofruns, y_bar = y_bar, regionchoice=regionchoice)
  render(target, output_format="html_document", params = params)
}



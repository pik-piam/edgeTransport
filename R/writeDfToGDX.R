#' Export Data Frame / Data Table to GDX Parameter and save to file
#' based on gamstransfer package
#'
#' @param ddata data.frame or data.table
#' @param gdxPath Output file path (e.g., "output.gdx")
#' @param paramName Name of the GAMS parameter in GDX (default: "p35_fe2es")
#' @param domainCols Optional vector of column names for domains.
#'                    If NULL, defaults to all columns except `valueCol`.
#' @param valueCol Name of the numerical value column (default: "value")
#' @param description Optional description for the parameter (default: "")
#' @author Alex K. Hagen
#'
writeDfToGDX <- function(ddata,
                         gdxPath,
                         paramName,
                         domainCols = NULL,
                         valueCol = "value",
                         description = "") {


  # Automatically infer domainCols if not specified
  if (is.null(domainCols)) {
    domainCols <- setdiff(names(ddata), valueCol)
  }

  # Sanity check: Ensure required columns exist
  missingCols <- setdiff(c(domainCols, valueCol), names(ddata))
  if (length(missingCols) > 0) {
    stop(sprintf("Column(s) not found in data frame: %s", paste(missingCols, collapse = ", ")))
  }

  # Keep only required columns & convert safely to base data.frame
  colsToKeep <- c(domainCols, valueCol)
  dfdata <- as.data.frame(ddata)[colsToKeep]

  # Ensure non-character domain columns are converted to character
  for (col in domainCols) {
    if (!is.character(dfdata[[col]]) && !is.factor(dfdata[[col]])) {
      dfdata[[col]] <- as.character(dfdata[[col]])
    }
  }

  # Initialize GAMS Container
  m <- gamstransfer::Container$new()

  # Add each domain column as a GAMS Set symbol holding all unique elements
  for (col in domainCols) {
    set_elements <- unique(na.omit(dfdata[[col]]))
    m$addSet(
      name = col,
      records = set_elements
    )
  }

  # Add parameter records
  m$addParameter(
    name = paramName,
    domain = domainCols,
    records = dfdata,
    description = description
  )

  m$write(gdxPath)
}

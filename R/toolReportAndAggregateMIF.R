
toolReportAndAggregateMIF <- function(vars, helpers) {
  
  rename <- function(var, mapNames) {
    browser()
    cols <- names(var)
    cols <- cols[!cols %in% c("region", "period", "variable", "value", "unit")]
    for (colName in cols) {
      map <- copy(mapNames)
      setnames(map, "name", colName)
      var[grepl(".*tmp", eval(colName)), eval(colName) := NA]
      var <- merge(var, map, by = colName, all.x = TRUE)
      var[!is.na(reportName), colName := reportName][, reportName := NULL]
    } 
    
  return(var)  
  }
  
  aggregate <- function(var, mapAggregation) {
    var <- merge(var, mapAggregation, by = "univocalName", allow.cartesian = TRUE)
    
  }
  
  vars <- lapply(vars, rename, helpers$reportingNames)
  
}
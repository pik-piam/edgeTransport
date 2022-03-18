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


#' Extract preference trends and price trends for processing in the EXCEL table.
#'
#' @param output_folder output folder to extract the preferences and prices from
#' @param logit_data logit input data
#' @param prefs share weight trends
#' @author Johanna Hoppe


Calc_pref_and_prices <- function(output_folder, logit_data, prefs){
  subsector_L2 <- subsector_L3 <- NULL

  EDGET_time <- c(seq(2010,2060,5),seq(2060,2100,10))
  Prices_S2S3 <- logit_data$share_list$S2S3_shares[subsector_L2 %in% c("Bus","trn_pass_road_LDV")]
  Prices_S2S3 <- Prices_S2S3[,c("subsector_L2","year","region","tot_price")]
  setnames(Prices_S2S3,"subsector_L2","mode")
  Prices_S3S <- logit_data$share_list$S3S_shares[subsector_L3 %in% c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  Prices_S3S <- Prices_S3S[,c("subsector_L3","year","region","tot_price")]
  setnames(Prices_S3S,"subsector_L3","mode")


  Mode_Prices <- rbind(Prices_S2S3,Prices_S3S)
  Mode_Prices <- Mode_Prices[year %in% EDGET_time]
  Mode_Prices <- dcast(Mode_Prices,mode + region ~ year)


  Mode_Prices <- as.data.table(Mode_Prices)
  write.csv(Mode_Prices,file.path(output_folder,"/Mode_Prices_NEW_VOT.csv"),row.names=FALSE)

  Pref_S2S3 <- prefs$S2S3_final_pref[subsector_L2 %in% c("Bus","trn_pass_road_LDV")]
  Pref_S2S3 <- Pref_S2S3[,c("subsector_L2","year","region","sw")]
  setnames(Pref_S2S3,"subsector_L2","mode")
  Pref_S3S <- prefs$S3S_final_pref[subsector_L3 %in% c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  Pref_S3S <- Pref_S3S[,c("subsector_L3","year","region","sw")]
  setnames(Pref_S3S,"subsector_L3","mode")

  Mode_Prefs <- rbind(Pref_S2S3,Pref_S3S)
  Mode_Prefs <- Mode_Prefs[year %in% EDGET_time]
  Mode_Prefs <- dcast(Mode_Prefs,mode + region ~ year)

  Mode_Prefs <- as.data.table(Mode_Prefs)
  write.csv(Mode_Prefs,file.path(output_folder,"/Mode_Prefs.csv"),row.names=FALSE)

}


#' Update Validation Excel Tool
#'
#' @param Excel_path path to Excel Sheet
#' @param hist path to historical.mif
#' @param EDGE_T_run path to output folder of EDGE-T run
#' @author Johanna Hoppe
#'
#' @importFrom xlsx getSheets loadWorkbook CellStyle createSheet createCell createRow setCellValue setCellStyle addDataFrame saveWorkbook
#' @import data.table
#' @importFrom ggplot2 ggplot
#' @export


Update_Validation_Excel_tool <- function(Excel_path, hist, EDGE_T_run){
  CountryCode <- RegionCode <- Year <- variable <- scenario <- value <- weight <- Period <- NULL
  model <- unit <- tot <- period <- Font <- Alignment <- Border <- region <- `.` <- NULL
  ## load mappings
  REMIND2ISO_MAPPING <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))[, .(ISO = CountryCode, region = RegionCode)]
  #Choose regions to be considered from MIF file
  regions <- unique(REMIND2ISO_MAPPING$region)


  #Data for GDP per capita plot
  GDP_country = {
    x <- calcOutput("GDP", aggregate = F)
    getSets(x)[1] <- "ISO3"
    getSets(x)[2] <- "Year"
    x
  }
  POP_country = {
    x <- calcOutput("Population", aggregate = F)
    getSets(x)[1] <- "iso2c"
    x
  }

  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  GDP_country[, variable := paste0(sub("gdp_", "", variable))]
  setnames(GDP_country, c("ISO3", "variable", "value", "year"), c("CountryCode", "scenario", "weight", "period"))
  POP <- as.data.table(POP_country)
  POP[, year := as.numeric(gsub("y", "", year))]
  POP[, variable := paste0(sub("pop_", "", variable))]
  setnames(POP, c("iso2c", "variable", "year"), c("CountryCode", "scenario", "period"))

  Regionmapping_21_EU11 <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
  setnames(Regionmapping_21_EU11, "RegionCode", "region")

  GDP_21 <- aggregate_dt(GDP_country, Regionmapping_21_EU11[, -c("X", "missingH12")], yearcol = "period", fewcol =   "region", manycol = "CountryCode", datacols = "scenario", valuecol = "weight")
  POP_21 <- aggregate_dt(POP, Regionmapping_21_EU11[, -c("X", "missingH12")], yearcol = "period", fewcol = "region", manycol = "CountryCode", datacols = "scenario", valuecol = "value")
  setnames(POP_21,"value","POP")

  GDP_POP_21 <- merge(GDP_21[scenario == "SSP2"],POP_21[scenario == "SSP2"])
  GDP_POP_21[, value := weight/POP][, weight := NULL][, POP := NULL]

  mif <- list.files(EDGE_T_run, "*.mif", full.names=T)
  stopifnot(length(mif) == 1)
  #Read in EDGE-T data
  data <- readMIF(mif)
  data <- melt(
      data,
      id.vars=c("Variable","Unit","Region","Scenario","Model"),
      variable.name = "Period",
      value.name = "Value")
  data <- as.data.table(data)

  setnames(data,c("Variable","Unit","Region","Scenario","Model","Period","Value"),c("variable","unit","region","scenario","model","period","value"))

  # #Weighted Population denisty for region clusters
  # Weighted_POP <-  fread(WD_POP,header=TRUE ,sep=",")[,.(ISO,value = PWD_G,year,Area)]
  # Weighted_POP <- merge(Weighted_POP,REMIND2ISO_MAPPING,all.x = TRUE)
  # Weighted_POP <- Weighted_POP[,value:=value*(Area/sum(Area)),by=c("region")]
  # Weighted_POP <- Weighted_POP[,.(value=sum(value)),by=.(region)]
  # Weighted_POP[,range:=(max(value)-min(value))/4]
  # Weighted_POP[value<=range,cluster:=1][value>range & value<=2*range,cluster:=2][value>2*range & value<=3*range,cluster:=3][value>3*range,cluster:=4]
  # cluster <-Weighted_POP[,.(region,cluster)]
  #
  # #Expectation: Convergence of Passenger mode shares in 2150 according to country with highest GDP/cap projection in 2150
  # vars <- c(
  #   "ES|Transport|Pass|Aviation|International|Share",
  #   "ES|Transport|Pass|Road|LDV|Share",
  #   "ES|Transport|Pass|Road|Bus|Share",
  #   "ES|Transport|Pass|Rail|non-HSR|Share",
  #   "ES|Transport|Pass|Aviation|Domestic|Share",
  #   "ES|Transport|Pass|Rail|HSR|Share",
  #   "ES|Transport|Pass|Road|Non-Motorized|Walking|Share",
  #   "ES|Transport|Pass|Road|Non-Motorized|Cycling|Share"
  # )
  #
  #
  # Conv_year <- 2150
  # Exp <- data[variable %in% vars & region %in%regions]
  # Exp <- merge(Exp,cluster,all.x=TRUE)
  # Exp <- merge(Exp,GDP_POP_21[period==Conv_year,.(region,GDP_POP=value)],all.x=TRUE)
  # Exp[,lead_reg:= max(GDP_POP),by="cluster"]
  # Exp[,lead_reg:= ifelse(GDP_POP==lead_reg, 1,0)]
  #
  #
  # Exp_conv <- Exp[variable %in% vars]
  # Exp_conv[,period:=as.numeric(as.character(period))]
  # Exp_conv[,lead_share:=value[lead_reg==1 & period==Conv_year],by=c("cluster","variable")]
  # Exp_conv[period >= 2020, value := value[period == 2020]*(Conv_year-period)/(Conv_year-2020) + lead_share*(period-2020)/(Conv_year-2020), by =c("variable", "region")]
  # Exp_conv[,sum:=sum(value),by=c("region","period")]
  # #Print lead regions
  # print(paste0("Lead regions:",unique(Exp_conv[lead_reg==1,.(cluster,region)])))
  # Exp_conv <- Exp_conv[, c("model","scenario","region","variable","unit","period","value")]



  #Read ETP data
  historical <- fread(hist, header=TRUE)
  historical <- as.data.table(historical)
  historical <- melt(
    historical,
    id.vars=c("Variable","Unit","Region","Scenario","Model"),
    variable.name = "Period",
    value.name = "Value")
  historical <- as.data.table(historical)
  historical[, Period := as.numeric(gsub("X", "", Period))]
  setnames(historical, c("Variable", "Unit", "Region", "Scenario", "Model", "Period", "Value"),c("variable", "unit", "region", "scenario", "model", "period", "value"))

  vars <- c(
    "ES|Transport|Pass|Aviation",
    "ES|Transport|Pass|Rail",
    "ES|Transport|Pass|Road|Bus",
    "ES|Transport|Pass|Road|LDV"
  )

  hist_ES_Pass_Shares <- historical[variable %in% vars & grepl("IEA ETP", model)][, unit := NULL]
  hist_ES_Pass_Shares <- hist_ES_Pass_Shares[!value == "N/A"]
  hist_ES_Pass_Shares <- hist_ES_Pass_Shares[, value := as.double(value)]
  hist_ES_Pass_Shares[, tot:= sum(value), by= c("period","region","scenario","model")]
  hist_ES_Pass_Shares[,value:=value/tot*100][, unit := "%"][, tot := NULL]
  hist_ES_Pass_Shares <- hist_ES_Pass_Shares[, variable := paste0(variable, "|Share")]


  hist_ES_Pass_Shares_RTS <- hist_ES_Pass_Shares[model == "IEA ETP RTS"]
  hist_ES_Pass_Shares_RTS <- hist_ES_Pass_Shares_RTS[period %in% c(2014,2025,2030,2050,2060)]
  hist_ES_Pass_Shares_RTS[, value := round(value, 1)]
  hist_ES_Pass_Shares_RTS <- dcast(hist_ES_Pass_Shares_RTS, variable + unit + region + scenario + model ~ period)
  hist_ES_Pass_Shares_RTS <- as.data.table(hist_ES_Pass_Shares_RTS)

  #Read prices and prefs
  Prices <- fread(file.path(EDGE_T_run, "Mode_Prices_NEW_VOT.csv"), header = TRUE)
  Prefs <- fread(file.path(EDGE_T_run, "Mode_Prefs.csv"), header = TRUE)

  wb <- xlsx::loadWorkbook(Excel_path)
  sheets <- getSheets(wb)
  # define style for title
  title_style <- xlsx::CellStyle(wb) +
    Font(wb, heightInPoints = 16,
         isBold = TRUE)

  # define style for row and column names
  rowname_style <- xlsx::CellStyle(wb) +
    Font(wb, isBold = TRUE)
  colname_style <- xlsx::CellStyle(wb) +
    Font(wb, isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
    Border(color = "black",
           position = c("TOP", "BOTTOM"),
           pen = c("BORDER_THIN", "BORDER_THIN"))


  vars <- c(
    "ES|Transport|Pass|Aviation|International|Share",
    "ES|Transport|Pass|Road|LDV|Share",
    "ES|Transport|Pass|Road|Bus|Share",
    "ES|Transport|Pass|Rail|non-HSR|Share",
    "ES|Transport|Pass|Aviation|Domestic|Share",
    "ES|Transport|Pass|Rail|HSR|Share",
    "ES|Transport|Pass|Road|Non-Motorized|Walking|Share",
    "ES|Transport|Pass|Road|Non-Motorized|Cycling|Share"
  )



  for (region0 in regions){

    if (is.null(sheets[[region0]])){
      #Create new worksheet if necessary
      ws <- xlsx::createSheet(wb, sheetName = region0)
      #create a new row
      rows <- xlsx::createRow(ws, rowIndex = 1)
      #create a cell in the row to contain the title.
      sheetTitle <- xlsx::createCell(rows, colIndex = 1)
      # set the cell value
      xlsx::setCellValue(sheetTitle[[1,1]], paste0("Validation ", region0))
      # set the cell style
      xlsx::setCellStyle(sheetTitle[[1,1]], title_style)
      sheets <- getSheets(wb)}

    xlxs_data <- data[period %in% c(2010,2020,2030,2050,2100,2150) & region == region0 & variable %in% vars]
    xlxs_data[,value:=round(as.numeric(value),1)]
    xlxs_data <- dcast(xlxs_data, variable + unit + region + scenario + model ~ period)
    xlxs_data <- xlxs_data[match(vars,variable)]

    # Exp_conv0 <- Exp_conv[period %in% c(2010,2020,2030,2050,2100,2150) & region == region0 & variable %in% vars]
    # Exp_conv0[,value:=round(value,1)]
    # Exp_conv0 <- dcast(Exp_conv0, variable + unit + region + scenario + model ~ period)
    # Exp_conv0 <-Exp_conv0[match(vars,variable)]

    Prices0 <- Prices[region==region0]
    Prefs0 <- Prefs[region==region0]
    target <- c("trn_pass_road_LDV","Bus","Passenger Rail","Domestic Aviation","HSR","Walk","Cycle","trn_pass_road")
    Prices0 <- Prices0[match(target,mode)][,region:=NULL]
    Prefs0 <- Prefs0[match(target,mode)][,region:=NULL]
    Prefs0 <- Prefs0[,c("mode","2010")]

    xlsx::addDataFrame(xlxs_data, sheet = sheets[[region0]], startRow = 3, startColumn = 1,
                       colnamesStyle = colname_style,
                       rownamesStyle = rowname_style,
                       row.names = FALSE)

    # xlsx::addDataFrame(Exp_conv0, sheet = sheets[[region0]], startRow = 16, startColumn = 1,
    #                    colnamesStyle = colname_style,
    #                    rownamesStyle = rowname_style,
    #                    row.names = FALSE)

    xlsx::addDataFrame(Prices0, sheet = sheets[[region0]], startRow = 56, startColumn = 3,
                       colnamesStyle = colname_style,
                       rownamesStyle = rowname_style,
                       row.names = FALSE)

    xlsx::addDataFrame(Prefs0, sheet = sheets[[region0]], startRow = 68, startColumn = 3,
                       colnamesStyle = colname_style,
                       rownamesStyle = rowname_style,
                       row.names = FALSE)
    # #Add ETP Shares
    # rows <- xlsx::createRow(sheets[[region0]], rowIndex = 37)
    # sheetTitle <- xlsx::createCell(rows, colIndex = 1)
    # xlsx::setCellValue(sheetTitle[[1,1]], paste0("ETP RTS data ", region0))
    # xlsx::setCellStyle(sheetTitle[[1,1]], title_style)
    #
    # xlsx::addDataFrame(hist_ES_Pass_Shares_RTS[region == region0], sheet = sheets[[region0]], startRow = 39, startColumn = 1,
    #                    colnamesStyle = colname_style,
    #                    rownamesStyle = rowname_style,
    #                    row.names = FALSE)
    #
    #
    # vars2 <- c(
    #   "ES|Transport|Pass|Aviation|Domestic",
    #   "ES|Transport|Pass|Rail|HSR",
    #   "ES|Transport|Pass|Rail|non-HSR",
    #   "ES|Transport|Pass|Road|Bus",
    #   "ES|Transport|Pass|Road|LDV",
    #   "ES|Transport|Pass|Road|Non-Motorized|Cycling",
    #   "ES|Transport|Pass|Road|Non-Motorized|Walking"
    # )
    #


    # #Add energy services plot
    # png("plot1.png", height=900, width=1600, res=250, pointsize=8)
    # p1 <- mipArea(data[variable %in% vars2 & region == region0], scales="free_y")
    # plot(p1)
    # dev.off()
    # addPicture("plot1.png", sheet = sheets[[region0]], scale = 1, startRow = 13,
    #            startColumn = 12)


    #ADD GDP_POP plot
    # png("plot.png", height=900, width=1600, res=250, pointsize=8)
    # p1 <- ggplot(GDP_POP_21[(region %in% c("DEU","IND","CHA","USA") | region == region0) & period <= 2150], aes(x = as.character(period), y = value,group = region, color = region))+
    #   geom_line(size = 1)+
    #   scale_x_discrete(breaks = c(2010,2020,2030,2050,2100))+
    #   labs(x = "", y = paste0("GDP per Capita"), title = paste0("GDP per Capita for all regions"))
    # plot(p1)
    # dev.off()
    # addPicture("plot.png", sheet = sheets[[region0]], scale = 1, startRow = 2,
    #            startColumn = 24)




  }


  xlsx::saveWorkbook(wb, Excel_path)
}


#' Change sw trends
#'
#' @param Excel_path path to Validation Excel Sheet
#' @param sw_trends path to sw trend table
#' @author Johanna Hoppe
#' @param SSP_scen SSP or SDP scenario
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom utils write.csv
#' @export


Update_sw_trend <- function(Excel_path, SSP_scen, sw_trends){
  subsector_L2 <- subsector_L3 <- SSP_scenario <- techscen <- subsector_L1 <- value <- i.value <- NULL
  sw_table <- fread(sw_trends, header=TRUE)

  sw_targets <- list()
  for (region0 in unique(sw_table$region)){
    sw_targets0 <- read_excel(Excel_path,sheet=region0,"X68:AD76")
    sw_targets <- rbind(sw_targets, sw_targets0)}
  sw_targets <- as.data.table(sw_targets)

  sw_targets[,subsector_L2:=ifelse(mode %in% c("Bus","trn_pass_road_LDV"), mode, "")]
  sw_targets[,subsector_L3:=ifelse(mode %in% c("Bus","trn_pass_road_LDV"), "trn_pass_road", mode)][, mode:=NULL]

  sw_table <- data.table::melt(
    sw_table,
    id.vars=c(
      "SSP_scenario", "region", "technology", "vehicle_type",
      "subsector_L1", "subsector_L2", "subsector_L3",
      "sector", "level", "techscen", "approx"),
    variable.name = "period",
    value.name = "value")

  sw_targets <- data.table::melt(
    sw_targets,
    id.vars=c("subsector_L2","subsector_L3","region"),
    variable.name = "period",
    value.name = "value")
  sw_targets[, SSP_scenario := SSP_scen]

  sw_targets[, techscen := unique(sw_table$techscen)][, subsector_L1 := ""]
  sw_table[sw_targets, value := i.value,
           on=c("SSP_scenario", "region", "period",
                "subsector_L2", "subsector_L3", "subsector_L1", "techscen")]
  sw_table <- dcast(sw_table, ...~ period, value.var = "value")
  write.csv(sw_table,sw_trends,row.names=FALSE)
}

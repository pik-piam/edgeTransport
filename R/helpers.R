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

  # EDGET_time <- c(seq(2010,2060,5),seq(2060,2100,10))
  # Prices_S2S3 <- logit_data$prices_list$S1S2[subsector_L2 %in% c("Bus","trn_pass_road_LDV")]
  # Prices_S2S3 <- Prices_S2S3[,c("subsector_L2","year","region","tot_price")]
  # setnames(Prices_S2S3,"subsector_L2","mode")
  # Prices_S3S <- logit_data$prices_list$S2S3[subsector_L3 %in% c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  # Prices_S3S <- Prices_S3S[,c("subsector_L3","year","region","tot_price")]
  # setnames(Prices_S3S,"subsector_L3","mode")
  #
  #
  # Mode_Prices <- rbind(Prices_S2S3,Prices_S3S)
  # Mode_Prices <- Mode_Prices[region=="CHA" & year %in% EDGET_time][,region:=NULL]
  # Mode_Prices <- dcast(Mode_Prices,mode ~ year)
  # target <- c("trn_pass_road_LDV","Bus","Passenger Rail","Domestic Aviation","HSR","Walk","Cycle","trn_pass_road")
  #
  # Mode_Prices <- as.data.table(Mode_Prices)
  # Mode_Prices <- Mode_Prices[match(target,mode)]
  # write.csv(Mode_Prices,paste0(output_folder,"/Mode_Prices_oldVOT.csv"))

  EDGET_time <- c(seq(2010,2060,5),seq(2060,2100,10))
  Prices_S2S3 <- logit_data$share_list$S2S3_shares[subsector_L2 %in% c("Bus","trn_pass_road_LDV")]
  Prices_S2S3 <- Prices_S2S3[,c("subsector_L2","year","region","tot_price")]
  setnames(Prices_S2S3,"subsector_L2","mode")
  Prices_S3S <- logit_data$share_list$S3S_shares[subsector_L3 %in% c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  Prices_S3S <- Prices_S3S[,c("subsector_L3","year","region","tot_price")]
  setnames(Prices_S3S,"subsector_L3","mode")


  Mode_Prices <- rbind(Prices_S2S3,Prices_S3S)
  Mode_Prices <- Mode_Prices[region=="CHA" & year %in% EDGET_time][,region:=NULL]
  Mode_Prices <- dcast(Mode_Prices,mode ~ year)
  target <- c("trn_pass_road_LDV","Bus","Passenger Rail","Domestic Aviation","HSR","Walk","Cycle","trn_pass_road")

  Mode_Prices <- as.data.table(Mode_Prices)
  Mode_Prices <- Mode_Prices[match(target,mode)]
  write.csv(Mode_Prices,paste0(output_folder,"/Mode_Prices_NEW_VOT.csv"))

  Pref_S2S3 <- prefs$S2S3_final_pref[subsector_L2 %in% c("Bus","trn_pass_road_LDV")]
  Pref_S2S3 <- Pref_S2S3[,c("subsector_L2","year","region","sw")]
  setnames(Pref_S2S3,"subsector_L2","mode")
  Pref_S3S <- prefs$S3S_final_pref[subsector_L3 %in% c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  Pref_S3S <- Pref_S3S[,c("subsector_L3","year","region","sw")]
  setnames(Pref_S3S,"subsector_L3","mode")

  Mode_Prefs <- rbind(Pref_S2S3,Pref_S3S)
  Mode_Prefs <- Mode_Prefs[region=="CHA" & year %in% EDGET_time][,region:=NULL]
  Mode_Prefs <- dcast(Mode_Prefs,mode ~ year)


  target <- c("trn_pass_road_LDV","Bus","Passenger Rail","Domestic Aviation","HSR","Walk","Cycle","trn_pass_road")
  Mode_Prefs <- as.data.table(Mode_Prefs)
  Mode_Prefs <- Mode_Prefs[match(target,mode)]
  write.csv(Mode_Prefs,paste0(output_folder,"/Mode_Prefs.csv"))

}

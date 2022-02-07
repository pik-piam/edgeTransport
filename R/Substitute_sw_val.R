Substitute_sw_val <- function(pref){
  
  path <- "C:/Users/johannah/Documents/EDGE-Transport/Validation/SW_package.csv"
  sw_pack <- fread(path,header = TRUE)

  
  sw_pack <- melt(
    sw_pack,
    id.vars= c("mode","region"),
    variable.name = "year",
    value.name = "sw_new")
  
  sw_pack <- as.data.table(sw_pack)
  sw_pack$year <- as.numeric(as.character(sw_pack$year))
  sw_pack_S2S3 <- sw_pack[mode %in% c("Bus","trn_pass_road_LDV")]
  setnames(sw_pack_S2S3,"mode","subsector_L2")
  sw_pack_S3S <- sw_pack[mode %in%  c("HSR","Passenger Rail","Domestic Aviation","Walk","Cycle","trn_pass_road")]
  setnames(sw_pack_S3S,"mode","subsector_L3")
  
  pref$S2S3_final_pref <- merge(pref$S2S3_final_pref,sw_pack_S2S3, all = TRUE)
  pref$S2S3_final_pref[!is.na(sw_new),sw:=sw_new][,sw_new:=NULL]  
  pref$S3S_final_pref <- merge(pref$S3S_final_pref,sw_pack_S3S, all = TRUE) 
  pref$S3S_final_pref[!is.na(sw_new),sw:=sw_new][,sw_new:=NULL]
  return(pref)
}
#' Harmonize the energy intensities to match the IEA energy balances.
#'
#' We provide energy service trajectories. IEA energy balances have to be met and are *not*
#' consistent with GCAM intensities and ES trajectories.
#' Therefore we have to change energy intensities for historical timesteps and smoothly
#' change them to the GCAM/PSI values.
#'
#' @param int intensity
#' @param demKm demand in million km
#' @param IEA IEA balances
#' @importFrom rmndt magpie2dt
#'


toolIEAharmonization <- function(int, demKm, IEA) {
    te <- isbunk <- flow <- value <- `.` <- region <- EJ_Mpkm <- conv_pkm_MJ <- subsector_L1 <- NULL
    subsector_L3 <- technology <- EJ_Mpkm.mean <- vehicle_type <- sector <- EJ_Mpkm_ave_adjusted <- NULL
    Mpkm_tot <- factor_intensity <- EJ_Mpkm_ave <- EJ_Mpkm_adjusted <- lambda <- EJ_Mpkm_final <- NULL
    EJ_tot_adjusted <- NULL

    IEA <- IEA[, , c("fedie", "fepet", "fegat", "feelt"), pmatch = TRUE]

    IEA_dt <- magpie2dt(IEA, datacols=c("se", "fe", "te", "mod", "flow"), regioncol="region", yearcol="year")
    IEA_dt <- IEA_dt[te != "dot"]  #delete fedie.dot
    IEA_dt2plot = copy(IEA_dt)
    IEA_dt2plot = IEA_dt2plot[, .(value = sum(value)), by=.(region, year, flow)]
    IEA_dt2plot = IEA_dt2plot[!flow %in% c("PIPELINE", "TRNONSPE", "NETRANS")]

    IEA_dt = IEA_dt[year==2005]
    IEA_dt[, isbunk := ifelse(grepl("BUNK", flow), flow, "short-medium")]

    IEA_dt[, c("se", "fe", "mod", "flow") := NULL]
    ## sum fossil liquids and biofuel to tdlit, and biogas and natural gas to tdgat
    IEA_dt[te %in% c("tdfospet", "tdfosdie", "tdbiopet", "tdbiodie"), te := "tdlit"]
    IEA_dt[te %in% c("tdfosgat", "tdbiogat"), te := "tdgat"]
    IEA_dt[, value := sum(value), by=.(region, year, te, isbunk)]

    IEA_dt <- unique(IEA_dt)

    ## load pkm and intensity from GCAM database conversion rate MJ->EJ
    CONV_MJ_EJ <- 1e-12
    CONV_millionkm_km <- 1e6
    vehicle_intensity <- copy(int)
    vehicle_intensity[, EJ_Mpkm := conv_pkm_MJ * CONV_millionkm_km * CONV_MJ_EJ][,conv_pkm_MJ:=NULL]

    ## output is given in million pkm
    tech_output <- copy(demKm)
    tech_output <- merge(tech_output, vehicle_intensity, all.x = TRUE,
                         by = intersect(colnames(tech_output), colnames(vehicle_intensity)))
    tech_output <- tech_output[!subsector_L3 %in% c("Cycle", "Walk"), ]  #no non-motorized

    ## use only 2005
    tech_output <- tech_output[year == 2005]

    setkey(tech_output, "technology")

    ## apply the IEA-friendly categories to tech_output
    elts <- c("Electric", "BEV")
    tech_output[technology %in% elts, te := "tdelt"]
    gats <- "NG"
    tech_output[technology %in% gats, te := "tdgat"]

    ## all others are liquids (this includes some coal), as well as Hybrid Electric
    tech_output[is.na(te), te := "tdlit"]

    tech_output <- tech_output[tech_output > 0]

    dups <- duplicated(tech_output, by=c("region", "technology", "vehicle_type"))
    if(any(dups)){
        warning("Duplicated techs found in supplied demand.")
        print(tech_output[dups])
        tech_output <- unique(tech_output, by=c("region", "technology", "vehicle_type"))
    }


    ## if there is output but no intensity, we have to apply avg. intensity
    tech_output[, EJ_Mpkm.mean := mean(EJ_Mpkm, na.rm = T), by=.(year, technology, vehicle_type)
                ][is.na(EJ_Mpkm), EJ_Mpkm := EJ_Mpkm.mean
                  ][,EJ_Mpkm.mean := NULL]

    tech_output[, isbunk := ifelse(sector == "trn_aviation_intl", "AVBUNK", NA)]
    tech_output[, isbunk := ifelse(sector == "trn_shipping_intl", "MARBUNK", isbunk)]
    tech_output[, isbunk := ifelse(is.na(isbunk), "short-medium", isbunk)]


    tech_output_aggr <- tech_output[, .(Mpkm_tot = sum(tech_output),
                                        EJ_Mpkm_ave = sum(tech_output/sum(tech_output) * EJ_Mpkm)),
                                        by = c("region", "year", "te", "isbunk")]

    ## merge with IEA
    tech_output_iea <- merge(IEA_dt, tech_output_aggr, all.y = TRUE, by = c("year","region", "te", "isbunk"))

    ## inconsistencies such as IEA stating 0 feelt in AFG and GCAM saying that the
    ## total pkm there are1.791546e+07 are solved in the hard way, deleting the demand
    ## for the country that in theory is there according to GCAM
    tech_output_iea <- tech_output_iea[value > 0]

    ## calculate intensity factor
    tech_output_iea[, EJ_Mpkm_ave_adjusted := value/Mpkm_tot]
    ## calculate the ratio between the new and the old energy intensity
    tech_output_iea[, factor_intensity := EJ_Mpkm_ave_adjusted/EJ_Mpkm_ave][, year := NULL]

    ## redistribute the energy intensity to the broader category they belong to, in
    ## the energy intensity dt
    tech_output <- merge(tech_output, tech_output_iea, all = FALSE,
                             by = c("region", "te", "isbunk"))

    ## remove all the columns that I don't need, including years (so to have it for all years)
    tech_output=tech_output[, c("region","technology","vehicle_type", "factor_intensity")]

    ## harmonize data
    # leave out DEU as it was carefully adjusted by Alois to meet Ariadne data
    merged_intensity <- tech_output[vehicle_intensity, on=c("region", "technology", "vehicle_type")]
    merged_intensity[!region == "DEU", EJ_Mpkm_final := EJ_Mpkm * factor_intensity]
    ## if there is no harmonization data, lets use the existing one
    merged_intensity[is.na(EJ_Mpkm_final), EJ_Mpkm_final := EJ_Mpkm]


    ## delete columns not useful anymore
    merged_intensity[, c("EJ_Mpkm", "factor_intensity")] = NULL
    ## cut energy intensity to 3 for NG LDVs
    merged_intensity[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "NG", EJ_Mpkm_final := min(EJ_Mpkm_final, 3.2e-06), by = c("region", "year", "vehicle_type")]

    return(list(merged_intensity = merged_intensity,
           IEA_dt2plot = IEA_dt2plot))

}

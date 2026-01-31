#' Update inconvenience costs for LDV 4Ws in India
#'
#' Calibrates the cost markup for 4W inconvenience costs to reproduce historical target shares.
#'
#' @param S3S2sharesOld data.table containing S3S2shares before calibration
#' @param package name of the package for internal targetShares CSV (default: "edgeTransport")
#' @returns updated S3S2shares with calibrated 4W inconvenience costs
#' @import data.table
#' @export
updateIncoCosts <- function(S3S2sharesOld) {
  library(data.table)
  reg <- c("IND")

  S3S2shares <- copy(S3S2sharesOld)
   #add row for inconvenience costs 
  newRow <- S3S2shares[subsectorL3 == "trn_pass_road_LDV_4W" & variable == "Fuel costs" & region %in% reg,]
  newRow[, variable := "Inconvenience costs"][, value := 0.0]
  S3S2shares <- rbind(S3S2shares, newRow)

  # --- STEP 1: target shares ---
  targetShares <- fread(system.file("extdata", "targetShares.csv",package = "edgeTransport"), header = TRUE)
  targetShares[, share := as.numeric(share)]
  periods <- targetShares$period

  # --- STEP 2: Subset all road modes for India ---
  logit <- copy(S3S2shares[
    region == reg &
      period %in% periods &
      subsectorL1 == "trn_pass_road"
  ])
 
  
  markup <- 0
  logit_dt <- copy(logit)
  target_share_period <- targetShares[1]
  # --- STEP 3: Function to evaluate markup for a single period ---
  evaluate_markup <- function(markup, logit_dt, target_share_period) {
    test_dt <- copy(logit_dt[period == target_share_period$period])

    # Apply markup only to 4W inconvenience costs
    test_dt[variable == "Inconvenience costs", value := value + markup]
    # Recalculate total price per alternative
    test_dt <- test_dt[, .(totPrice = sum(value)),
                       by = setdiff(names(test_dt), c("variable", "type", "value"))]

    # Compute logit shares
    test_dt[, share := calculateShares(totPrice, lambda, pref),
            by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]

    # Extract simulated 4W share
    sim_share <- test_dt[subsectorL3 == "trn_pass_road_LDV_4W", sum(share, na.rm = TRUE)]

    # Return absolute error vs target
    return(as.numeric(abs(sim_share - target_share_period$share)))
  }
i <- 1
  # --- STEP 4: Calibrate per period ---
  
  markups <- sapply(1:nrow(targetShares), function(i) {
    period_i <- targetShares[i]
    optimize(
      evaluate_markup,
      interval = c(-2, 10),  
      logit_dt = logit,
      target_share_period = period_i
    )$minimum
  })

  # --- STEP 5: Apply calibrated markups for India, 4W only ---
  for (i in seq_along(periods)) {
    period_i <- periods[i]
    S3S2shares[
      region == reg &
        period == period_i &
        subsectorL3 == "trn_pass_road_LDV_4W" &
        variable == "Inconvenience costs",
      value := value + markups[i]
    ]
  }

  # --- STEP 6: Recalculate final shares for all road modes ---
  #S3S2shares <- S3S2shares[, .(totPrice = sum(value)),
  #                        by = setdiff(names(S3S2shares), c("variable", "type", "value"))]
  #S3S2shares[, share := calculateShares(totPrice, lambda, pref),
  #           by = c("region", "period", "sector", "subsectorL1", "subsectorL2")]

  # Optionally, print calibrated markups
  print(data.table(period = periods, markup = markups))

  return(S3S2shares)
}


# 1) hop degredation over time
# 2) hop ibu calculations

#' @title Hop utilization calculations
#' @description Calculate hop requirements for brewing beer
#' @aliases hop_calcs hop_weight
#' @param hw Weight of hops in g
#' @param wv Wort volume in L
#' @param ibu International Bittering Units in mg/L
#' @param aa Percentage Alpha Acids
#' @param utilization Percentage hop utilization
#' @return the missing variable in the equation hw = (wv*ibu)/(aa*utilization*1000)
#' @examples
#' # calculate the weight of hops to achieve 25 IBU using Cascade at 10.3% Alpha Acids and 30 % utilization
#' hop_calcs(hw = NA, wv = 21, ibu = 25, aa = 0.103, utilization = 0.30)
#' # calculate the weight of hops to achieve 25 IBU using Cascade at 10.3% Alpha Acids and 30 % utilization
#' hop_weight(21, 25, 0.103, 0.30)
#' @references
#' Lewis MJ, Young, TW. (2001). Brewing, 2nd Edition. Klewer Academic / Plenum Publishers
#' @export
hop_calcs <- function(hw, wv, ibu, aa,  utilization) {
  # weight of hops = (wort volume * desired IBU)/(% Alpha Acids * % utilization)
  # hw = (wv*ibu)/(aa*utilization)
  # wv=(aa*hw*utilization)/ibu
  # ibu=(aa*hw*utilization)/wv
  # aa=(ibu*wv)/(hw*utilization)
  # utilization=(ibu*wv)/(aa*hw)

  if (sum(is.na(c(wv,ibu,aa,utilization))) > 1) {
    ## put error message here: too many unassigned variables
  } else if (is.na(hw) == T) {
    return((wv*ibu)/(aa*utilization*1000))
  } else if (is.na(wv) == T) {
    return((aa*hw*utilization*1000)/ibu)
  } else if (is.na(ibu) == T) {
    return((aa*hw*utilization*1000)/wv)
  } else if (is.na(aa) == T) {
    return((ibu*wv)/(hw*utilization*1000))
  } else if (is.na(utilization) == T) {
    return((ibu*wv)/(aa*hw*1000))
  } else {
    return(NA)
  }
}

#' @rdname hop_calcs
#' @export
hop_weight <- function(wv, ibu, aa,  utilization) {
  return(hop_calcs(wv, ibu, aa, utilization, hw = NA))
}

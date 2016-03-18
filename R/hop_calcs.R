
# 1) hop degredation over time
# 2) hop ibu calculations

#' @title Hop utilization calculations
#' @description Calculate hop requirements for brewing beer
#' @aliases hop_calcs hop_weight
#' @param mh mass of hops in g
#' @param vw Wort volume in L
#' @param ibu International Bittering Units in mg/L
#' @param aa Percentage Alpha Acids
#' @param utilization Percentage hop utilization
#' @return the missing variable in the equation mh = (vw*ibu)/(aa*utilization*1000)
#' @examples
#' # calculate the weight of hops to achieve 25 IBU using Cascade at 10.3% Alpha Acids and 30 % utilization
#' hop_calcs(mh = NA, vw = 21, ibu = 25, aa = 0.103, utilization = 0.30)
#' # calculate the weight of hops to achieve 25 IBU using Cascade at 10.3% Alpha Acids and 30 % utilization
#' hop_weight(21, 25, 0.103, 0.30)
#' @references
#' Lewis MJ, Young, TW. (2001). Brewing, 2nd Edition. Klewer Academic / Plenum Publishers
#' @export
hop_calcs <- function(mh = NA, vw = NA, ibu = NA, aa = NA, utilization = NA) {
  # weight of hops = (wort volume * desired IBU)/(% Alpha Acids * % utilization)
  # mh = (vw*ibu)/(aa*utilization)
  # vw=(aa*mh*utilization)/ibu
  # ibu=(aa*mh*utilization)/vw
  # aa=(ibu*vw)/(mh*utilization)
  # utilization=(ibu*vw)/(aa*mh)

  if (sum(is.na(c(vw,ibu,aa,utilization))) > 1) {
    ## put error message here: too many unassigned variables
  } else if (is.na(mh) == T) {
    return((vw*ibu)/(aa*utilization*1000))
  } else if (is.na(vw) == T) {
    return((aa*mh*utilization*1000)/ibu)
  } else if (is.na(ibu) == T) {
    return((aa*mh*utilization*1000)/vw)
  } else if (is.na(aa) == T) {
    return((ibu*vw)/(mh*utilization*1000))
  } else if (is.na(utilization) == T) {
    return((ibu*vw)/(aa*mh*1000))
  } else {
    return(NA)
  }
}

#' @rdname hop_calcs
#' @export
hop_weight <- function(vw, ibu, aa,  utilization) {
  return(hop_calcs(vw, ibu, aa, utilization, mh = NA))
}

#' @rdname hop_calcs
#' @export
ibu <- function(mh, vw, aa, utilization) {
  return(hop_calcs(mh, vw, ibu = NA, aa,  utilization))
}

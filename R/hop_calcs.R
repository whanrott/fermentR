
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
#' # calculate the ibu of using 17g Cascade hops at 10.3% Alpha Acids and 30 % utilization
#' ibu(mh = 17, vw = 21, aa = 0.103, utilization = 0.30)
#' # calculate the hop utilization
#' hop_utilization(mh = 17, vw = 21, ibu = 25, aa = 0.103)
#' @references
#' Lewis MJ, Young, TW. (2001). Brewing, 2nd Edition. Klewer Academic / Plenum Publishers
#' @export
hop_calcs <- function(mh = NA, vw = NA, ibu = NA, aa = NA, utilization = NA) {
  # mass of hops = (wort volume * desired IBU)/(% Alpha Acids * % utilization)
  tmp <- is.na(list(mh, vw, ibu, aa, utilization))
  if (sum(tmp) == 0) {
    stop("You can't specify all three values")
  }
  if (sum(tmp) > 1) {
    stop("Too many values specified")
  }
  switch( which(is.na(list(mh, vw, ibu, aa, utilization))),
          # mh = (vw*ibu)/(aa*utilization)
          return((vw*ibu)/(aa*utilization*1000)),
          # vw=(aa*mh*utilization)/ibu
          return((aa*mh*utilization*1000)/ibu),
          # ibu=(aa*mh*utilization)/vw
          return((aa*mh*utilization*1000)/vw),
          # aa=(ibu*vw)/(mh*utilization)
          return((ibu*vw)/(mh*utilization*1000)),
          # utilization=(ibu*vw)/(aa*mh)
          return((ibu*vw)/(aa*mh*1000))
          )
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

#' @rdname hop_calcs
#' @export
hop_utilization <- function(mh, vw, ibu, aa) {
  return(hop_calcs(mh, vw, ibu, aa, utilization = NA))
}

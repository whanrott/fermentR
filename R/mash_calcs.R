
# 1) required grain for a given OG
# 2) water volume & temp required for target strike temperature
# 3) extract by colour?
# 4) extract efficiency

#' @title Mash temperature calculator
#' @description Calculate mash composition when brewing beer
#' @param ww weight of water in Kg
#' @param wg weight of grain in Kg
#' @param tm mash temperature in degrees C
#' @param tw temperature of water in C before mixing
#' @param tg temperature of grain in C before mixing
#' @param shw specific heat of water in kJ/KgC. Defaults to 1.0kJ/KgC
#' @param shg specific heat of grain in kJ/KgC. Defaults to 0.4kJ/KgC
#' @return the missing variable in the equation tm=(shw*tw*ww+shg*tg*wg)/(shw*ww+shg*wg)
#' @examples
#' # calculate temperature of water when mashing in
#' mash_calcs(ww = 5, wg = 5, tm = NA, tw = 80, tg = 20)
#' # calculate strike temperature
#' mash_calcs(5,5,63,NA,20)
#' @export
mash_calcs <- function(ww, wg, tm, tw, tg, shw = 1.0, shg = 0.4) {
    if (sum(is.na(c(ww, wg, tm, tw, tg, shw, shg))) > 1) {
        ## put error message here: too many unassigned variables
    } else if (is.na(tm) == T) {
        ## calculate mash temperature
        return(((shw * ww * tw) + (shg * wg * tg))/((shw * ww) + (shg * wg)))
    } else if (is.na(tw) == T) {
        ## calculate strike temperature (ie water temp)
        return((tm * shw * ww + (tm * shg - shg * tg) * wg)/(shw * ww))
    } else if (is.na(ww)) {
        ## calculate mash liquor
        return(abs(((shg * tg - tm * shg) * wg)/(shw * tw - tm * shw)))
    }
}

#' @title Strike Temperature calculator
#' @description Calculate the strike temperature for hot liquor to achieve a given mash temperature
#' @param ww weight of water in Kg
#' @param wg weight of grain in Kg
#' @param tm mash temperature in degrees C
#' @param tg temperature of grain in C before mixing
#' @return the strike temperature in degreed C
#' @examples
#' # calculate strike temperature
#' strike(5,5,63,20)
#' @export
strike <- function(ww,wg,tm,tg) {
  mash_calcs(ww, wg, tm, tw = NA, tg)
}

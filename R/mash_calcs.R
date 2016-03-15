
# 1) required grain for a given OG
# 2) water volume & temp required for target strike temperature
# 3) extract by colour?
# 4) extract efficiency

#' Mash temperature calculator
#'
#' Use mash_calc() to calculate mash composition when brewing beer. Use strike()
#' to calculate the strike temperature for hot liquor to achieve a given mash
#' temperature. The word liquor is used here in the brewing sense to mean water
#' which is mixed into the mash.
#'
#' @aliases mash_calcs strike step_mash
#' @param wl weight of liquor in Kg (or volume in L)
#' @param wg weight of grist in Kg
#' @param tl temperature of liquor in degrees C before mixing
#' @param tg temperature of grist in degrees C before mixing
#' @param tm temperature of mash in degrees C after mixing
#' @param shw specific heat of pure water. This is 4.181kJ/Kg/K
#' @param shl specific heat of liquor relative to pure water. Defaults to 1.0
#' @param shg specific heat of grist relative to the liquor. Defaults to 0.4
#' @return the missing variable in the equation tm=(shl*tl*wl+shg*tg*wg)/(shl*wl+shg*wg)
#' @examples
#' # calculate temperature of liquor when mashing in
#' mash_calcs(wl = 5, wg = 5, tm = NA, tl = 80, tg = 20)
#' # calculate strike temperature
#' mash_calcs(5,5,63,NA,20)
#' # calculate strike temperature
#' strike(5,5,63,20)
#' @references
#' Lewis MJ, Young, TW. (2001). Brewing, 2nd Edition. Klewer Academic / Plenum Publishers
#'
#' B.K Bala & J.L Woods (1991) "Physical and Thermal Properties of Malt", Drying
#'  Technology, 9:4, 1091-1104, DOI: 10.1080/07373939108916735
#'
#' "Feel the Mash Heat", BYO Magazine, Sept 1997. http://byo.com/stories/item/627-feel-the-mash-heat
#'
#' @export
mash_calcs <- function(wl = NA, wg = NA, tm = NA, tl = NA, tg = NA, shl = 1.0, shg = 0.4) {
  tmp <- is.na(list(wl, wg, tm, tl, tg, shl, shg))
  if (sum(tmp) == 0) {
    stop("You can't specify all three values")
    }
  if (sum(tmp) > 1) {
    stop("Too many values specified")
    }
  switch( which(is.na(list(wl, wg, tm, tl, tg, shl, shg))),
          ## calculate weight of mash liquor
          return(-(((shg * tg - tm * shg) * wg)/(shl * tl - tm * shl))),
          ## calculate weight of grain
          return(-((shl*tm-shl*tl)*wl)/(shg*tm-shg*tg)),
          ## calculate tm mash temperature
          return(((shl * wl * tl) + (shg * wg * tg))/((shl * wl) + (shg * wg))),
          ## calculate strike temperature (ie liquor temp)
          return((tm * shl * wl + (tm * shg - shg * tg) * wg)/(shl * wl)),
          ## calculate temperature of grain
          return(((shl*tm-shl*tl)*wl+shg*tm*wg)/(shg*wg)),
          ## calculate shl
          return(-((shg*tm-shg*tg)*wg)/((tm-tl)*wl)),
          ## calculate shg
          return(-((shl*tm-shl*tl)*wl)/((tm-tg)*wg))
  )
}

#' @rdname mash_calcs
#' @export
strike <- function(wl,wg,tm,tg) {
  mash_calcs(wl, wg, tm, tl = NA, tg)
}

## step mash: either by adding liquor in steps or by removing a portion and boiling it
## adding liquor: input vector of target mash temps; grist temp; grist weight;
##               output must be a vector of liquor volumes
# http://byo.com/stories/item/627-feel-the-mash-heat

#' @rdname mash_calcs
#' @export
step_mash <- function(wl,wg,tm,tg) {
  # function will take vector of mash temperatures and indicate the weight of mash to be boiled to acheieve it
  }

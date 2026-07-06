#' HMRC Duty calculator
#'
#' Use \code{hmrc_duty} to calculate tax on beer in pence.
#'
#' @aliases duty_calc
#' @param volume mass of liquor in Kg (or volume in L)
#' @param mg mass of grist in Kg
#' @param tl temperature of liquor in degrees C before mixing
#' @param tg temperature of grist in degrees C before mixing
#' @param tm temperature of mash in degrees C after mixing
#' @param shw specific heat of pure water. This is 4.181kJ/Kg/K
#' @param shl specific heat of liquor relative to pure water. Defaults to 1.0
#' @param shg specific heat of grist relative to the liquor. Defaults to 0.4
#' @return the missing variable in the equation tm=(shl*tl*ml+shg*tg*mg)/(shl*ml+shg*mg)
#' @examples
#' # calculate temperature of liquor when mashing in
#' mash_calcs(ml = 5, mg = 5, tm = NA, tl = 80, tg = 20)
#' # calculate strike temperature
#' mash_calcs(5,5,63,NA,20)
#' # calculate strike temperature
#' strike(5,5,63,20)
#' @references
#' "Alcohol Duty Rates", Gov.uk, https://www.gov.uk/guidance/alcohol-duty-rates
#'
#' @export
hmrc_duty <- function(volume, strength, type, hmrc_rates_2026) {
  # if (!(as.numeric(volume) > 0)) {
  #   stop("volume must be a positive number")
  # }
  # if (!dplyr::between(strength, 0, 1)) {
  #   stop("percentage alcohol must be expressed as a number between 0 and 1")
  # }
#   hmrc_rates_2026 <- readr::read_tsv(
#     I(
#       "percent_lower	percent_upper	beer	cider_still	cider_sparkling	other	beer_draught	cider_still_draught	cider_sparkling_draught
# 0	1.3	0	0	0	0	0	0	0
# 1.3	3.5	9.96	9.96	9.96	9.96	8.58	8.58	8.58
# 3.5	5.5	22.58	10.39	10.39	26.61	19.45	8.95	8.95
# 5.5	8.5	22.58	10.39	26.61	26.61	19.45	8.95	19.45
# 8.5	22	30.62	30.62	30.62	30.62	30.62	30.62	30.62
# 22		33.99	33.99	33.99	33.99			"
#     )
#   ) |>
#     tidyr::pivot_longer(-c(percent_lower, percent_upper),
#                         names_to = "key",
#                         values_to = "value")
  
  # print(hmrc_rates_2026 |> select(key) |> arrange(key) |> distinct() |> unlist() |> as.character())
  
  rates_beer <- hmrc_rates_2026 |>
    dplyr::filter(strength >= percent_lower / 100 &
                    strength <= percent_upper / 100 &
                    key == type)
  
  if (nrow(rates_beer) == 0) {
    print("Type not recognised. reverting to values for section 'Other fermented products like fruit ciders'")
    print(paste("accepted types are: ", paste(hmrc_rates_2026 |> select(key) |> arrange(key) |> distinct() |> unlist() |> as.character(), collapse = ", ")))
    rates_beer <- hmrc_rates_2026 |>
      dplyr::filter(strength >= percent_lower / 100 &
                      strength <= percent_upper / 100 &
                      key == "other")
  }
  # print(hmrc_rates_2026[hmrc_rates_2026$key == type, ])

  calculate_alcohol_in_l <- function(volume, strength) {
    return(volume * strength)
  }

  # print(rates_beer)
  rate <- calculate_alcohol_in_l(volume, strength) * rates_beer[1, "value"]
  return(rate$value)
}

# abv: a function to calculate Alcohol by Volume

#' @title Alcohol by Volume calculator
#' @description Calculates the alcohol by volume (ABV) given starting and finishing gravities
#' @param og the starting specific gravity of the wort expressed relative to 1 (eg: 1.034)
#' @param fg the finishing specific gravity of the wort expressed relative to 1 (eg:1.012)
#' @return alcohol by volume exressed in percentage volume of liquid
#' @examples
#' # calculate alcohol by volume for a wort which ferments from 1034 SG to 1008 SG
#' abv(1.034,1.012)
#' @export
abv <- function(og, fg) {
  return((76.08 * (og - fg)/(1.775 - og)) * (fg/0.794))
}

# abv_hmrc: a function to calculate Alcohol by Volume using the UK Government's HMRC calculator

#' @title Alcohol by Volume calculator for HMRC returns
#' @description Calculates the alcohol by volume (ABV) given starting and finishing gravities using
#' conversion factors set out in the Beer Duty notices. These were taken from Beer Duty Notice #226 on 22/09/2016.
#' @references https://www.gov.uk/government/publications/excise-notice-226-beer-duty/excise-notice-226-beer-duty--2#calculation-of-alcoholic-strength
#' @references https://www.gov.uk/government/collections/alcohols-notices
#' Beer Duty notices and other relevant documentation can be found on the HMRC website
#' @param og the starting specific gravity of the wort expressed relative to 1 (eg: 1.034)
#' @param pg the present specific gravity of the wort expressed relative to 1 (eg:1.012)
#' @param f is the factor
#' @return alcohol by volume exressed in percentage volume of liquid using calculations specified by HMRC department of the UK Government
#' @examples
#' # calculate alcohol by volume for a wort which ferments from 1034 SG to 1008 SG
#' abv_hmrc(1.034,1.012)
#' @export
abv_hmrc <- function(og, pg, f) {
  # Value of factor ‘f’ for various alcoholic strengths
  # (OG - PG)     % ABV     Factor
  # Up to 6.9     Up to   0.8     0.125
  #  7.0 -  10.4   0.8 -  1.3     0.126
  # 10.5 -  17.2   1.3 -  2.1     0.127
  # 17.3 -  26.1   2.2 -  3.3     0.128
  # 26.2 -  36.0   3.3 -  4.6     0.129
  # 36.1 -  46.5   4.6 -  6.0     0.130
  # 46.6 -  57.1   6.0 -  7.5     0.131
  # 57.2 -  67.9   7.5 -  9.0     0.132
  # 68.0 -  78.8   9.0 - 10.5     0.133
  # 78.9 -  89.7  10.5 - 12.0     0.134
  # 89.8 - 100.7  12.0 - 13.6     0.135
  x <- round((og-pg)*1000,1)
  ## local function to tidy the switch below.
  ## code borrowed from data.table package but I didn't want to
  ## require the data.table package for such a simple function
  between <- function (x, lower, upper, incbounds = TRUE)
  {
    if (incbounds)
      x >= lower & x <= upper
    else x > lower & x < upper
  }
  switch(
    which(
      c(
        between(x,   0, 6.9),
        between(x, 7.0,10.4),
        between(x,10.5,17.2),
        between(x,17.3,26.1),
        between(x,26.2,36.0),
        between(x,36.1,46.5),
        between(x,46.6,57.1),
        # between(x,,),
        !between(x,0,100,7)
        )
      ),
    x * 0.125,
    x * 0.126,
    x * 0.127,
    x * 0.128,
    x * 0.129,
    x * 0.130,
    x * 0.131,
    print("Error: the difference in gravity is either negative or greater than the HMRC calculator provisions for (100.7)")
    )
}

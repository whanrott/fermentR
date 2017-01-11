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
  if(!is.numeric(c(og,fg)))
    stop("Error. The gravity should be a number relative to 1. For example 1.042")
  return((76.08 * (og - fg)/(1.775 - og)) * (fg/0.794))
}

# abv_hmrc: a function to calculate Alcohol by Volume using the UK Government's HMRC calculator

#' @title Alcohol by Volume calculator for HMRC returns
#' @description Calculates the alcohol by volume (ABV) given starting and finishing gravities using
#' conversion factors set out in the Beer Duty notices. These were taken from Beer Duty Notice #226 on 22/09/2016.
#' Reference values are expressed in gravity * 1000, to 1 decimal place. For example 1045.6. This function expects
#' gravity to be expressed as 1.0456 but can take values in either format.
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
  ## input is format 1.xxxx to 4 decimal places. HMRC guidance uses figures 1xxxx.x to 1 dp.
  if(og > 0 & og < 2) {og <- og*1000}
  if(pg > 0 & pg < 2) {pg <- pg*1000}
  x <- (round(og,1)-round(pg,1))
  switch(which(c(
    x >    0 & x <=   6.9,    x >  7.0 & x <=  10.4,    x > 10.5 & x <=  17.2,    x > 17.3 & x <=  26.1,
    x > 26.2 & x <=  36.0,    x > 36.1 & x <=  46.5,    x > 46.6 & x <=  57.1,    x > 57.2 & x <=  67.9,
    x > 68.0 & x <=  78.8,    x > 78.9 & x <=  89.7,    x > 89.8 & x <= 100.7,    x <=   0 | x >  100.7
    )),
    x * 0.125,    x * 0.126,    x * 0.127,    x * 0.128,    x * 0.129,    x * 0.130,    x * 0.131,
    x * 0.132,    x * 0.133,    x * 0.134,    x * 0.135,
    print("Error: the difference in gravity betweem OG and PG is either negative or greater than the HMRC calculator provisions for (100.7)")
    )
}

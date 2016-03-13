# abv: a function to calculate Alcohol by Volume

#' Calculates the alcohol by volume (ABV) given starting and finishing gravities
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

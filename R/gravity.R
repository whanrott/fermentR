# gravity: a function to converting between Brix, Plato and Specific Gravity in unfermened worts
# the function converts back and forth. I will create an alias for converting from Brix,
# from Plato etc

## the calculations to SG twere produced by a computer algebra system working on the sg to brix
## and sg to plato polynomials.  Ideally these will be replaced by something simpler. The
## CAS which calculated this was Maxima.

## Bx = (((182.4601*SG -775.6821)*SG +1262.7794)*SG -669.5622) http://medlibrary.org/medwiki/Brix#Tables

#' @title Brewing gravity converter
#' @description Convert between Brix, Plato and Specific Gravity in unfermented worts
#' @param x a numeric vector giving the measurement value in its original units.
#' @param from the unit in which the measurement was made.
#' @param to	the unit to which the measurement is to be converted.
#' @return numeric vector in the unit specified
#' @export
gravity <- function(x, from = "brix", to = "sg") {
    if (tolower(from) == "brix" & to == "sg") {
        ## using the given polynomial, 10Bx == 1.040027sg. The following 2 solutions to the polynomial show 1.040027sg == 9.999932Bx which seems reasonable accuracy
        return(((1824601 * sqrt(2) * sqrt(1.12359447310534e+21 * x^2 - 1.83050301606428e+23 * x + 1.14483239199408e+25) + 16645844046005000 * 3^(3/2) * x - 451976053349203776 * 3^(5/2))^(2/3) +
            861869 * 3^(3/2) * (1824601 * sqrt(2) * sqrt(1.12359447310534e+21 * x^2 - 1.83050301606428e+23 * x + 1.14483239199408e+25) + 16645844046005000 * 3^(3/2) * x - 451976053349203776 *
                3^(5/2))^(1/3) - 2984594884847)/(1824601 * sqrt(3) * (1824601 * sqrt(2) * sqrt(1.12359447310534e+21 * x^2 - 1.83050301606428e+23 * x + 1.14483239199408e+25) + 16645844046005000 *
            3^(3/2) * x - 451976053349203776 * 3^(5/2))^(1/3)))
    } else if (tolower(from) == "plato" & to == "sg") {
        return(((2 * sqrt(31210623015187500 * x^2 - 6048240462052202496 * x + 3.81373272810243e+20))/(18495184009 * 3^(3/2)) + (249684984121500 * x - 24192961848208808)/67912817571143272)^(1/3) -
            56092325756/(166456656081 * ((2 * sqrt(31210623015187500 * x^2 - 6048240462052202496 * x + 3.81373272810243e+20))/(18495184009 * 3^(3/2)) + (249684984121500 * x - 24192961848208808)/67912817571143272)^(1/3)) +
            630272/407991)


    } else if (from == "sg" & to == "brix") {
        return(x * (x * (182.4601 * x - 775.6821) + 1262.7794) - 669.5622)
    } else if (from == "sg" & to == "plato") {
        return(x * (x * (135.997 * x - 630.272) + 1111.14) - 616.868)
    } else if (from == "balling" | to == "balling") {
        ## This function doesn't cover Balling scale
        return(NA)
    } else {
        return(NA)
    }
}

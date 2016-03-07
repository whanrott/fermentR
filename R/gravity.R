# gravity: a function to converting between Brix, Plato and Specific Gravity in unfermened worts
# the function converts back and forth. I will create an alias for converting from Brix, from Plato etc

gravity <- function(x, fromunit = "brix", tounit="sg") {
    ## the calculations to SG were produced by a computer algebra system working on the sg to brix and sg to plato polynomials.
    ## Ideally these will be replaced by something simpler. The CAS which calculated this was Maxima.
  if (tolower(fromunit) == "brix" & tounit == "sg") {
    ## both the following solutions work, taing 10Bx == 1.040027sg, they give 1.040027sg == 9.999932Bx
    return(
      ((1824601*sqrt(2)*sqrt(1123594473105337500000*x^2-183050301606427528875000*x+11448323919940769914396849)+16645844046005000*3^(3/2)*x-451976053349203775*3^(5/2))^(2/3)+861869*3^(3/2)*
      (1824601*sqrt(2)*sqrt(1123594473105337500000*x^2-183050301606427528875000*x+11448323919940769914396849)+16645844046005000*3^(3/2)*x-451976053349203775*3^(5/2))^(1/3)-2984594884847)/
        (1824601*sqrt(3)*(1824601*sqrt(2)*sqrt(1123594473105337500000*x^2-183050301606427528875000*x+11448323919940769914396849)+16645844046005000*3^(3/2)*x-451976053349203775*3^(5/2))^(1/3))
    );
    # return(
    #   ((16645844046005000*x-1355928160047611325)/6074404738436953801+(sqrt(2)*sqrt(1123594473105337500000*x^2-183050301606427528875000*x+11448323919940769914396849))/(3329168809201*3^(3/2)))
    # ^(1/3)-(2984594884847)/(9987506427603*((16645844046005000*x-1355928160047611325)/6074404738436953801+(sqrt(2)*sqrt(1123594473105337500000*x^2-183050301606427528875000*x+11448323919940769914396849))/(3329168809201*3^(3/2)))^(1/3))+2585607/1824601
    # );
  } else if (tolower(fromunit) == "plato" & tounit == "sg") {
    return(
      ((2*sqrt(31210623015187500*x^2-6048240462052202500*x+381373272810243497203))/(18495184009*3^(3/2))+(249684984121500*x-24192961848208810)/67912817571143271)^(1/3)-
    56092325756/(166456656081*((2*sqrt(31210623015187500*x^2-6048240462052202500*x+381373272810243497203))/(18495184009*3^(3/2))+(249684984121500*x-24192961848208810)/67912817571143271)^(1/3))+630272/407991);
    ## http://medlibrary.org/medwiki/Brix#Tables
    ## °Bx = (((182.4601*SG -775.6821)*SG +1262.7794)*SG -669.5622)

  } else if (fromunit == "sg" & tounit == "brix") {
    return(x * (x * (182.4601 * x - 775.6821) + 1262.7794) - 669.5622);
  } else if (fromunit == "sg" & tounit == "plato") {
    return(x * (x * (135.997  * x - 630.272 ) + 1111.14  ) - 616.868 );
  } else if (fromunit == "balling" | tounit == "balling") {
    ## This function doesn't cover Balling scale
    return(NA)
  } else {
    return(NA)
  }
}
#
# fromsg <- function(x, unit = "brix", temp = 20, tscale = "c") {
#   if (tolower(unit) == "brix") {
#     return(x * (x * (182.4601 * x - 775.6821) + 1262.7794) - 669.5622);
#   } else if (tolower(unit) == "plato") {
#     return(x * (x * (135.997  * x - 630.272 ) + 1111.14  ) - 616.868 );
#   } else if (tolower(unit) == "balling") {
#     return(NA)
#   } else {
#     return(NA)
#   }
# }

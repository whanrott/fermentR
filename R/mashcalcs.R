
# 1) required grain for a given OG
# 2) water volume & temp required for target strike temperature
# 3) extract by colour?
# 4) extract efficiency

# ww: weight of water in Kg
# wg: weight of grain in Kg
# tm: mash temperature
# tw: temperature of water in C before mixing
# tg: temperature of grain in C before mixing
# shw: specific heat of water in kJ/KgC. Defaults to 1.0kJ/KgC
# shg: specific heat of grain in kJ/KgC. Defaults to 0.4kJ/KgC

# > mashtemp(5,5,80,20)
# [1] 62.85714
# > strike(5,5,62,20)
# [1] 78.8


mashtemp <- function(ww, wg, tm, tw, tg, shw = 1.0,shg = 0.4) {
  print(is.na(c(ww,wg,tm, tw,tg,shw,shg)))
  if(sum(is.na(c(ww,wg,tm, tw,tg,shw,shg))) > 1) {
    ## put error message here: too many unassigned variables
    } else if(is.na(tm) == T) {
    ## calculate mash temperature
    return(((shw*ww*tw)+(shg*wg*tg))/((shw*ww)+(shg*wg)))
    } else if (is.na(tm) == T) {
    ## calculate strike temperature (ie water temp)
      return((tm*shw*ww+(tm*shg-shg*tg)*wg)/(shw*ww))
    } else if (is.na(ww)) {
    ## calculate mash liquor
      return(abs(((shg*tg-tm*shg)*wg)/(shw*tw-tm*shw)))
    }
  }

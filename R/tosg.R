# tosg: a function to convert from Balling, Brix or Plato to Specific Gravity
# in unfermened worts

tosg <- function(x, unit = "plato", temperature = 20) {
#  {Plato/(258.6-([Plato/258.2]*227.1)}+1 = Specific gravity
  if (tolower(unit) == "plato") {
    return(x/(258.6-((x/258.2)*227.1)+1))
  } else {
    print("you've chosen to use another unit")
  }
}

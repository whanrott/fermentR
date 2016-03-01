# abv: a function to calculate Alcohol by Volume

abv <- function(og, fg) {
# the less accurate calculation
# ABV = (og – fg) * 131.25
#  return((og - fg) * 131.25);

# the more accurate caculation
# ABV =(76.08 * (og-fg) / (1.775-og)) * (fg / 0.794)
  return((76.08 * (og-fg) / (1.775-og)) * (fg / 0.794));
}

# This file just contains assumptions relevant for the management technology choice
# this is the yield multiplier that goes from the observed yield to the "hi" and "lo" yields
#  (observed plus or minus observed times this number)
MGMT_YIELD_ADJ <- 0.1

# the logit exponent for the hi/lo management technology choice, and the selection of absolute or relative cost logit
mgmt_logit <- 0.5
mgmt_logit_type <- "absolute-cost-logit"




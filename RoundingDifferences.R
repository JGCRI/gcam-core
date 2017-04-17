library(dplyr)
library(tidyr)

# read in old DS output
#A <- read.csv(file="~/GitHub/gcamdata/tests/testthat/comparison_data/aglu/L102.ag_HA_bm2_R_C_GLU.csv", comment.char = "#")
A <- read.csv(file="~/GitHub/gcamdata/tests/testthat/comparison_data/aglu/L102.ag_Prod_Mt_R_C_GLU.csv", comment.char = "#")

#read in new DS output
#B <- read.csv(file="~/GitHub/gcamdata/outputs/L102.ag_HA_bm2_R_C_GLU.csv",comment.char = "#")
B <- read.csv(file="~/GitHub/gcamdata/outputs/L102.ag_Prod_Mt_R_C_GLU.csv",comment.char = "#")

# arrange old DS so everything in same order
A %>% arrange(GCAM_region_ID, GCAM_commodity) -> A1


#define round_df
round_df <- function(x, digits = 3) {
  integer_columns <- sapply(x, class) == "integer"
  x[, integer_columns] <- sapply(x[, integer_columns], as.numeric)

  numeric_columns <- sapply(x, class) == "numeric"
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}


#apply round_df
B2 <- round_df(B,3)
A2 <- round_df(A1, 3)


# get non-zero differences
A2$value-B2$value -> D
which(D != 0) -> Y


# investigate
A2[Y,]
B2[Y,]


# corresponding points in the original files
A[A$GCAM_region_ID==3 &A$GLU=="GLU087" & A$GCAM_commodity=="MiscCrop",]
B[B$GCAM_region_ID==3 & B$GLU=="GLU087" & B$GCAM_commodity=="MiscCrop",]

A[A$GCAM_region_ID==8 & A$GLU=="GLU227" & A$GCAM_commodity=="SugarCrop",]
B[B$GCAM_region_ID==8 & B$GLU=="GLU227" & B$GCAM_commodity=="SugarCrop",]

A[A$GCAM_region_ID==20 & A$GLU=="GLU135" & A$GCAM_commodity=="FodderGrass",]
B[B$GCAM_region_ID==20 & B$GLU=="GLU135" & B$GCAM_commodity=="FodderGrass",]

A[A$GCAM_region_ID==22 & A$GLU=="GLU104" & A$GCAM_commodity=="SugarCrop",]
B[B$GCAM_region_ID==22 & B$GLU=="GLU104" & B$GCAM_commodity=="SugarCrop",]

A[A$GCAM_region_ID==29 & A$GLU=="GLU159" & A$GCAM_commodity=="PalmFruit",]
B[B$GCAM_region_ID==29 & B$GLU=="GLU159" & B$GCAM_commodity=="PalmFruit",]


# max and min difference in Full, unrounded data
A1$value - B$value -> fullDiff
max(fullDiff)
min(fullDiff)


# mimic sum test
sum(A$value) == sum(B$value)

library(testthat)
library(rhud)

# Before test check, make sure to quiet and then unquiet it.
options(rhud_quiet_loads = TRUE)
test_check("rhud")
options(rhud_quiet_loads = FALSE)

# Quiet readr's column specification messages during tests
withr::local_options(
  list(readr.show_col_types = FALSE),
  .local_envir = teardown_env()
)

# Make the magrittr pipe available in tests without library(dplyr)
`%>%` <- magrittr::`%>%`

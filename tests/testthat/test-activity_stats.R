
# rm(list = ls()); library(testthat); library(arcstats)

require(data.table)
require(lubridate)
require(dplyr)


## CONTEXT (without exclude or include) ----------------------------------------

context("Testing activity_stats()")

out_activity_stats <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arcstats")) %>%
    as.data.frame()
  acc     <- dat_i$vectormagnitude
  acc_ts  <- ymd_hms(dat_i$timestamp)
  out <- activity_stats(acc, acc_ts)
  return(out)
})

out_all_steps <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arcstats")) %>%
    as.data.frame()
  acc    <- dat_i$vectormagnitude
  acc_ts <- ymd_hms(dat_i$timestamp)
  ## Get acc data vector in "midnight_to_midnight" format
  acc <- midnight_to_midnight(acc, acc_ts)
  ## Get wear/non-wear flag
  wear_flag <- get_wear_flag(acc)
  ## Get valid/non-valid day flag
  valid_day_flag <- get_valid_day_flag(wear_flag)
  ## Impute missing data in acc data vector
  acc <- impute_missing_data(acc, wear_flag, valid_day_flag)
  ## Summarize PA
  out <- summarize_PA(acc, acc_ts, wear_flag, valid_day_flag)
  return(out)
})


test_that_desc <- paste0(
  "Compare the wrapper out_activity_stats() gives same results as step by step procedure")
test_that(test_that_desc, {
  for (i in 1:length(out_activity_stats)){ # i <- 4
    out1 <- out_activity_stats[[i]]
    out2 <- out_all_steps[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})

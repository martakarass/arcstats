
# rm(list = ls()); library(testthat); library(arcstats)

require(lubridate)
require(data.table)
require(dplyr)

## Alternative implementation --------------------------------------------------

MidnightToMidnight_JU <- function(ac, timestamp){

  # ac - minute-level activity counts
  # timestamp - minute-level timestamp (2016-01-29 12:15:00)

  timestamp = ymd_hms(timestamp)

  date = as.Date(timestamp)
  time = hour(timestamp)/24*1440 + minute(timestamp) +1

  date.ind = julian(date) - min(julian(date))
  out = data.frame(date.ind, time, ac)

  ndays = length(levels(as.factor(date)))

  ac.complete = rep(NA, 1440*ndays)
  ac.complete[out$date.ind*1440 + out$time] = ac

  return(ac.complete)
}



## Define objects common to all test contexts  ---------------------------------

## Read data from files attached to the package to be used in tests
dat_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  fread(system.file("extdata", extdata_fname_i, package = "arcstats")) %>%
    as.data.frame()
})

## Precompute output with the use of package function implementation
MidnightToMidnight_out_list <- lapply(dat_list, function(dat_i){
  midnight_to_midnight(dat_i$vectormagnitude, ymd_hms(dat_i$timestamp))
})

## Precompute output with the use of alternative (old) function implementation
MidnightToMidnight_JU_out_list <- lapply(dat_list, function(dat_i){
  MidnightToMidnight_JU(dat_i$vectormagnitude, dat_i$timestamp)
})



## Context ---------------------------------------------------------------------

context("Testing midnight_to_midnight()")

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same number of NA entries")
test_that(test_that_desc, {
  for (i in 1:length(dat_list)){
    out1 <- MidnightToMidnight_out_list[[i]]
    out2 <- MidnightToMidnight_JU_out_list[[i]]
    expect_equal(sum(!is.na(out1)),
                 sum(!is.na(out2)))
  }
})

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same location of NA entries")
test_that(test_that_desc, {
  for (i in 1:length(dat_list)){
    out1 <- MidnightToMidnight_out_list[[i]]
    out2 <- MidnightToMidnight_JU_out_list[[i]]
    expect_true(all(which(!is.na(out1)) == which(!is.na(out2))))
  }
})

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same values of non-NA entries")
test_that(test_that_desc, {
  for (i in 1:length(dat_list)){
    out1 <- MidnightToMidnight_out_list[[i]]
    out2 <- MidnightToMidnight_JU_out_list[[i]]
    expect_true( all(out1[!is.na(out1)] == out2[(!is.na(out2))]))
  }
})

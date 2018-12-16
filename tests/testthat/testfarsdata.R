datadir <- system.file("extdata", package = "farsdata")
oldwd <- setwd(datadir)

expect_that(length(fars_read_years(c(2013, 2014, 2015))), equals(3))

setwd(oldwd)

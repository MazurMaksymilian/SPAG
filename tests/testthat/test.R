#test_that("Checking data quality for Coverage Index", {
#  df <- data.frame(emp = c(5,8,2,4,14,11,3,6,9,6,2,14,5), category=c("A","A","B","B","B","C","C","C","C","A","A","A","A"))
#  expect_equal(calcCoverageIndex(df,unique(df[,2])), c(40/89,20/89,29/89, 1))
#  df <- data.frame(emp = c(2,4,14,11,3,6,9,6,2,14,5,5,8), category=c("B","B","B","C","C","C","C","A","A","A","A","A","A"))
#  expect_equal(calcCoverageIndex(df,unique(df[,2])), c(20/89,29/89,40/89, 1))
#})

test_that("Checking calculation of SPAG Index", {
  x <- SPAG(CompaniesPoland, MapPoland, theoreticalSample = nrow(CompaniesPoland), empiricalSample = nrow(CompaniesPoland))
  expect_equal(round(x$IDist,10),c(0.5843005101, 0.6327159350, 0.5348323911, 0.5477285319, 0.5009985312, 0.5190096150, 0.4991059020, 0.0000000000, 0.5922972215))
  expect_equal(round(x$IOver,10),c(0.2970702617, 0.3249806962, 0.2305102030, 0.2587197124, 0.2843701244, 0.2772325759, 0.3036286216, 1.0000000000, 0.2396641768))
  expect_equal(round(x$ICov,10),c(0.2537445603, 0.4299286509, 0.1515661370, 0.0859857302, 0.0223914584, 0.0356745269, 0.0206077320, 0.0001012043, 1.0000000000))
  })

test_that("Checking if additional parameters do not break calculation", {
  x <- SPAG(CompaniesPoland, MapPoland, theoreticalSample = nrow(CompaniesPoland), empiricalSample = nrow(CompaniesPoland),
            numberOfSamples = 5)
  expect_equal(round(x$IDist,10),c(0.5843005101, 0.6327159350, 0.5348323911, 0.5477285319, 0.5009985312, 0.5190096150, 0.4991059020, 0.0000000000, 0.5922972215))
  expect_equal(round(x$IOver,10),c(0.2970702617, 0.3249806962, 0.2305102030, 0.2587197124, 0.2843701244, 0.2772325759, 0.3036286216, 1.0000000000, 0.2396641768))
  expect_equal(round(x$ICov,10),c(0.2537445603, 0.4299286509, 0.1515661370, 0.0859857302, 0.0223914584, 0.0356745269, 0.0206077320, 0.0001012043, 1.0000000000))

  x_total <- SPAG(CompaniesPoland, MapPoland, totalOnly=TRUE, theoreticalSample = nrow(CompaniesPoland), empiricalSample = nrow(CompaniesPoland))
  x <- attr(x, "IndexDF")[9,2:5]
  x_total <- attr(x_total, "IndexDF")[1,2:5]
  rownames(x) <- NULL
  rownames(x_total) <- NULL
  expect_equal(x, x_total)
})

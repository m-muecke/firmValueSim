test_that("pv() works with sample data", {
  pv <- calc_pv(2000, 0.03, 3)
  expect_equal(round(pv, 3), 1830.283)
  
  pv <- calc_pv(420000, 0.05, 1)
  expect_equal(pv, 400000)
  
  pv <- calc_pv(114.49, 0.07, 2)
  expect_equal(pv, 100)
  
  pv <- calc_pv(0, 0, 0)
  expect_equal(pv, 0)
  
  pv <- calc_pv(0, 0.05, 3)
  expect_equal(pv, 0)
  
  pv <- calc_pv(100, 0.05, 0)
  expect_equal(pv, 100)
  
  pv <- calc_pv(100, 0, 5)
  expect_equal(pv, 100)
})

test_that("fv() works with sample data", {
  fv <- calc_fv(1000, 0.1, 5)
  expect_equal(fv, 1610.51)
  
  fv <- calc_fv(100, 0.07, 2)
  expect_equal(fv, 114.49)
  
  fv <- calc_fv(100, 0.05, 2)
  expect_equal(fv, 110.25)
  
  fv <- calc_fv(0, 0, 0)
  expect_equal(fv, 0)
  
  fv <- calc_fv(0, 0.05, 2)
  expect_equal(fv, 0)
  
  fv <- calc_fv(100, 0.05, 0)
  expect_equal(fv, 100)
  
  fv <- calc_fv(100, 0, 5)
  expect_equal(fv, 100)
})

test_that("npv() works with sample data", {
  cf <- rep(25000, 60)
  npv <- calc_npv(cf, 0.0064, 1000000)
  expect_equal(round(npv, 1), 242322.8)
  
  npv <- calc_npv(420000, 0.05, 370000) 
  expect_equal(npv, 30000)
  
  cf <- c(20000, 420000)
  npv <- calc_npv(cf, 0.12, 370000)
  expect_equal(round(npv, 2), -17321.43)
  
  npv <- calc_npv(0, 0, 0)
  expect_equal(npv, 0)
  
  npv <- calc_npv(rep(0, 6), 0, 0)
  expect_equal(npv, 0)
  
  npv <- calc_npv(rep(0, 6), 0.05, 0)
  expect_equal(npv, 0)
  
  npv <- calc_npv(rep(0, 6), 0.05, 1000)
  expect_equal(npv, -1000)
  
  npv <- calc_npv(rep(100, 6), 0, 1000)
  expect_equal(npv, -400)
}) 

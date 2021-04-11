test_that("ddm() works with sample data", {
  price <- ddm(1.5, 0.05, 0.03)
  expect_equal(price, 75)
  
  price <- ddm(1.89, 0.07, 0.05)
  expect_equal(price, 94.50)
  
  price <- ddm(1.89, 0.07, 0.045)
  expect_equal(price, 75.6)
  
  price <- ddm(2.12, 0.05, 0.02)
  expect_equal(round(price, 2), 70.67)
  
  price <- ddm(0, 0.05, 0.02)
  expect_equal(price, 0)
  
  price <- ddm(0, 0, 0.2)
  expect_equal(price, 0)
  
  price <- ddm(2, 0, 0.2)
  expect_equal(price, -10)
  
  price <- ddm(2, 0.05, 0)
  expect_equal(price, 40)
  
  expect_error(ddm(2, 0, 0))
})

test_that("ddm_sim() outputs correct vector length", {
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01) 
  expect_length(prices, 1000)
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01,
                    distribution = "normal") 
  expect_length(prices, 1000)
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01, n_sim = 5) 
  expect_length(prices, 5)
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01, n_sim = 100000) 
  expect_length(prices, 100000)
  
  expect_error(ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01, n_sim = 0))
  
  prices <- ddm_sim(1.8, r = 0.07, distribution = "triangle", g_min = 0.015,
                    g_max = 0.03)
  expect_length(prices, 1000)
  
  expect_error(ddm_sim(1.8, r = 0.07, distribution = "triangle", g_min = 0.015,
               g_max = 0.03, n_sim = 0))
  
  prices <- ddm_sim(1.8, r = 0.07, distribution = "uniform", g_min = 0.015,
                    g_max = 0.03, n_sim = 5000)
  expect_length(prices, 5000)
  
  expect_error(ddm_sim(1.8, r = 0.07, distribution = "uniform", g_min = 0.015,
                       g_max = 0.03, n_sim = 0))
})

test_that("ddm_sim() works for sample data", {
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01, n_sim = 1,
                    seed = 12345) 
  expect_equal(prices, 106.0467)
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0.01, n_sim = 3,
                    seed = 12345) 
  expect_equal(round(prices, 5), c(116.23096, 106.04670, 71.11353))
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0.03, g_sigma = 0, n_sim = 5,
                    seed = 12345) 
  expect_equal(prices, rep(75, 5))
  
  prices <- ddm_sim(1.5, r = 0.05, g_mu = 0, g_sigma = 0, n_sim = 5,
                    seed = 12345) 
  expect_equal(prices, rep(30, 5))
  
  prices <- ddm_sim(1.8, r = 0.07, distribution = "triangle", g_min = 0.015,
                    g_max = 0.03, n_sim = 3, seed = 12345)
  expect_equal(round(prices, 5), c(41.15378, 41.15378, 39.47072))
  
  prices <- ddm_sim(1.8, r = 0.1, distribution = "triangle", g_min = 0.01,
                    g_max = 0.05, n_sim = 5, seed = 12345)
  expect_equal(round(prices, 5),
               c(30.01550, 25.39138, 28.20078, 30.01550, 28.20078))
  
  prices <- ddm_sim(1.8, r = 0.07, distribution = "uniform", g_min = 0.015,
                    g_max = 0.03, n_sim = 3, seed = 12345)
  expect_equal(round(prices, 5), c(42.99698, 42.99698, 40.73648))
  
  prices <- ddm_sim(1.8, r = 0.1, distribution = "uniform", g_min = 0.01,
                    g_max = 0.03, n_sim = 5, seed = 12345)
  expect_equal(round(prices, 5),
               c(24.83288, 22.25784, 24.07049, 24.83288, 24.07049))
})

test_that("ddm_sim() fails as expected", {
  expect_error(ddm_sim(1.8, n_sim = 0))
  expect_error(ddm_sim(1.8, r = 0.07, distribution = "normal", n_sim = 0))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "triangle"))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "triangle", g_min = 0.01))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "triangle", g_max = 0.03))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "uniform"))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "uniform", g_min = 0.01))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "uniform", g_max = 0.03))
  expect_error(ddm_sim(1.5, r = 0.07, distribution = "gaussian"))
  expect_error(ddm_sim(1.8, r = 0.07, distribution = "normal"))
  expect_error(ddm_sim(1.8, r = 0.07, g_mu = 0.03))
  expect_error(ddm_sim(1.8, r = 0.07, g_sigma = 0.03))
})
  
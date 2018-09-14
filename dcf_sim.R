library(ggplot2)
library(FinCal)

cash_flow <- c(0, 68327, 64653, 61177, 57887, 54773)
future_value <- 1737330
debt ^. ^^^^^^llööäää^^^^^<<<<<<<<<<>>>>>>>><<<<<^^----^^^^^--------<<^^^^^^<<^^^^√√√√√√√^^^^^°°°
monte_carlo <- function(n_sim = 100000, CF = cash_flow, FV = future_value, debt = 8777, shares = 36228, 
                        wacc_mu = 7.0, wacc_sigma = 1.5, g_mu = 3.0, g_sigma = 0.5, years = 5){
  ## monte carlo sim for unlevered free cash flow model with perpetuity growth method
  set.seed(1729) #optional, done for replication purposes
  wacc <- rnorm(1:n_sim, mean = wacc_mu, sd = wacc_sigma)
  g <- rnorm(1:n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    wacc_t <- sample(wacc, size = 1, replace = TRUE)
    all_pv <- npv(r = wacc_t, cf = CF)
    fcf <- - pv(r = wacc_t, fv = FV, n = years)
    fcf_1 <-  fcf * (1 + g_t)
    terminal_value <- fcf_1 / (wacc_t - g_t)
    pv_terminal_value <- - pv(r = wacc_t, fv = terminal_value, n = years)
    enterprise_value <- pv_terminal_value + all_pv
    net_debt <- debt
    equity_value <- enterprise_value - net_debt
    shares <- shares
    value_per_share <- equity_value / shares
    share_values[i] <- values_per_share
  }
  print(boxplot.stats(share_values)$out)
  return(share_values)
}

distribution_plot <- function(){
  values <-  monte_carlo()
  hist(values, probability = FALSE, main = "Distribution function of simulated stock prices",
       xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 25)
}

hist(rnorm(1:n_sim, mean = 11.1, sd = 0.5), probability = FALSE, main = "Normalverteilung WACC",
     xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 50)

hist(rnorm(1:n_sim, mean = 2.41, sd = 0.5), probability = FALSE, main = "Normalverteilung Wachstumsrate",
     xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 50)

hist(rnorm(1:n_sim, mean = 2.68, sd = 0.3), probability = FALSE, main = "Monte Carlo Simulation",
     xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 100)

tmp <- (rnorm(1:n_sim, mean = 95.25, sd = 0.5))


rtriangle(1:n_sim, a = 0.5, b = 1.5)


density_plot <- function(){
  values <-  monte_carlo()
  hist(values, probability = TRUE, main = "Density function of simulated stock prices",
       xlab = "", ylab = "", cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 25)
  density <- density(values)
  lines(density, col = "red", lwd = 2)
}


### code below still filled with bugs i.e. not review
df_values <- data.frame(prices = monte_carlo(), upside = monte_carlo() >= target_price)
p <- ggplot(data = df_values, aes(prices)) +
      geom_histogram(bins = 50, color = "black", fill = "white") + 
      xlab("Stock Price") +
      ylab("Frequency") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x) +
      xlim(quantile(df_values$prices)[2] - 1.5 * IQR(df_values$prices), quantile(df_values$prices)[4] + 1.5 * IQR(df_values$prices))
        
      web#geom_vline(aes(xintercept = median(df_values$prices)), colour = "#990000", linetype = "dashed") +
      #geom_vline(aes(xintercept = target_price), colour = "blue", linetype = "dashed") +
      #scale_fill_manual(values = c("grey", "green"))
      #scale_fill_manual(values = ifelse(df_values$prices >= target_price, "blue", "red")[-length(df_values$prices)])

#calculating prob. of achieving upside
target_price <- 6
summary(df_values$prices)
cat("Probabilty of achieving a upside is:", length(df_values$prices[df_values$prices >= target_price]) / length(df_values$prices))

value_at_x_perc <- sort(df_values$prices)[0.5 * length(df_values$prices)]
cat("Upside when probabilty is 50%:", ((median(df_values$prices) / target_price) - 1))

density(df_values$prices)

pdf_plot <- function() {
  plot(df_values$`monte_carlo()`)
  hist(df_values$`monte_carlo()`, probability = TRUE, main = "Propability Density Function of Simulated Share Prices",
       ylab = "", xlab = "", cex.lab = 2.5, cex.axis = 2.5, cex.main = 2.5, cex.sub = 1.5, breaks = 100)
  density <- density(df_values$`monte_carlo()`)
  lines(density, col = "red", lwd = 2)
  curve(dnorm(x, mean = mean(df_values$`monte_carlo()`), sd = sd(df_values$`monte_carlo()`)), add = TRUE, col = "darkblue", lwd = 2)
  summary(density$x)
}

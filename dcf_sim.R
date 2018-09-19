library(ggplot2)
library(FinCal)

dcf_sim_norm <- function(n_sim, CF, FV, debt, shares, wacc_mu, wacc_sigma, g_mu, g_sigma, years){
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
  values <-  dcf_sim_norm()
  hist(values, probability = FALSE, main = "Distribution function of simulated stock prices",
       xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 25)
}

density_plot <- function(){
  values <-  dcf_sim_norm()
  hist(values, probability = TRUE, main = "Density function of simulated stock prices",
       xlab = "", ylab = "", cex.lab = 2.5, cex.axis = 2.5, cex.main = 2, cex.sub = 1.5, breaks = 25)
  density <- density(values)
  lines(density, col = "red", lwd = 2)
}

#df_values <- data.frame(prices = monte_carlo(), upside = monte_carlo() >= target_price)
#p <- ggplot(data = df_values, aes(prices)) +
#      geom_histogram(bins = 50, color = "black", fill = "white") + 
#      xlab("Stock Price") +
#      ylab("Frequency") +
#      theme(panel.grid.major.x = element_blank(),
#            panel.grid.minor.x) +
#      xlim(quantile(df_values$prices)[2] - 1.5 * IQR(df_values$prices), quantile(df_values$prices)[4] + 1.5 * IQR(df_values$prices))
#        
#      #geom_vline(aes(xintercept = median(df_values$prices)), colour = "#990000", linetype = "dashed") +
#      #geom_vline(aes(xintercept = target_price), colour = "blue", linetype = "dashed") +
#      #scale_fill_manual(values = c("grey", "green"))
#      #scale_fill_manual(values = ifelse(df_values$prices >= target_price, "blue", "red")[-length(df_values$prices)])
#
##calculating prob. of achieving upside
#target_price <- 6
#summary(df_values$prices)
#cat("Probabilty of achieving a upside is:", length(df_values$prices[df_values$prices >= target_price]) / length(df_values$prices))
#
#value_at_x_perc <- sort(df_values$prices)[0.5 * length(df_values$prices)]
#cat("Upside when probabilty is 50%:", ((median(df_values$prices) / target_price) - 1))
#
#density(df_values$prices)
#
#pdf_plot <- function() {
#  plot(df_values$`monte_carlo()`)
#  hist(df_values$`monte_carlo()`, probability = TRUE, main = "Propability Density Function of Simulated Share Prices",
#       ylab = "", xlab = "", cex.lab = 2.5, cex.axis = 2.5, cex.main = 2.5, cex.sub = 1.5, breaks = 100)
#  density <- density(df_values$`monte_carlo()`)
#  lines(density, col = "red", lwd = 2)
#  curve(dnorm(x, mean = mean(df_values$`monte_carlo()`), sd = sd(df_values$`monte_carlo()`)), add = TRUE, col = "darkblue", lwd = 2)
#  summary(density$x)
#}

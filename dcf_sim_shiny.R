library(ggplot2)
library(shiny)
library(FinCal)
library(scales)

# define UI for application that plots monte carlo
ui <- fluidPage(
  #sidebar layout with a input and output definition
  sidebarLayout(
    #inputs select variables to plot
    sidebarPanel(
      #select adj. dynamic parameters in model
      numericInput(inputId = "x", label = "Number of Simulations", 1000),
      numericInput(inputId = "wacc_mu", label = "Expected Value of WACC", 7.0),
      numericInput(inputId = "wacc_sigma", label = "Standard Deviation of WACC", 1.5),
      numericInput(inputId = "g_mu", label = "Expected Value of g", 3.0),
      numericInput(inputId = "g_sigma", label = "Standard Deviation of g", 0.5),
      numericInput(inputId = "years", label = "Amount of Years", 5),
      numericInput(inputId = "target_price", label = "Target Price", value = 6)
    ),
    mainPanel(
      plotOutput(outputId = "histogram")
    )
  )
)
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$histogram <- renderPlot({
    cash_flow <- c(0, 1765594, 1550571, 1411615, 1330523, 1296891)
    future_value <- 1737330
    monte_carlo <- function(n_sim = input$x, CF = cash_flow, FV = future_value, debt = 8777, shares = 36228, 
                            wacc_mu = input$wacc_mu, wacc_sigma = input$wacc_sigma, g_mu = input$g_mu, g_sigma = input$g_sigma, years = input$years){
      ## monte carlo sim for unlevered free cash flow model with perpetuity growth method
      #set.seed(1729) #optional, done for replication purposes
      wacc <- rnorm(1:n_sim, mean = wacc_mu, sd = wacc_sigma)
      g <- rnorm(1:n_sim, mean = g_mu, sd = g_sigma)
      share_values <- c()
      for(i in 1:n_sim){
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
        share_values <- c(share_values, value_per_share)
      }
      return(share_values)
    }
    df_values <- as.data.frame(monte_carlo())
    colnames(df_values) <- "prices"
    ggplot(data = df_values, aes_string(x = df_values$prices)) +
      geom_histogram(bins = 50) + 
      xlab("Stock Price") +
      ylab("Frequency") +
      xlim(quantile(df_values$prices)[2] - 1.5 * IQR(df_values$prices), quantile(df_values$prices)[4] + 1.5 * IQR(df_values$prices)) +
      ggtitle("Histogram of simulated stock prices") +
      theme(plot.title = element_text(lineheight = .8, face="bold")) +
      geom_vline(aes(xintercept = median(df_values$prices)), colour = "#990000", linetype = "dashed") +
      geom_vline(aes(xintercept = input$target_price), colour = "blue", linetype = "dashed") +
      scale_fill_manual(values = ifelse(df_values$prices >= input$target_price, "grey", "red")[-length(h$breaks)])
  })
}
# Create a Shiny app object
shinyApp(ui = ui, server = server)
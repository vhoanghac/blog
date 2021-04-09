component_matrix <- function(returns, w){
  
  # Covariance matrix
  cov_matrix <- cov(returns)
  
  # Portfolio stdev
  sd_portfolio <- sqrt(t(w) %*% cov_matrix %*% w)
  
  # Marginal contribution:
  marginal_contribution <- w %*% cov_matrix / sd_portfolio[1, 1]
  
  component_contribution <- marginal_contribution * w
  
  # Percentages:
  component_percentages <- component_contribution / sd_portfolio[1,1]
  
  component_percentages %>% 
    as_tibble() %>% 
    gather(symbol, contribution)
}

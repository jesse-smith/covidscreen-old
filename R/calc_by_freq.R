expand_freq <- function(f_vac, f_unvac) {
  f_vac <- 1 / params$test$p_asymp_vac
  f_unvac <- 1 / params$test$p_asymp_unvac
  CJ(f_vac = seq_len(f_vac), f_unvac = seq_len(f_unvac))
}

calc_tests <- function(
  n = 100,
  f_vac,
  f_unvac,
  params
) {
  do.call()
  n * sum(dist[dist$test]$p)
}

calc_inf_detect <- function(n = 100, dist = calc_dist()) {
  i_dist <- dist_data(dist[dist$inf], cols = "detect")

  i_total <- n * dist[dist$inf, "p"]

  i_detected <- n

  c(
    detected =
  )
}

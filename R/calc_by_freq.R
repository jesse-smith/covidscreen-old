expand_freq <- function(f_vac, f_unvac) {
  CJ(t_v = seq_len(1 / f_vac), t_u = seq_len(1 / f_unvac))
}

replace_freq <- function(params, t_v, t_u) {
  modifyList(
    params,
    val = list(
      test = list(
        p_asymp_vac   = 1 / t_v,
        p_asymp_unvac = 1 / t_u
      )
    )
  )
}

calc_tests <- function(n = 100, params, t_v, t_u) {
  dist <- calc_dist_cache(replace_freq(params, t_v = t_v, t_u = t_u))
  reactive(n * sum(dist()[dist()$test]$p))
}

calc_detected <- function(n = 100, params, t_v, t_u) {
  dist <- calc_dist_cache(replace_freq(params, t_v = t_v, t_u = t_u))
  reactive(n * sum(dist()[dist()$inf & dist()$detect]$p))
}

calc_freq <- function(n, params) {
  # Create parameter grid for testing frequencies
  t_df <- expand_freq(params$test$p_asymp_vac, params$test$p_asymp_unvac)

  t_v_lvls <- sort(unique(t_df$t_v))
  t_u_lvls <- sort(unique(t_df$t_u))

  dist_inf <- calc_dist_cache(params)
  n_inf    <- reactive(n * sum(dist_inf()[dist_inf()$inf]$p))

  # Calculate distribution for each parameter set
  reactive(t_df[,
                c("n", "n_inf", "tests", "detected") := list(
                  n,
                  n_inf(),
                  calc_tests(n = n, params = params, t_v = .SD$t_v, t_u = .SD$t_u)(),
                  calc_detected(n = n, params = params, t_v = .SD$t_v, t_u = .SD$t_u)()
                ),
                by = seq_len(NROW(t_df))
  ][,
    c("undetected", "pct_tested", "pct_active", "pct_detected", "pct_undetected") := list(
      .SD$n_inf - .SD$detected,
      100 * .SD$tests / .SD$n,
      100 * .SD$n_inf / .SD$n,
      100 * .SD$detected / .SD$n_inf,
      100 * (n_inf() - .SD$detected) / .SD$n_inf
    )
  ][,
    c("freq_txt", "t_v_fct", "t_u_fct") := list(
      paste0(
        "Tests per Day: ", round(.SD$tests, 1), " (", round(.SD$pct_tested, 1), "&#37; of organization)<br>",
        "Undetected Cases: ", round(.SD$undetected, 1), " (", round(.SD$pct_undetected, 1), "&#37; of active cases)<br>",
        "<b>Vax: ", .SD$t_v, " days | Unvax: ", .SD$t_u, " days </b>"
      ),
      factor(.SD$t_v, levels = t_v_lvls, labels = paste(t_v_lvls, "days")),
      factor(.SD$t_u, levels = t_u_lvls, labels = paste(t_u_lvls, "days"))
    )
  ])
}

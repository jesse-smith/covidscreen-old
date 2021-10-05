# Export -----------------------------------------------------------------------

#' Calculate the Steady-State Joint Distribution for a Population
#'
#' `calc_dist()` calculates the discrete joint distribution of vaccination,
#' infection, symptoms, tests, and detections in a population.
#'
#' @param incid `[numeric(1)]` Incidence rate per 100k per day in the community
#'
#' @param vac `[list(3)]` A named list containing vaccination parameters:
#'   \describe{
#'     \item{p_comm `[numeric(1)]`}{Proportion vaccinated in the community}
#'     \item{p_org `[numeric(1)]`}{Proportion vaccinated in the organization of interest}
#'     \item{eff `[numeric(1)]`}{Vaccine efficacy}
#'   }
#'
#' @param inf `[list(3)]` A named list containing infection parameters:
#'   \describe{
#'     \item{p_symp `[numeric(1)]`}{Proportion of infections eventually showing symptoms}
#'     \item{t_inf `[numeric(1)]`}{Duration of infectious period}
#'     \item{t_presymp `[numeric(1)]`}{Duration of presymptomatic period}
#'   }
#'
#' @param test `[list(2)]` A named list containing testing parameters:
#'   \describe{
#'     \item{p_symp `[numeric(1)]`}{Probability of being tested if symptomatic}
#'     \item{p_asymp `[numeric(1)]`}{Probability of being tested if asymptomatic}
#'   }
#'
#' @param detect `[list(2)]` A named list containing detection parameters:
#'   \describe{
#'     \item{sens `[numeric(1)]`}{Test sensitivity}
#'     \item{spec `[numeric(1)]`}{Test specificity}
#'   }
calc_dist <- function(
  # Vaccination parameters
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.7),
  # Infection parameters
  inf = list(p_incid = 0.5, p_symp = 0.5, t_symp = 10, t_presymp = 3),
  # Symptom parameters
  symp   = list(p_vac_inf = 0.5, p_unvac_inf = 0.5, p_uninf = 0),
  # Test parameters
  test   = list(p_symp = 0.95, p_asymp = 1/7),
  # Detection parameters
  detect = list(sens = 0.85, spec = 1)
) {

  # Check arguments
  # assert_args(
  #   incid = incid,
  #   vac = vac,
  #   inf = inf,
  #   test = test,
  #   detect = detect
  # )

  # Create conditional distributions
  dt_vac    <- dist_vac(vac)
  dt_inf    <- dist_inf(inf, .vac = vac)
  dt_symp   <- dist_symp(inf)
  dt_test   <- dist_test(test)
  dt_detect <- dist_detect(detect)

  # Create joint distribution
  dt_vac %>%
    join_dist(dt_inf) %>%
    join_dist(dt_symp) %>%
    join_dist(dt_test) %>%
    join_dist(dt_detect) %>%
    # Set key and reorder columns
    setcolorder(c("p", "vac", "inf", "symp", "test", "detect")) %>%
    setorderv(order = -1L, na.last = TRUE) %>%
    # Workaround to print on return after modify-by-reference
    .[]
}

# Join Distributions -----------------------------------------------------------

join_dist <- function(x, y) {
  # Join by all common columns except probability (`p`)
  cols_common <- intersect(colnames(x), colnames(y))
  by <- cols_common[!cols_common %chin% "p"]
  # Left join distribution data tables
  d <- merge.data.table(
    x,
    y,
    by = by,
    all.x = TRUE,
    suffixes = c("", "_y"),
    allow.cartesian = TRUE
  )

  # Fill missing conditional probs
  setnafill(d, fill = 1, cols = "p_y")
  # Multiply probabilities and remove conditional probs
  d[, "p" := .SD$p * .SD$p_y][, "p_y" := NULL]
}

# Create Distributions ---------------------------------------------------------

dist_vac <- function(.vac) {
  create_dist(
    # Probs conditional on vaccination status
    vac = c(TRUE, FALSE),
    .p  = probs_vac(.vac)
  )
}

dist_inf <- function(.inf, .vac) {
  create_dist(
    # Probs conditional on vaccination and infection status
    vac = c(TRUE, FALSE, TRUE, FALSE),
    inf = c(TRUE, TRUE, FALSE, FALSE),
    .p   = probs_inf(.inf, vac = .vac)
  )
}

dist_symp <- function(.inf) {
  create_dist(
    # Probs conditional on infection and symptomatic status
    inf  = c(TRUE, FALSE, TRUE, FALSE),
    symp = c(TRUE, TRUE, FALSE, FALSE),
    .p   = probs_symp(.inf)
  )
}

dist_test <- function(.test) {
  create_dist(
    # Probs conditional on symptoms and test status
    symp = c(TRUE, FALSE, TRUE, FALSE),
    test = c(TRUE, TRUE, FALSE, FALSE),
    .p   = probs_test(.test)
  )
}

dist_detect <- function(.detect) {
  create_dist(
    # Probabilities conditional on infection, testing, and detection
    inf    = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    test   = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    detect = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    .p     = probs_detect(.detect)
  )
}

create_dist <- function(..., .p) {
  # Create data table
  dt  <- data.table(..., p = .p)
  # Create key from all columns except `p`
  cols <- colnames(dt)
  key  <- cols[!cols %chin% "p"]
  # Set key and return
  setkeyv(dt, cols = key)
}

# Probability Calculations -----------------------------------------------------

probs_vac <- function(vac) {
  # Add complement and return
  c(vac$p_org, 1 - vac$p_org)
}

probs_inf <- function(inf, vac) {
  # Uncorrected incidence "probabilities" within each group (may be > 1)
  p_incid_u <- inf$p_incid / (1 - vac$p_comm * vac$eff)
  p_incid_v <- p_incid_u * (1 - vac$eff)
  # Combine
  p_incid <- c(v = p_incid_v, u = p_incid_u)
  # Scale from incidence to infection probabilities (assuming constant t_inf)
  p_inf   <- pmin(1, p_incid * (inf$t_symp + inf$t_presymp))
  # Add complements and return
  # (order is {inf_vac, inf_unvac, uninf_vac, uninf_unvac})
  c(p_inf, 1 - p_inf)
}

probs_symp <- function(inf) {
  # Account for presymptomatic illness
  p_symp <- inf$p_symp * (1 - inf$t_presymp / (inf$t_presymp + inf$t_symp))
  # Add complement and return
  c(p_symp, 0, 1 - p_symp, 1)
}

probs_test <- function(test) {
  # Symptomatic is <= asymptomatic
  p_symp <- max(test$p_symp, test$p_asymp)
  # Combine
  p_test <- c(p_symp, test$p_asymp)
  # Add complements and return
  c(p_test, 1 - p_test)
}

probs_detect <- function(detect) {
  # Combine and add probabilities for not tested (0)
  p_d <- c(detect$sens, 1 - detect$spec, 0, 0)

  # Add complements and return
  c(p_d, 1 - p_d)
}

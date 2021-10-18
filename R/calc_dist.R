# Export -----------------------------------------------------------------------

#' Calculate the Steady-State Joint Distribution for a Population
#'
#' `calc_dist()` calculates the discrete joint distribution of vaccination,
#' infection, symptoms, tests, and detections in a population.
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
#'     \item{p_incid `[numeric(1)]`}{Proportion of community newly infected each day}
#'     \item{t_symp `[numeric(1)]`}{Duration of symptomatic period}
#'     \item{t_presymp `[numeric(1)]`}{Duration of presymptomatic period}
#'   }
#'
#' @param symp `[list(3)]` A named lust containing symptom parameters:
#'   \describe{
#'     \item{p_inf_vac}{Proportion of vaccinated infections who are symptomatic}
#'     \item{p_inf_unvac}{Proportion of unvaccinated infections who are symptomatic}
#'     \item{p_uninf}{Proportion of uninfected people who are symptomatic}
#'   }
#'
#' @param test `[list(3)]` A named list containing testing parameters:
#'   \describe{
#'     \item{p_symp `[numeric(1)]`}{Probability of being tested if symptomatic}
#'     \item{p_asymp_vac `[numeric(1)]`}{Probability of being tested if asymptomatic and vaccinated}
#'     \item{p_asymp_unvac `[numeric(1)]`}{Probability of being tested if asymptomatic and unvaccinated}
#'   }
#'
#' @param detect `[list(2)]` A named list containing detection parameters:
#'   \describe{
#'     \item{sens `[numeric(1)]`}{Test sensitivity}
#'     \item{spec `[numeric(1)]`}{Test specificity}
#'   }
#'
#' @return A `data.table`
#'
#' @export
calc_dist <- function(
  # Vaccination parameters
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.7),
  # Infection parameters
  inf = list(p_incid = 1e-3, t_symp = 10, t_presymp = 3),
  # Symptom parameters
  symp   = list(p_inf_vac = 0.5, p_inf_unvac = 0.5, p_uninf = 0),
  # Test parameters
  test   = list(p_symp = 0.95, p_asymp_vac = 1/7, p_asymp_unvac = 1/7),
  # Detection parameters
  detect = list(sens = 0.85, spec = 1)
) {

  # Create conditional distributions
  dt_vac    <- dist_vac(vac)
  dt_inf    <- dist_inf(inf, .vac = vac)
  dt_symp   <- dist_symp(symp, .inf = inf)
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

#' Cached `calc_dist()`
#'
#' A version of `calc_dist()` that caches outputs based on previous inputs. This
#' is done using `bindCache()`, which allows the cache to be shared across user
#' sessions. It returns a reactive expression.
#'
#' @param params A named `list` of arguments to `calc_dist()`
#'
#' @return A `reactive` expression
#'
#' @keywords internal
#'
#' @export
calc_dist_cache <- function(params) {
  bindCache(
    x = reactive(do.call(calc_dist, args = params)),
    params,
    cache = "app"
  )
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

dist_symp <- function(.symp, .inf) {
  create_dist(
    # Probs conditional on vaccination, infection, and symptomatic status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    inf  = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    symp = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    .p   = probs_symp(.symp, .inf)
  )
}

dist_test <- function(.test) {
  create_dist(
    # Probs conditional on vaccination, symptom, and test status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    symp = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    test = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
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

probs_symp <- function(symp, inf) {
  # Account for presymptomatic illness
  p_presymp <- (1 - inf$t_presymp / (inf$t_presymp + inf$t_symp))
  # Combine symptoms probs for infections into scaled vector
  p_symp_inf <- c(symp$p_inf_vac, symp$p_inf_unvac) * p_presymp
  # Combine all symptom probs
  # (order is {inf_vac, inf_unvac, uninf_vac, uninf_unvac})
  p_symp <- c(p_symp_inf, symp$p_uninf, symp$p_uninf)
  # Add complement and return
  c(p_symp, 1 - p_symp)
}

probs_test <- function(test) {
  # Combine asymptomatic probabilities
  p_asymp <- c(test$p_asymp_vac, test$p_asymp_unvac)
  # Symptomatic is <= asymptomatic (and length 2)
  p_symp <- pmax(test$p_symp, p_asymp)
  # Combine symptomatic and asymptomatic
  # (order is {vac_symp, unvac_symp, vac_asymp, unvac_asymp})
  p_test <- c(p_symp, p_asymp)
  # Add complements and return
  c(p_test, 1 - p_test)
}

probs_detect <- function(detect) {
  # Combine and add probabilities for not tested (0)
  p_d <- c(detect$sens, 1 - detect$spec, 0, 0)
  # Add complements and return
  c(p_d, 1 - p_d)
}

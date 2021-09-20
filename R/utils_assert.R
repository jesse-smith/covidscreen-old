# Assertions -------------------------------------------------------------------

#' Check that an Argument Is Non-Negative
#'
#' `assert_non_negative()` check that its input is a non-`NULL`, non-missing,
#' and non-negative numeric. By default, it checks whether then input is scalar
#' and finite, but this can be relaxed with `scalar = FALSE` and
#' `finite = FALSE`, respectively.
#'
#' @param x `[any]` Object to check.
#'
#' @param upper `[numeric(1)]` Upper value all elements of `x` must be lower
#'   than or equal to.
#'
#' @param scalar `[logical(1)]` Should `x` be a scalar?
#'
#' @param finite `[logical(1)]` Should `x` have only finite values?
#'
#' @param .var.name `[character(1)]` Name of the checked object to print in
#'   assertions.
#'
#' @param add `[AssertCollection]` Collection to store assertion messages. See
#'   `checkmate::AssertCollection`.
#'
#' @return If successful, returns `x` invisibly. If unsuccessful, throws an
#'   error message.
#'
#' @noRd
assert_non_negative <- function(
  x,
  upper = Inf,
  scalar = TRUE,
  finite = TRUE,
  .var.name = checkmate::vname(x),
  add = NULL
) {

  assert_bool(scalar)

  if (scalar) {
    checkmate::assert_number(
      x,
      lower = 0,
      upper = upper,
      finite = finite,
      .var.name = .var.name,
      add = add
    )
  } else {
    checkmate::assert_numeric(
      x,
      lower = 0,
      upper = upper,
      finite = finite,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = .var.name,
      add = add
    )
  }
}

#' Check that an Argument is Boolean
#'
#' `assert_bool()` checks whether its argument is a single `TRUE` or `FALSE`
#' value. Missing, `NULL`, and vector inputs are not allowed.
#'
#' @param x `[any]` Object to check.
#'
#' @param .var.name `[character(1)]` Name of the checked object to print in
#'   assertions.
#'
#' @param add `[AssertCollection]` Collection to store assertion messages. See
#'   `checkmate::AssertCollection`.
#'
#' @return If successful, returns `x` invisibly. If unsuccessful, throws an
#'   error message.
#'
#' @noRd
assert_bool <- function(x, .var.name = checkmate::vname(x), add = NULL) {
  checkmate::assert_logical(
    x,
    any.missing = FALSE,
    len = 1,
    .var.name = .var.name,
    add = add
  )
}

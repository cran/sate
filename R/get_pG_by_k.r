#' Calculates vector of probabilities that jury with jury_n will return a guilty verdict
#'
#' @param jury_n Size of the jury (i.e. 6, 8, or 12); default value is 6.
#' @return Returns a vector of probabilities for guilty verdict of size jury_n + 1.
#' @description Calculates a vector probabilities that a jury with jury_n will return a guilty verdict.
#'              The vector represents P(G|k) for 0, 1, 2, ... , jury_n where k is the number of jurors
#'              initially in favor of guilty verdict.
#' @examples
#'    library(sate)
#'    get_pG_by_k(10)
#'
#'    get_pG_by_k(jury_n=12)
#' @export
get_pG_by_k <- function(jury_n=6)
{
  if(base::missing(jury_n)) stop("Missing jury_n value.")
  if(!base::is.numeric(jury_n) || (jury_n %% 1 != 0) || (jury_n <= 0)) stop("jury_n must be positive integer.")
  # jury_n = 12
  P_matrix <- transition.matrix(jury_n = jury_n)
  A_matrix <- P_matrix[, c(1, jury_n+1)]
  R_matrix <- P_matrix[c(1, jury_n+1), -c(1, jury_n+1)]
  Q_matrix <- P_matrix[-c(1, jury_n+1), -c(1, jury_n+1)]
  I_matrix <- diag(jury_n - 1)
  B_matrix <- R_matrix %*% solve(I_matrix - Q_matrix)
  pG_by_k <- c(0, B_matrix[2,], 1)
  return(pG_by_k)
}

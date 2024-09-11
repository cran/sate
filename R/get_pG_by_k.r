#' Calculates vector of probabilities that jury with n_jurors will return a guilty verdict
#'
#' @param n_jurors Size of the jury (i.e. 6, 8, or 12); default value is 6.
#' @return Returns a vector of probabilities for guilty verdict of size n_jurors + 1.
#' @description Calculates a vector probabilities that a jury with n_jurors will return a guilty verdict.
#'              The vector represents P(G|k) for 0, 1, 2, ... , n_jurors where k is the number of jurors
#'              initially in favor of guilty verdict.
#' @examples
#'    library(sate)
#'    get_pG_by_k(10)
#'
#'    get_pG_by_k(n_jurors=12)
#' @export
get_pG_by_k <- function(n_jurors=6)
{
  if(base::missing(n_jurors)) stop("Missing jury_n value.")
  if(!base::is.numeric(n_jurors) || (n_jurors %% 1 != 0) || (n_jurors <= 0)) stop("jury_n must be positive integer.")
  n_g <- 1:(n_jurors-1)
  margin_vote_prob_g <- .5*((n_g-1) / n_jurors) + .25
  prob_add_g <- margin_vote_prob_g^2
  prob_lose_g <-(1-margin_vote_prob_g)^2
  prob_same_g <- 1 - prob_add_g - prob_lose_g
  # start with all zeroes
  transition_matrix <- matrix(nrow = n_jurors+1, ncol = n_jurors+1, byrow = T,
                              data = rep(0, (n_jurors+1)^2)) # dimnames = list(0:n_jurors, 0:n_jurors)
  # absorbing states
  transition_matrix[1,1] <- 1
  transition_matrix[n_jurors+1, n_jurors+1] <- 1
  for(i in 1:(n_jurors-1))
  {
    # stay same probabilities
    transition_matrix[i+1, i+1] <- prob_same_g[i]
    # lose one probabilities
    transition_matrix[i, i+1] <- prob_lose_g[i]
    # gain one probabilities
    transition_matrix[i+2, i+1] <- prob_add_g[i]
  }
  # round(transition_matrix, 4)
  next_matrix <- transition_matrix %*% transition_matrix
  while(sum(next_matrix[-c(1,n_jurors+1),]) > 1e-16)
  {
    next_matrix <- next_matrix %*% transition_matrix
  }
  pG_by_k <- next_matrix[n_jurors+1,]
  # table <- cbind(k=0:n_jurors, pG=round(pG_by_k, 5))
  # print(table)
  return(pG_by_k)
}

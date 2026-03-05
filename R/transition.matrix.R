#' Creates and Returns a Transition Probability Matrix for Deliberating Criminal Jury.
#'
#' @param jury_n The number of jurors.
#' @return A matrix of transition probabilities.
#' @description Returns a (jury_n + 1) by (jury_n + 1) matrix of probabilities.
#' Columns represent current state and rows represent next state. Column values sum to 1.
#' Depending on use, you may want to transpose rows and columns.
#' @examples
#'    library(sate)
#'
#'    transition.matrix(6)
#'
#'    transition.matrix(jury_n=12)
#' @export
transition.matrix <- function(jury_n)
{
  assert_required(jury_n)
  assert_positive_integer(jury_n)
  n_g <- 1:(jury_n-1)
  margin_vote_prob_g <- .5*((n_g-1) / jury_n) + .25
  prob_add_g <- margin_vote_prob_g^2
  prob_lose_g <-(1-margin_vote_prob_g)^2
  prob_same_g <- 1 - prob_add_g - prob_lose_g
  # start with all zeroes
  transition_matrix <- matrix(nrow = jury_n+1, ncol = jury_n+1, byrow = T,
                              data = rep(0, (jury_n+1)^2))
  # absorbing states
  transition_matrix[1,1] <- 1
  transition_matrix[jury_n+1, jury_n+1] <- 1
  for(i in 1:(jury_n-1))
  {
    # stay same probabilities
    transition_matrix[i+1, i+1] <- prob_same_g[i]
    # decrease by one probabilities
    transition_matrix[i, i+1] <- prob_lose_g[i]
    # increase by one probabilities
    transition_matrix[i+2, i+1] <- prob_add_g[i]
  }
  return(transition_matrix)
}

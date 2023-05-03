
sample_B_random <- function(n, p){
  matrix(sample(-1:1, n * p, replace = TRUE), n, p)
}

sample_B_alt <- function(n, P){
  Br <- diag(sample(c(-1,1), n, replace = TRUE))
  while (ncol(Br) < P) {
    Br <- Br %>% cbind(diag(sample(c(-1,1), n, replace = TRUE)))
  }
  Br <- Br[,1:P]
  Br
}

sample_B_seq <- function(n, p){
  Bo <- matrix(nrow = n, ncol = 0)
  for (i in 1:length(p)) {
    M <- matrix(0, n, p[i])
    M[((i-1) %% n)+1, ] <- sample(c(-1,1), p[i], replace = TRUE)
    Bo <- cbind(Bo, M)
  }
  Bo
}

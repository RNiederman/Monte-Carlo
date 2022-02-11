# https://fivethirtyeight.com/features/a-riddle-that-will-make-you-scream/

library(magrittr)
library(doParallel)
library(foreach)

as.percent <- function(p, decs = 0, raw = TRUE) {
  frmt <- paste0("%1.", decs, "f%%")
  multi <- ifelse(!raw, 1, 100)
  sprintf(frmt, p * multi)
}

N.Actors <- 5
N.Cast <- 3

Actors <- LETTERS[1:N.Actors]
Cast <- LETTERS[1:N.Cast] 

Scream <- function(cast.size = 3) {
  Guess <- sample(Actors, cast.size)
  Correct <- sum(Guess %in% Cast)
  return(Correct)
}

runs <- 999999 + 1

Starter <- Sys.time()
c <- detectCores()
c <- ifelse(c > 5, 5, c - 1)

while (runs %% c != 0) {
  runs <- runs + 1
}
runs.0 <- runs/c

registerDoParallel(cores = c)
Q <- foreach(j = 1:c, .combine = 'c') %dopar% {replicate(runs.0, Scream(3) ) }
stopImplicitCluster()

ST <- Sys.time() - Starter

which(Q>=2) %>% length %>% divide_by(length(Q)) %>% as.percent(2)
table(Q)
print(ST)

rm(runs, runs.0)
rm(N.Actors, N.Cast)
rm(c, ST, Starter)
rm(Actors, Cast)
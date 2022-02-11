# https://fivethirtyeight.com/features/a-riddle-that-will-make-you-scream/

library(magrittr)
library(pbapply)

as.percent <- function(p, decs = 0, raw = TRUE) {
  frmt <- paste0("%1.", decs, "f%%")
  multi <- ifelse(!raw, 1, 100)
  sprintf(frmt, p * multi)
}

Scream <- function(n = 5, cast.size = 3) {
  Actors <- LETTERS[1:n]
  Cast <- LETTERS[1:cast.size]
  Guess <- sample(Actors, cast.size)
  Correct <- sum(Guess %in% Cast)
  return(Correct)
}

runs <- 999999 + 1

Q <- pbreplicate(runs, Scream(5, 3))

which(Q>=2) %>% length %>% divide_by(length(Q)) %>% as.percent(2)
table(Q)

rm(runs)
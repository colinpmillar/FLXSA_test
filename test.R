install.packages("iterators", repos = "https://cloud.r-project.org/")
install.packages("FLXSA", repos = "http://flr-project.org/R")

test <- function(stk, idxs, ctrl, what = c(".Call", "FLXSA"), n = 20) {
  what <- match.arg(what)
  FUN <-
    switch(what,
      .Call = function() .Call("runFLXSA", stk, idxs, ctrl, TRUE),
      FLXSA = function() FLXSA(stk, idxs, ctrl, diag = TRUE)
    )

  res <-
    list(try(FUN()))
  for (i in 2:n) {
    res <-
      c(
        res,
        try(FUN())
      )
  }
  res
}

library(FLXSA)
data(ple4)
data(ple4.indices)
control <- FLXSA.control()
# fails at the start and less often as time goes on... ?
ok1 <- test(ple4, ple4.indices[1], control, "FLXSA", n = 100)

which(!ok1)
# fails every time
ok2 <- test(ple4, ple4.indices, control, ".Call")
all(!ok2)
sapply(ok2, is.FLXSA)

if (
  any(sapply(ok1, is.FLXSA) ||
  any(sapply(ok2, is.FLXSA)) {
  stop("error in FLXSA") 
}

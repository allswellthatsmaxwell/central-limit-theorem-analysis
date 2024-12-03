library(ggplot2)
library(magrittr)
library(tibble)

FACES <- 1:6

rollDice <- function(n) {
  round(runif(n, 1, 6))
}

rollBiasedDie <- function(targetFace, targetFaceProba) {
  dist <- getModifiedFaceDistribution(targetFace, targetFaceProba)
  sample(FACES, prob=dist, size=1)
}

getModifiedFaceDistribution <- function(targetFace, targetFaceProba) {
  remainingMass <- 1 - targetFaceProba
  remainingFaces <- max(FACES) - 1
  massPerNonTargetSide <- remainingMass / remainingFaces
  rollDist <- rep(massPerNonTargetSide, max(FACES))
  rollDist[targetFace] <- targetFaceProba
  rollDist %>% {. / sum(.)}
}

rollDiceTryingToMatchPrevious <- function(n, targetFaceProba) {
  # roll k has targetFaceProba probability of matching roll k-1.
  rolls <- numeric(n)
  face <- rollDice(1)
  rolls[1] <- face
  
  for (i in 2:n) {
    face %<>% rollBiasedDie(targetFaceProba)
    rolls[i] <- face
  }
  rolls
}

getCorrespondingGaussian <- function(ys) {
  μ <- mean(ys)
  σ <- sd(ys)
  lb <- min(ys)
  ub <- max(ys)
  xs <- seq(lb, ub)
  tibble(x=xs, y=dnorm(xs, μ, σ))
}


plotDensities <- function(sums) {
  dat <- tibble(x=sums)
  comparisonDat <- getCorrespondingGaussian(sums)
  ggplot(aes(x=x), data=dat) +
    geom_line(aes(y=y), data=comparisonDat) +
    geom_density(color='red') +
    theme_minimal()
}


ndice <- 200
nrounds <- 6000
roundSums <- sapply(1:nrounds, function(i) sum(rollDice(ndice)))
plotDensities(roundSums)

pbias <- 0.95
roundSumsDependent <- sapply(
  1:nrounds, function(i) sum(rollDiceTryingToMatchPrevious(ndice, pbias)))

plotDensities(roundSumsDependent)
# hahahahahhahahahahahahahahahhahha it looks equally gaussian.
# make a metric for gaussanity. incorporate feedback from popular post.
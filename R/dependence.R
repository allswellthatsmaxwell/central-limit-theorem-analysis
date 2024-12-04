library(ggplot2)
library(magrittr)
library(tibble)
library(purrr)
library(dplyr)

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

sumNonOverlappingWindows <- function(ys, step) {
  n <- length(ys)
  nWindows <- floor(n / step)
  sapply(1:nWindows, function(i) sum(ys[(1 + (i - 1) * step) : (i * step)]))
}

plotDensities <- function(sums) {
  dat <- tibble(x=sums)
  comparisonDat <- getCorrespondingGaussian(sums)
  ggplot(aes(x=x), data=dat) +
    geom_line(aes(y=y), data=comparisonDat) +
    geom_density(color='red') +
    theme_minimal()
}

getFairSumsVariance <- function(ndice, nDiceInWindow) {
  ndice %>%
    rollDice() %>% 
    sumNonOverlappingWindows(nDiceInWindow) %>% 
    var()
}

getDependentSumsVariance <- function(ndice, nDiceInWindow, pbias) {
  ndice %>%
    rollDiceTryingToMatchPrevious(pbias) %>%
    sumNonOverlappingWindows(nDiceInWindow) %>%
    var()
}

getSumsVarianceDat <- function(fn, ndice, windowSizes) {
  windowSizes %>%
    sapply(function(winSize) {
      fn(ndice, winSize)
    }) %>%
    tibble(winSize = windowSizes, variance = .)
}



ndice <- 1e6
nDiceInWindow <- 100

## Answering Sonnet 3.6's question about how the variance scales in the independent
## vs. dependent cases.
windowSizes <- seq(100, 8000, 50)
fairSumsVariancesDat <- getSumsVarianceDat(getFairSumsVariance, ndice, windowSizes)

dependentSumsVariancesDat <- getSumsVarianceDat(
  partial(getDependentSumsVariance, pbias=pbias), ndice, windowSizes)

variancesDat <- dplyr::bind_rows(
  fairSumsVariancesDat %>% mutate(style='fair'),
  dependentSumsVariancesDat %>% mutate(style='dependent'))
variancesDat %>%
  ggplot(aes(x=winSize, y=sqrt(variance), color=style)) +
  geom_line() +
  theme_minimal()

## The Gaussian comparisons.
fairRolls <- rollDice(ndice)
fairSums <- sumNonOverlappingWindows(fairRolls, nDiceInWindow)
plotDensities(fairSums)

pbias <- 0.40
dependentRolls <- rollDiceTryingToMatchPrevious(ndice, pbias)
dependentSums <- sumNonOverlappingWindows(dependentRolls, nDiceInWindow)

plotDensities(dependentSums)
# hahahahahhahahahahahahahahahhahha it looks equally gaussian.
# make a metric for gaussanity. incorporate feedback from popular post.
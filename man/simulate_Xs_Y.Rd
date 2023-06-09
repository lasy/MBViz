% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_Xs_Y.R
\name{simulate_Xs_Y}
\alias{simulate_Xs_Y}
\title{Simulate "multiblock" data}
\usage{
simulate_Xs_Y(
  n = 200,
  p = 2:5,
  q = 4,
  nlX = 2,
  nlY = 3,
  snr = 1,
  Bxx_function = sample_B_random,
  Bxy_function = sample_B_random,
  By_function = sample_B_random,
  categorical_Y = FALSE
)
}
\arguments{
\item{n}{(an `integer`) the number of samples.}

\item{p}{(a `vector` of `integer`s) the number of variable in each block.
The number of block is specified by the length of `p`.}

\item{q}{(an `integer`) the number of response variables.}

\item{nlX}{(an `integer`) the number of latent variables for the
explanatory variables only.}

\item{nlY}{(an `integer`) the number of latent variables shared by the
explanatory and response variables.}

\item{snr}{(a strictly positive `double`) the signal to noise ratio.}

\item{Bxx_function}{(a `function`) a function specifying how the explanatory
variables depend on the X-only latent factors. `sample_B_random` is the default.}

\item{Bxy_function}{(a `function`) a function specifying how the explanatory
variables depend on the shared latent factors. `sample_B_random` is the default.}

\item{By_function}{(a `function`) a function specifying how the response
variables depend on the shared latent factors. `sample_B_random` is the default.}

\item{categorical_Y}{(a `logical`) whether the response variables are Bernoulli.}
}
\value{
a `list` with the simulated data.
}
\description{
Simulate "multiblock" data
}

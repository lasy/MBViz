% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mtbda_coef.R
\name{plot_mtbda_coef}
\alias{plot_mtbda_coef}
\title{Plots the coefficients of associations between the explanatory and response variables}
\usage{
plot_mtbda_coef(res, boot = NULL, Yvar, format = "long")
}
\arguments{
\item{res}{the output of `ade4::mbpls` (or `ade4::mbpcaiv`) (the input of `ade4::randboot`)}

\item{boot}{(optional) the output of `ade4::randboot` to display the confidence intervals}

\item{format}{"long" or "wide". a `character` specifying if the coefficients should be displayed horizontally or vertically. (default = "long")}

\item{Y_var}{(optional) a `integer` or a `character` specifying the indices
or names of selected response variables if one desires to display the
coefficients for a subset of variables.}
}
\value{
a `ggplot2` object
}
\description{
Plots the coefficients of associations between the explanatory and response variables
}

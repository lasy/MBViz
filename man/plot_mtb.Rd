% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mtb.R
\name{plot_mtb}
\alias{plot_mtb}
\title{Plot results from a `ade4::mbpls` or `ade4::mbpcaiv` output}
\usage{
plot_mtb(
  res,
  xaxis = 1,
  yaxis = 2,
  scale_axes = "fixed",
  samples_color = NULL,
  block_colors = NULL
)
}
\arguments{
\item{res}{the output of `ade4::mbpls` or `ade4::mbpcaiv`}

\item{xaxis}{an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.}

\item{yaxis}{an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.}

\item{scale_axes}{how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX o)}

\item{input_var}{a `data.frame` xxxx}
}
\value{
a `patchwork` of `ggplot2` plots.
}
\description{
Plot results from a `ade4::mbpls` or `ade4::mbpcaiv` output
}

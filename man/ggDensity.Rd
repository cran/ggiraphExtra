% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggDensity.R
\name{ggDensity}
\alias{ggDensity}
\title{Make a density plot with histogram}
\usage{
ggDensity(
  data,
  mapping,
  linecolor = "red",
  addhist = TRUE,
  use.label = TRUE,
  use.labels = TRUE
)
}
\arguments{
\item{data}{a data.frame}

\item{mapping}{Set of aesthetic mappings created by aes or aes_.}

\item{linecolor}{Color of density curve}

\item{addhist}{Whether add histogram or not}

\item{use.label}{Logical. Whether or not use column label in case of labelled data}

\item{use.labels}{Logical. Whether or not use value labels in case of labelled data}
}
\description{
Make a density plot with histogram
}
\examples{
require(ggplot2)
require(moonBook)
ggDensity(acs,aes(x=age))
ggDensity(acs,aes(x=age,color=sex,fill=sex),addhist=FALSE)
ggDensity(acs,aes(x=age,color=sex,fill=sex))
ggDensity(acs,aes(x=age,fill=sex),addhist=FALSE)
ggDensity(acs,aes(x=age,color=sex))
}

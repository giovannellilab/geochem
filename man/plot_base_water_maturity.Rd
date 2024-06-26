% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-water-maturity.R
\name{plot_base_water_maturity}
\alias{plot_base_water_maturity}
\title{Plots the base ternary diagram for water maturity}
\usage{
plot_base_water_maturity(type)
}
\arguments{
\item{type}{String indicating whether Giggenbach's or Duchi's coordinates
should be used}
}
\value{
A \code{ggplot2} object.
}
\description{
Plots the base ternary diagram for water maturity
}
\examples{
plot_base_water_maturity(type="giggenbach")
plot_base_water_maturity(type="duchi")

}
\references{
{
\insertRef{giggenbach1988_GeothermalSoluteEquilibria}{geochem}
\insertRef{duchi1995_HydrogeochemistryCampaniaRegion}{geochem}
}
}
\seealso{
\code{\link[=plot_base_gibbs]{plot_base_gibbs()}}

\code{\link[=plot_base_ll]{plot_base_ll()}}

\code{\link[=plot_base_major_anions]{plot_base_major_anions()}}

\code{\link[=plot_base_major_cations]{plot_base_major_cations()}}

\code{\link[=plot_base_piper]{plot_base_piper()}}
}

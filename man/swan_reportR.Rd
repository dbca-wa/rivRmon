% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swan_reports.R
\name{swan_reportR}
\alias{swan_reportR}
\title{Function to create all of the plots and tables for the annual Swan River
report.}
\usage{
swan_reportR(
  inpath,
  outpath,
  surface = "blue",
  bottom = "red",
  chloro = "darkgreen"
)
}
\arguments{
\item{inpath}{character filepath to premade annual report data
\code{\link{swan_WIN_report_data}}.}

\item{outpath}{character filepath to desired export location.}

\item{surface}{colour for surface plots. Can be named colour or hex format.
Defaults to "blue".}

\item{bottom}{colour for bottom plots. Can be named colour or hex format.
Defaults to "red".}

\item{chloro}{colour for integrated plots. Can be named colour or hex format.
Defaults to "darkgreen".}
}
\value{
panel plots for all metrics and a csvs of metrics for
    inclusion to tables.
}
\description{
\code{swan_reportR} produces panel plots and tables for all
    metrics.
}
\details{
This is a wrapper function that runs all of the individual functions
    to produce all of the plots and tables for the Swan River.
    Outputs will be exported to two folders created at the outpath
    location. `s_panels/` for plots and `s_tables/` for data tables.
}
\examples{
\dontrun{
swan_reportR(inpath, outpath, surface = "blue", bottom = "red",
chloro = "darkgreen")}

}
\author{
Bart Huntley, \email{bart.huntley@dbca.wa.gov.au}

For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
{the rivRmon website}
}

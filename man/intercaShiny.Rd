\name{intercaShiny}
\alias{intercaShiny}
\title{
Runs the intercaShiny application
}
\description{
The function runs a Shiny app that allows the user to upload a data file and calculate the MCA output (tables, plots and ready-made reports) through a graphical interface. The data can be loaded in .xslx or csv format and then the number of axes to be retained should be specified. The user can  produce an interpretive axis or plane, filter points based on their interpretive coordinates, and download the plots and tables in .pdf (.xlsx) format, as well as a ready report with the interpretation of MPCA results.
}
\value{shiny application object}
\usage{
intercaShiny()
}
\examples{
# --- Run the 'shiny' app
\dontrun{intercaShiny()}
}

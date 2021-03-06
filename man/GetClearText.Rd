% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RobsRUtils.R
\name{GetClearText}
\alias{GetClearText}
\title{Utility to get the clear text from an encrypted file}
\usage{
GetClearText(encrypted.pw.filename, priv.k)
}
\arguments{
\item{priv.k}{- a private encrypted key}

\item{clear.pw}{- the clear text}
}
\value{
the clear text
}
\description{
Utility to get the clear text from an encrypted file
}
\examples{
\dontrun{
Fill these in with the timestamped file names

encrypted.pw.filename<-'SSpw-DT-201607131414.bin'
clear.text<-GetClearText(encrypted.pw.filename,key.ring$priv.k)
clear.text
}

}

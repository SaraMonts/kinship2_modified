\name{ped.legend}
\alias{ped.legend}
\title{
  Plot a legend for the pedigree
}
\description{
Create a plot with the description of the symbols that appear in the pedigree.
}
\usage{
\method{ped}{legend}(x, ped_id, adopted = NULL, phen.labels = c("phen1", "phen2", "phen3", "phen4"), 
                       cex = 1, col = c(1, 1, 1, 1), symbolsize = 1, mar = c(4.1, 1, 4.1, 1), width = 10, 
                       height = 4, density = c(-1, -1, -1, -1), angle = c(45, 45, 45, 45))
}
\arguments{
\item{x}{
object of class \code{pedigree} or \code{pedigreeList} obtained from the \code{pedigree} function.
}
\item{ped_id}{
identifier number of the pedigree.
}
\item{adopted}{
vector that contains adoption information for every subject. 
Values can be: "in" (adopted inside the family), "out" (adopted outside the family) or NA (not adopted).
}
\item{phen.labels}{
vector that contains the names of the four phenotypes as character strings.
}
\item{cex}{
controls text size.  Default=1.
}
\item{col}{
color for each phenotype.  Default=c(1, 1, 1, 1).
}
\item{symbolsize}{
controls symbolsize. Default=1.
}
\item{mar}{
margin parmeters, as in the \code{par} function
}
\item{width}{
width of the legend plot. Default=10
}
\item{height}{
height of the legend plot. Default=4
}
\item{density}{
defines the density used in the symbols for every phenotype.  Default=c(-1, -1, -1, -1). 
}
\item{angle}{
defines the angle used in the symbols for every phenotype.  Default=c(45, 45, 45, 45). 
}
}




\value{
Creates a plot containing the legend of the indicated pedigree.
}
\seealso{
\code{\link{plot.pedigree}}
}
\examples{
}

\keyword{genetics}

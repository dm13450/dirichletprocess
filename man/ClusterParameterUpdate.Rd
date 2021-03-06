% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_parameter_update.R
\name{ClusterParameterUpdate}
\alias{ClusterParameterUpdate}
\title{Update the cluster parameters of the Dirichlet process.}
\usage{
ClusterParameterUpdate(dpObj)
}
\arguments{
\item{dpObj}{Dirichlet process object}
}
\value{
Dirichlet process object with update cluster parameters
}
\description{
Update the parameters of each individual cluster using all the data assigned to the particular cluster.
A sample is taken from the posterior distribution using a direct sample if the mixing distribution is conjugate or the Metropolis Hastings algorithm for non-conjugate mixtures.
}
\examples{
dp <- DirichletProcessGaussian(rnorm(10))
dp <- ClusterParameterUpdate(dp)

}

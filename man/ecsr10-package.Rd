\name{ecsr10-package}
\alias{ecsr10-package}
\alias{ecsr10}
\docType{package}
\title{Elixhauser Comorbidity Software Refined for ICD-10-CM}
\description{
The ecsr10 package provides an R implementation of the Elixhauser Comorbidity 
Software Refined for ICD10 Diagnoses workflow for analyzing patient diagnosis codes and 
calculating comorbidity indices.
}
\details{
This package processes patient diagnosis data to identify 38 specific 
comorbidity categories and calculate mortality/readmission risk indices.

Main functions:
\itemize{
  \item \code{\link{comorbidity}}: Apply comorbidity analysis to patient diagnosis data
  \item \code{\link{cmr_index}}: Calculate mortality and readmission risk indices
  \item \code{\link{build_comfmt_from_csv}}: Build comorbidity format from CSV
  \item \code{\link{build_poa_exempt_formats}}: Build POA exempt code lists
}
}
\author{
Dong Pei <peidong2009@gmail.com>
}
\keyword{package}

###--------------------------------###
###    Italian regional datasets   ###
###--------------------------------###

#' Estimates of Italian population by age, sex, and region, 2006-2014.
#'
#' The population counts refer to 1st January of each year.
#'
#' Census data for year 2011
#'
#' @format An array with dimensions "age", "sex", "region", and "year".
#' Single year age groups.
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://demo.istat.it/}.

"italy.popn1Y"


#' Estimates of Italian population by age, sex, and region, 2006-2014.
#'
#' The population counts refer to 1st January of each year.
#'
#' Census data for year 2011
#'
#' @format An array with dimensions "age", "sex", "region", and "year".
#' 5 years age groups. Last groups "100 +".
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://demo.istat.it/}.

"italy.popn5Y.100"

#' Estimates of Italian population by age, sex, and region, 2006-2014.
#'
#' The population counts refer to 1st January of each year.
#'
#' Census data for year 2011
#'
#' @format An array with dimensions "age", "sex", "region", and "year".
#' 5 years age groups. Last groups "90 +".
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://demo.istat.it/}.

"italy.popn.reg"

#' Registered births in Italy by age and region, 2006-2016.
#'
#' The \code{"age"} variable refers to the age of the mother.
#'
#'
#' All births to mothers aged less than 20 (included those to mothers aged less than
#' 15) have been included in age group \code{"15-19"}, and all births to mothers aged
#' 45 or higher (including those to mothers aged 50+) have been included in age
#' group \code{"45-49"}.
#'
#' @format An array with dimensions "region", "age" and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://demo.istat.it/}.

"italy.births.reg"

#' Registered deaths in Italy by age, sex, and region, 2006-2015.
#'
#' @format An array with dimensions "age", "sex", "region", and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://dati.istat.it/}.
#'
#' @seealso \code{\link{italy.popn.reg}}, \code{\link{italy.births.reg}},
#' \code{\link{nz.int.mig.reg}}, \code{\link{nz.ext.mig.reg}}.

"italy.deaths.reg"

#' New registrations of people coming from other italian regions
#' per region of destination, sex, large age groups, year 2006-2014.
#'
#' @format An array with dimensions "region", "sex", "age"  and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://dati.istat.it/}.

"italy.intl.imm"

#' Cancellations of people leaving from an italian region to any other
#' italian region per departing region, sex, large age groups,
#' year 2006-2014.
#'
#' @format An array with dimensions "region", "sex", "age"  and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://dati.istat.it/}.


"italy.intl.emi"

#' New registrations of people coming from a foreing country
#' per region of destination, sex, large age groups, year 2006-2014.
#'
#' @format An array with dimensions "region", "sex", "age"  and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://dati.istat.it/}.

"italy.ext.imm"

#' Cancellations of people leaving from an italian region to a foreign
#' country per departing region, sex, large age groups,
#' year 2006-2014.
#'
#' @format An array with dimensions "region", "sex", "age"  and "year"
#'
#' @source Custom tabulation from Italian Statistical Institute (ISTAT)
#' \url{http://dati.istat.it/}.

"italy.ext.emi"



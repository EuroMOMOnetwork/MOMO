#' Bank holidays or intensity of registration of deaths
#' Substitude the demanded BankHoliday file
#'
#' @param MData A data.frame or data.table with MOMO input data
#' @import data.table
#' @return Data with registration intensity (between 0 and 1) per date
#' @export
BankHolidays <- function(MData) {
  X <- data.table(MData)[!is.na(DoR), .(n = .N), keyby = DoR]
  X <- merge(data.frame(date = seq(min(MData$DoD), max(MData$DoD), by = 'day')), X, by.x = 'date', by.y = 'DoR', all.x = TRUE)
  X[is.na(X)] <- 0
  X$closed <- 0.5*(X$n == 0) + 0.5*(X$n < median(X$n))
  return(data.frame(X[, c('date', 'closed')]))
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('PACKAGE: MOMO')
  packageStartupMessage(sprintf('Version %s',packageVersion("MOMO")))
  packageStartupMessage('Developed by the EuroMOMO network (www.euromomo.eu)')
  packageStartupMessage('Update or install using install.packages("MOMO", repos="https://euromomonetwork.github.io/drat/")')
}

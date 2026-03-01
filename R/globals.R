# Declare global variables to avoid NOTE in R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "fclass",
    "geometry",
    "lengthkm",
    "p.distances",
    "sd"
  ))
}

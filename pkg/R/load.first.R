.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "armlite"))
  ver <- packageDescription("armlite", lib = mylib)$Version
  builddate <- packageDescription("armlite", lib = mylib)$Date
  cat(paste("\narmlite (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
  cat ("Working directory is", getwd(), "\n")
  options(digits = 2, papersize="letter")
  cat ("options( digits = ", getOption("digits"), ")\n")
  if(!any(search()=="package:car"))
    require(car)
  if(!any(search()=="package:foreign"))
    require(foreign) 
  if(!any(search()=="package:MASS"))
    require(MASS) 
  if(!any(search()=="package:R2WinBUGS"))
    require(R2WinBUGS)
}

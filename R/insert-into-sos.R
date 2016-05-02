## push data into an exisitng sos via templates in inst/templates
# sensor <- list(list("key:easting", 7.02),
#                list("key:northing", 52.0))

insertSensor <- function(SOS, sensor,
                         template=system.file("templates/insertSensor.xml",
                                              package = "sos4R")) {
  xmlFile <- readLines(template, -1, ok = T, warn = F)
  
  for (entr in sensor) {
    xmlFile <- locateAndReplace(xmlFile, entr[1], entr[2])
  }
  
  httpPUT(url = SOS, paste(xmlFile, collapse="\n"))
}

locateAndReplace <- function(lines, key, value) {
  occs <- gregexpr(key, lines)
  keyLineIds <- which(sapply(occs, function(x) x[1] >= 0))
  
  for (line in keyLineIds) {
    lines[line] <- gsub(key, value, lines[line])
  }
  
  return(lines)
}
devtools::install_github("sherrillmix/taxonomizr")
library(taxonomizr)
dbfile <- '/Users/simplexdna/Desktop/accessionTaxa.sql'
prepareDatabase(dbfile, getAccessions=FALSE)
getDescendants(33208, sqlFile = dbfile, desiredTaxa = "phylum")

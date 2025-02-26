devtools::install_github("sherrillmix/taxonomizr")
library(taxonomizr)
dbfile <- 'accessionTaxa.sql'
prepareDatabase(dbfile, getAccessions=FALSE)
getDescendants(33208, sqlFile = dbfile, desiredTaxa = "phylum")

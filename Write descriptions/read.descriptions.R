## Read a table of species descriptions from a file
## This is a shortcut function, it is (marginally) more convenient to use this than 'read.csv'

read.descriptions = function(f, ...){
    d = read.csv(f, colClasses="character", check.names=F, ...)
    d
}

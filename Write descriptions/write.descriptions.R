## Save species descriptions to a file
## This is a shortcut function, it is (marginally) more convenient to use this than 'write.table'

write.descriptions = function(txt, file="Species descriptions written by R.txt", ...){
        write.table(txt, file=file, quote=F, sep="", row.names=F, col.names=F, ...)
}

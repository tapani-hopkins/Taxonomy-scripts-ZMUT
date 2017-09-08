## Set the working directory (either this directory or any file in it)
wd = "/Users/tapani/T/Courses/2017/PhD research 2017/Uganda Rhyssinae 2017/Rhyssinae key/Rhyssinae key 170908"
if (file.exists(wd) && !dir.exists(wd)){ wd = dirname(wd) }
if (!dir.exists(wd)){ wd = file.choose(); if (!dir.exists(wd)) { wd = dirname(wd) } }
setwd(wd)

# Import the required functions
library(vegan)  # for doing PCO, otherwise optional
source("Morphological distances/standardise.r")
source("Morphological distances/compare.r")
source("Morphological distances/dist.mixed.r")
source("Morphological distances/plot.dist.r")
source("Morphological distances/plot.key.r")
source("Morphological distances/read.taxa.r")   # optional, for easier reading of the data

## Read the table of diagnostic characters, with rows for characters and columns for individuals
a = read.taxa("Uganda Rhyssinae diagnosis characters 170908.csv")
weights = a$weights
traits = a$traits
# Convert the character table to rows for individuals and columns for characters
aa = data.frame(t(traits))

## Read the table detailing the structure of the identification key
st = read.csv("Uganda Rhyssinae key structure 170908.csv")


## Calculate distances between the species, and visualise by plotting with pco

# Calculate distances between the Rhyssinae species
d = dist.mixed(aa, weights)

# PCO the distances into two dimensions
mds = metaMDS(d)

# Plot the distances and show the identification key
p = plot.dist(mds)
k = plot.key(p, st)


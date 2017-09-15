# Taxonomy-scripts-ZMUT
Taxonomy scripts used at the Zoological Museum of the University of Turku (Finland). These are used to process taxonomical data, and thus speed up the process of writing a taxonomical article.

## Licence in a nutshell
Use any way you want, but please attribute.

## Contents include:

#### Morphological distances
R functions used to calculate how morphologically similar different specimens are, and to visualise the result. 

Function *dist.mixed* produces the morphological distances when given a table of character traits. It uses helper functions *compare* and *standardise*. Function *plot.dist* plots the (PCO analysed) distances in two dimensions. Function *plot.key* adds a visualisation of an identification key to the plot. Function *read.taxa* is a convenient way of reading the files with character traits into R. 

Morphological distances can be used to group specimens into species. Or more commonly, to visualise the differences between species when making an identification key. Identification keys currently have to be made manually, but automatic generation of a key (based on species distances) will eventually become possible. 

#### Write descriptions
R functions that take a table of character traits and convert it into species descriptions. The result can be pasted directly into a taxonomy article.

#### Uganda Rhyssinae diagnosis characters 170915
Test data. These are character traits of Afrotropical Rhyssinae, with the species in columns and characters in rows. There are several extra columns used for giving weights to characters, or when automatically generating species descriptions. (NB! This data is genuine but very preliminary, do not place any taxonomical trust in it)

#### Uganda Rhyssinae key structure 170915 / Uganda Rhyssinae key structure shorthand
Test data. This is the structure of a preliminary identification key for the above Afrotropical Rhyssinae. It is in both longhand and shorthand formats: both are treated the same by the scripts.

#### Visualise morphological differences 170915.R
Test script. This takes the above diagnosis characters and key structure, visualises the differences between species by plotting in two dimensions, and adds the identification key to the plot. The further apart the species are, the more different they are in appearance.

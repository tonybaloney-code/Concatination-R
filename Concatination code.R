library("phylotools")
library("tidyverse")

##Code Testing

vac_dnapol<- read.fasta(file.choose())
vac_rnapol <- read.fasta(file.choose())
## sequences imported as strings

##Have to clean sequence names
vac1<- separate(vac_dnapol, "seq.name",
                              into = "name",
                              sep = (".CDS."),
                              extra = "drop")%>%
  separate(., "name",
           into = "accession",
           sep = (".peg."),
           extra = "drop")%>%
  mutate(., name = "vac")%>%
  unite(., "name", accession, name, sep = "")


##concatenation
vac1 <- namecleaner(vac_dnapol, "vaccinia")


dat2fasta() # exports sequence into fasta file

##Finalize##
namecleaner <- function(sequence, species) {
  separate(sequence, "seq.name",
           into = "name",
           sep = (".CDS."),
           extra = "drop")%>%
    separate(., "name",
             into = "accession",
             sep = (".peg."),
             extra = "drop")%>%
    mutate(., name = species)%>%
    unite(., "name", accession, name, sep = "")
}
Concfasta <- function(sequence_list, species)
library("phylotools")
library("tidyverse")

prot1 <- read.fasta(file.choose())
prot2 <- read.fasta(file.choose())
prot3 <- read.fasta(file.choose())
species = "vaccinia" #Type in species here
concfasta <- function(prot1, prot2, prot3, species){
  namecleaner <- function(sequence, species) {
    separate(sequence, "seq.name",
             into = "seq.name",
             sep = (".CDS."),
             extra = "drop")%>%
      separate(., "seq.name",
               into = "accession",
               sep = (".peg."),
               extra = "drop")%>%
      mutate(., name = species)%>%
      unite(., "seq.name", accession, name, sep = "")
  }
  prot1 <- namecleaner(prot1, species)
  prot2 <- namecleaner(prot2, species)
  prot3 <- namecleaner(prot3, species)
  conc <-  left_join(prot2,prot1, by ="seq.name")%>%
    left_join(., prot3, by = "seq.name")%>%
    unite(., "seq.text", seq.text, seq.text.x, seq.text.y, sep = "")
  dat2fasta(conc)
}
concfasta(prot1,prot2,prot3, species)


##Code Testing

prot1 <- read.fasta(file.choose())
prot2 <- read.fasta(file.choose())
prot3 <- read.fasta(file.choose())
species = "vaccinia" #Type in species here
## sequences imported as strings

##Name cleaning testing
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

#Concatenation testing
prot1 <- namecleaner(prot1, species)
prot2 <- namecleaner(prot2, species)
prot3 <- namecleaner(prot3, species)
conc <-  left_join(prot2,prot1, by ="seq.name")%>%
  left_join(., prot3, by ="seq.name")%>%
  unite(conc, "seq.text", seq.text, seq.text.x, seq.text.y, sep = "")

dat2fasta() # exports sequence into fasta file

##Finalize##
namecleaner <- function(sequence, species) {
  separate(sequence, "seq.name",
           into = "seq.name",
           sep = (".CDS."),
           extra = "drop")%>%
    separate(., "seq.name",
             into = "accession",
             sep = (".peg."),
             extra = "drop")%>%
    mutate(., name = species)%>%
    unite(., "seq.name", accession, name, sep = "")
}
concfasta <- function(prot1, prot2, prot3, species){
  namecleaner <- function(sequence, species) {
    separate(sequence, "seq.name",
             into = "seq.name",
             sep = (".CDS."),
             extra = "drop")%>%
      separate(., "seq.name",
               into = "accession",
               sep = (".peg."),
               extra = "drop")%>%
      mutate(., name = species)%>%
      unite(., "seq.name", accession, name, sep = "")
  }
  prot1 <- namecleaner(prot1, species)
  prot2 <- namecleaner(prot2, species)
  prot3 <- namecleaner(prot3, species)
  conc <-  left_join(prot2,prot1, by ="seq.name")%>%
    left_join(., prot3, by = "seq.name")%>%
    unite(., "seq.text", seq.text, seq.text.x, seq.text.y, sep = "")
  dat2fasta(conc)
}
concfasta(prot1,prot2,prot3, species)

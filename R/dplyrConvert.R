
dplyrConvert <- function(){

  data(varambally,package = "prostateCancerVarambally")
  library(tidyr)
  library(dplyr)
  geoData <- varambally
  
  varambally <- tbl_df(data.frame(Probe=featureNames(geoData),exprs(geoData))) %>%
    gather(Sample,Expression,- Probe)
  
  fd <- tbl_df(fData(geoData)) %>% select(ID,contains("Symbol"),ENTREZ_GENE_ID) %>%
    mutate(Probe=ID) %>%
    select(-ID)
  colnames(fd)[1] <- "Gene"
  pd <- tbl_df(pData(geoData)) %>% select(geo_accession, title)
  
  Sample_Group <- NULL
  
  Sample_Group[grep("Benign", pd$title)] <- "Benign"
  
  Sample_Group[grep("prostate cancer",pd$title)] <- "PC"
  
  Sample_Group[grep("Metastatic", pd$title)] <- "Metastatic"
  
  pd <- mutate(pd, Sample_Group = Sample_Group)  %>%
    mutate(Sample = geo_accession) %>%
    select(Sample_Group, Sample)
  
  varambally <- varambally %>% full_join(pd) %>% full_join(fd)
  varambally
}

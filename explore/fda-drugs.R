# dir.create("./explore")
source("functions.R")

# Packages =====

# install.packages("pdftools")

library("pdftools")
library("stringr")
library("tidyverse")

# FDA-approved drugs =====
# List of FDA approved products sourced from the Orange Book from the FDA
# https://www.fda.gov/Drugs/InformationOnDrugs/ucm129662.htm
# Appendix A: Product Name Index 

orange.raw <- pdf_text("./explore/orange-book-appx-a.pdf")
  
# extracting brand names and drug names
full.name <- lapply(orange.raw, 
       str_split, 
       pattern = "\n") %>%
  unlist()
full.name <- full.name[!str_detect(full.name, "DECEMBER 2018|APPENDIX A|\\*\\*")]

# extracting only drug names
drug.name <- str_extract(
  full.name,
  "\\,[[:space:]]([[:alpha:]]|[[:space:]])+$"
) %>%
  str_remove("^\\,[[:space:]]") %>%
  str_remove("\r$")
drug.name <- drug.name[!duplicated(drug.name)] %>% tolower()

# In total: 1401 unique drugs

# Downloading ====
# 1401 / 5 groups = 4 groups of 280 and one of 281

# dir.create("./explore/fda-molecules")

# Sets
# Paths for downloading
set.paths <- paste0("./explore/fda-molecules/set", 1:5, "/")
# lapply(set.paths, dir.create)

# Find files with size = 0 (failed to download)
find.empties <- function(path) {
  
  guests <- list.files(path) %>%
    str_remove(".SDF")
  sdf.sizes <- list.files(path, full.names = T) %>% 
    file.size() 
  return(guests[sdf.sizes == 0])
  
}

# Remove files with size = 0 (failed to download)
remove.empties <- function(path) {
  
  sdf.sizes <- list.files(path, full.names = T) %>%
    file.size()
  file.remove(list.files(path, full.names = T)[sdf.sizes == 0])
    
}

# In the event that "all connections are in use", restart R and pick up 
  # 1
fda1 <- drug.name[1:280]
lapply(fda1, 
       download.cactus.file, 
       path = set.paths[1])
# fda.failed <- find.empties(set.paths[1])
# remove.empties(set.paths[1])

  # 2
fda2 <- drug.name[281:560]
lapply(fda2, 
       download.cactus.file, 
       path = set.paths[2])

  # 3
fda3 <- drug.name[561:840]
lapply(fda3, 
       download.cactus.file, 
       path = set.paths[3])


  # 4
fda4 <- drug.name[841:1120]
lapply(fda4, 
       download.cactus.file, 
       path = set.paths[4])

  # 5
fda5 <- drug.name[1121:1401]
lapply(fda5, 
       download.cactus.file, 
       path = set.paths[5])

# Empties
# A list of all failed downloads
# 20.34% failed, 285/1401
fda.failed <- lapply(set.paths, find.empties) %>% unlist()
saveRDS(fda.failed, "./explore/failed-dwnld.RDS")

# removing empty files
lapply(set.paths, remove.empties)
# Will replace later
# replaced term is commented 
# insulin is a common one but because it's a protein, I'll skip it
# ditto for estrogens (conjugated)
fda.replacements <- c(
  "alendronate", # "" sodium
  "aripiprazole", 
  "atropine", # atropine sulfate
  "acetic acid" # glacial
)

# Descriptors =====

# Ran PaDEL

# I created two more folders - large molecules and small salts - that can't be
# run on PaDEL-Descriptor
# Here's a list of the "problem" molecules. 
large.molecules <- list.files("./explore/fda-molecules/large-molecules") %>%
  str_remove_all(".SDF")
small.salts <- list.files("./explore/fda-molecules/small-salts") %>%
  str_remove_all(".SDF")
saveRDS(large.molecules, "./explore/large-molecules.RDS")
saveRDS(small.salts, "./explore/small-salts.RDS")

fda.desc <- lapply(list.files("./explore/fda-desc", full.names = T), 
                   read.csv, 
                   header = T)

# QSAR/Ensemble prediction =====

# 1031 molecules
# 1031/1401 = 73.5% of cleaned molecules

# Alpha-CD
fda.pp.alpha <- do.call(
  rbind, 
  lapply(fda.desc, pp.desc, 
                       alpha.vars, alpha.pp, alpha.fill)) 
fda.pp.alpha <- dplyr::rename(fda.pp.alpha, guest = guests)
fda.ad.alpha <- domain.num(fda.pp.alpha) %>% 
  select(., guest, newSk, domain)
fda.qsar.alpha <- ensemble(fda.pp.alpha, alpha.models) %>%
  select(., guest, ensemble)

fda.alpha <- full_join(fda.qsar.alpha, fda.ad.alpha, by = "guest")

# Beta-CD
fda.pp.beta <- do.call(
  rbind, 
  lapply(fda.desc, pp.desc, 
         beta.vars, beta.pp, beta.fill)) 
fda.pp.beta <- dplyr::rename(fda.pp.beta, guest = guests)
fda.ad.beta <- domain.num(fda.pp.beta) %>% 
  select(., guest, newSk, domain)
fda.qsar.beta <- ensemble(fda.pp.beta, beta.models) %>%
  select(., guest, ensemble)

fda.beta <- full_join(fda.qsar.beta, fda.ad.beta, by = "guest")

# All combinations
fda <- rbind(
  mutate(fda.alpha, cd = "Alpha"), 
  mutate(fda.beta, cd = "Beta")
)
saveRDS(fda, "./explore/fda-pred.RDS")

# 638 are within the applicability domain
# 638/1401 = 45.5%
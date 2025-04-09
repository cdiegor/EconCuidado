
library(PNADcIBGE)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(survey)
library(readr)

# Diretorio
setwd("C:/Projetos/PNAD")

# Abrindo os dados
pnadc <- (readRDS(file = "PNADC_022024"))

pnads <- subset(pnadc, pnadc$V2009 >= 14 & pnadc$V2009 <= 17)
nrow(pnads)

pnads <- subset(pnadc, pnadc$V2009 >= 14 & pnadc$V2009 <= 17 & pnadc$UF == "Ceará")
nrow(pnads)

pnads <- subset(pnadc, pnadc$V2009 >= 14 & pnadc$V2009 <= 17 & pnadc$UF == "Ceará" & pnadc$V4012 == "Trabalhador doméstico")
nrow(pnads)


pnads <- subset(pnadc, pnadc$V4012 == "Trabalhador doméstico")
nrow(pnads)
print(unique(pnads$V4010))

nrow(subset(pnads, V4010 == 8350))


# Fazer uma vez para gerar o arquivo compilado

arquivo_pnad_2023T01 <- leitura("PNADC_012022.txt")
arquivo_pnad_2023T02 <- leitura("PNADC_022022.txt")
arquivo_pnad_2023T03 <- leitura("PNADC_032022.txt")
arquivo_pnad_2023T04 <- leitura("PNADC_042022.txt")
arquivo_pnad_2023T01 <- leitura("PNADC_012023.txt")
arquivo_pnad_2023T02 <- leitura("PNADC_022023.txt")
arquivo_pnad_2023T03 <- leitura("PNADC_032023.txt")
arquivo_pnad_2023T04 <- leitura("PNADC_042023.txt")


# Fazer a cada vez que quiser gerar o resultado

pnads <- c("PNADC_012022", "PNADC_022022", "PNADC_032022", "PNADC_042022",
           "PNADC_012023", "PNADC_022023", "PNADC_032023", "PNADC_042023")

todas_as_taxas(pnads)



# Fazer uma vez para gerar o arquivo compilado

arquivo_pnad_2018T01 <- leitura("PNADC_012018.txt")
arquivo_pnad_2018T02 <- leitura("PNADC_022018.txt")
arquivo_pnad_2018T03 <- leitura("PNADC_032018.txt")
arquivo_pnad_2018T04 <- leitura("PNADC_042018.txt")
arquivo_pnad_2019T01 <- leitura("PNADC_012019.txt")
arquivo_pnad_2019T02 <- leitura("PNADC_022019.txt")
arquivo_pnad_2019T03 <- leitura("PNADC_032019.txt")
arquivo_pnad_2019T04 <- leitura("PNADC_042019.txt")
arquivo_pnad_2020T01 <- leitura("PNADC_012020.txt")
arquivo_pnad_2020T02 <- leitura("PNADC_022020.txt")
arquivo_pnad_2020T03 <- leitura("PNADC_032020.txt")
arquivo_pnad_2020T04 <- leitura("PNADC_042020.txt")
arquivo_pnad_2021T01 <- leitura("PNADC_012021.txt")
arquivo_pnad_2021T02 <- leitura("PNADC_022021.txt")
arquivo_pnad_2021T03 <- leitura("PNADC_032021.txt")
arquivo_pnad_2021T04 <- leitura("PNADC_042021.txt")
arquivo_pnad_2022T01 <- leitura("PNADC_012022.txt")
arquivo_pnad_2022T02 <- leitura("PNADC_022022.txt")
arquivo_pnad_2022T03 <- leitura("PNADC_032022.txt")
arquivo_pnad_2022T04 <- leitura("PNADC_042022.txt")
arquivo_pnad_2023T01 <- leitura("PNADC_012023.txt")
arquivo_pnad_2023T02 <- leitura("PNADC_022023.txt")
arquivo_pnad_2023T03 <- leitura("PNADC_032023.txt")
arquivo_pnad_2023T04 <- leitura("PNADC_042023.txt")
arquivo_pnad_2024T01 <- leitura("PNADC_012024.txt")




# Fazer a cada vez que quiser gerar o resultado

pnads <- c("PNADC_012018", "PNADC_022018", "PNADC_032018", "PNADC_042018",
           "PNADC_012019", "PNADC_022019", "PNADC_032019", "PNADC_042019",
           "PNADC_012020", "PNADC_022020", "PNADC_032020", "PNADC_042020",
           "PNADC_012021", "PNADC_022021", "PNADC_032021", "PNADC_042021",
           "PNADC_012022", "PNADC_022022", "PNADC_032022", "PNADC_042022",
           "PNADC_012023", "PNADC_022023", "PNADC_032023", "PNADC_042023",
           "PNADC_012024")

pnads <- c("PNADC_012024")

todas_as_funcoes(pnads)

todas_as_atividades(pnads)

todas_as_posicoes(pnads)

todos_fora_forca(pnads)

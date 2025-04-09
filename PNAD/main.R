




library(PNADcIBGE)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(survey)
library(readr)

library(doParallel)
library(foreach)
library(itertools)
library(parallel)

#library(writexl)
library(openxlsx)
library(purrr)

#########################################################
# Definir diretório e carregar dependências internas
setwd("C:/Projetos/PNAD")
source("GerarDadosPNAD_Multi.R")
source("LeituraPNAD_Multi.R")
options(OutDec= ",")
#########################################################



#########################################################
# Escolher PNADs para trabalhar
# Cada linha fica em um arquivo separado.
pnads <- list()
pnads[[1]] <- c("PNADC_012016", "PNADC_022016", "PNADC_032016", "PNADC_042016")
pnads[[2]] <- c("PNADC_012017", "PNADC_022017", "PNADC_032017", "PNADC_042017")
pnads[[3]] <- c("PNADC_012018", "PNADC_022018", "PNADC_032018", "PNADC_042018")
pnads[[4]] <- c("PNADC_012019", "PNADC_022019", "PNADC_032019", "PNADC_042019")
pnads[[5]] <- c("PNADC_012020", "PNADC_022020", "PNADC_032020", "PNADC_042020")
pnads[[6]] <- c("PNADC_012021", "PNADC_022021", "PNADC_032021", "PNADC_042021")
pnads[[7]] <- c("PNADC_012022", "PNADC_022022", "PNADC_032022", "PNADC_042022")
pnads[[8]] <- c("PNADC_012023", "PNADC_022023", "PNADC_032023", "PNADC_042023")
pnads[[9]] <- c("PNADC_012024", "PNADC_022024", "PNADC_032024", "PNADC_042024")
#########################################################



#########################################################
# Registrar processadores para trabalho em paralelo
Cores <- makeCluster(10, outfile = "debug.txt")
registerDoParallel(Cores)
#########################################################


#########################################################
# Determinar anos que deseja gerar
ano_inicial = 2016
ano_final = 2024
#########################################################


#########################################################
# Combinar arquivos em xls                              
# Função                                                
combinar_csv <- function(padrao_csv, saida_xls)
{
  list.files(pattern = padrao_csv) %>% 
    map(., ~read_csv2(.)) %>% 
    write.xlsx(., saida_xls)
}
#########################################################





#########################################################
# Geração dos resultados                                #
# Escolher apenas um por vez                            #



#########################################################

# Gerar planilha base apenas com categorias
rosto("Planilha base", "resultado_planilha_base_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
  {
    for (pnad in pnads[ano-ano_inicial+1])
    {
      gerar_planilha_base(pnad, paste("resultado_planilha_base_", ano, ".csv"))
    }
  }
combinar_csv("^resultado_planilha_base_", "agregado_resultado_planilha_base_2016_2024.xlsx")



# Gerar resultado por posicao na ocupação
rosto("Posição na ocupação", "_resultado_posicao_na_ocupacao_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
{
  for (pnad in pnads[ano-ano_inicial+1])
  {
    if (ano < 2020)
      gerar_posicao_na_ocupacao(pnad, paste("_resultado_posicao_na_ocupacao_", ano, ".csv"))
    else
      gerar_posicao_na_ocupacao(pnad, paste("resultado_posicao_na_ocupacao_", ano, ".csv"))
  }
}
combinar_csv("^_resultado_posicao_na_ocupacao_", "agregado_resultado_posicao_na_ocupacao_2016_2019.xlsx")
combinar_csv("^resultado_posicao_na_ocupacao_", "agregado_resultado_posicao_na_ocupacao_2020_2024.xlsx")




# Gerar resultado por grupamento de atividades
rosto("Grupamento de atividades", "_resultado_atividades_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
{
  for (pnad in pnads[ano-ano_inicial+1])
  {
    if (ano < 2020)
      gerar_atividades(pnad, paste("_resultado_atividades_", ano, ".csv"))
    else
      gerar_atividades(pnad, paste("resultado_atividades_", ano, ".csv"))
  }
}
combinar_csv("^_resultado_atividades_", "agregado_resultado_atividades_2016_2019.xlsx")
combinar_csv("^resultado_atividades_", "agregado_resultado_atividades_2020_2024.xlsx")




# Gerar resultado para trabalhadores domésticos
rosto("Trabalhadores domésticos", "resultado_trabalhadores_domesticos_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
{
  for (pnad in pnads[ano-ano_inicial+1])
  {
    gerar_trabalhadores_domesticos(pnad, paste("resultado_trabalhadores_domesticos_", ano, ".csv"))
  }
}
combinar_csv("^resultado_trabalhadores_domesticos_", "agregado_resultado_domesticos_2016_2024.xlsx")


# Gerar trabalhadores cuidadores
rosto("Trabalhadores cuidadores", "_resultado_trabalhadores_cuidadores_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
{
  for (pnad in pnads[ano-ano_inicial+1])
  {
    if (ano < 2020)
      gerar_trabalhadores_cuidadores2(pnad, paste("_resultado_trabalhadores_cuidadores_", ano, ".csv"))
    else
      gerar_trabalhadores_cuidadores2(pnad, paste("resultado_trabalhadores_cuidadores_", ano, ".csv"))
  }
}
combinar_csv("^_resultado_trabalhadores_cuidadores_", "agregado_resultado_cuidadores_2016_2019.xlsx")
combinar_csv("^resultado_trabalhadores_cuidadores_", "agregado_resultado_cuidadores_2020_2024.xlsx")


# Gerar trabalhadores desocupados
rosto("Trabalhadores desocupados", "resultado_trabalhadores_desocupados_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
{
  for (pnad in pnads[ano-ano_inicial+1])
  {
    gerar_desocupados(pnad, paste("resultado_trabalhadores_desocupados_", ano, ".csv"))
  }
}
combinar_csv("^resultado_trabalhadores_desocupados_", "agregado_resultado_desocupados_2016_2024.xlsx")


# Gerar trabalhadores fora da força
rosto("Fora da força de trabalho", "resultado_fora_da_forca_ 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
  {
    for (pnad in pnads[ano-ano_inicial+1])
    {
      gerar_fora_da_forca(pnad, paste("resultado_fora_da_forca_", ano, ".csv"))
    }
  }
combinar_csv("^resultado_fora_da_forca_", "agregado_resultado_fora_da_forca_2016_2024.xlsx")



# Gerar resultado por posicao na ocupação
rosto("Posição na ocupação X Grupamento de atividades", "_resultado_pno_cnae 0.csv")
foreach(ano=2016:2024,  .packages=c('dplyr', 'tidyr')) %dopar%
  {
    for (pnad in pnads[ano-ano_inicial+1])
    {
      if (ano < 2020)
        gerar_posicao_na_ocupacao(pnad, paste("_resultado_pno_cnae", ano, ".csv"))
      else
        gerar_posicao_na_ocupacao(pnad, paste("resultado_pno_cnae", ano, ".csv"))
    }
  }
combinar_csv("^_resultado_pno_cnae_", "agregado_resultado_pno_cnae_2016_2019.xlsx")
combinar_csv("^resultado_pno_cnae_", "agregado_resultado_pno_cnae_2020_2024.xlsx")



# Parar processamento em paralelo
stopCluster(Cores)


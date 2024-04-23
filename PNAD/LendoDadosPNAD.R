library(PNADcIBGE)
library(dplyr)
library(survey)
library(readr)


# Leitura e salvamento dos dados
pnadc2023T04 <- read_pnadc("PNADC_042023.txt", "input_PNADC_trimestral.txt")
pnadc2023T04 <- pnadc_labeller(data_pnadc=pnadc2023T04, "dicionario_PNADC_microdados_trimestral.xls")

# Salvando um objeto RDS
saveRDS(pnadc2023T04,"pnadc2023T04")







#svymean(pnadc$rendatrabtotal, design = spnadc, na.rm=TRUE)

#svyby(pnadc$rendatrabtotal, pnadc$sexo, design = spnadc, svymean, na.rm=TRUE)

#svyby(pnadc$rendatrabtotal, pnadc$UFN, design = spnadc, svymean, 
#      na.rm = TRUE, keep.names =  TRUE)

#svyby(pnadc$rendatrabtotal, ~pnadc$UFN+pnadc$sexo, design = spnadc,
#        svymean , na.rm = TRUE,  keep.names =  FALSE )

#svytable(pnadc$ocupadas, 
#         design = subset(spnadc, pnadc$forca == "Pessoas na força de trabalho"))

taxa_desemprego <- function(local_, sexo_, faixa_etaria_, raca_, contribuicao_, posicao_, atividade_)
{
  # Idade mínima
  pnsub <- subset(pnadc,  pnadc$idade >=14)
  
  # Local (estado ou Brasil)
  pnsub <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
  
  # Sexo (homem ou mulher)
  pnsub <- subset(pnsub,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
  
  # Faixa etária
  pnsub <- subset(pnsub,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
  
  if (raca_ == "Negra")
  {
    pnsub <- subset(pnsub,  ( (raca == "Preta") | (raca == "Parda" ) ) )
  }
  else if (raca_ == "Não Negra")
  {
    pnsub <- subset(pnsub,  ( (raca != "Preta") & (raca != "Parda" ) ) )
  }
  else
  {
    pnsub <- subset(pnsub,  ( (raca_ == "") | (raca == raca_ ) ) )
  }

  # Contribuição previdenciária    
  pnsub <- subset(pnsub,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
  
  # Posição da ocupação
  pnsub <- subset(pnsub,  ( (posicao_ == "") | (posicao == posicao_ ) ) )
  
  # Setor econômico da atividade
  pnsub <- subset(pnsub,  ( (atividade_ == "") | (atividade == atividade_ ) ) )
  
  total_lsir <- sum(pnsub$V1028)
  #print(total_lsir)
  ocupadas_lsir <- sum(subset(pnsub, (pnsub$ocupadas=="Pessoas ocupadas") & (pnsub$forca=="Pessoas na força de trabalho") )$V1028 )  
  #print(ocupadas_lsir)
  forca_lsir <- sum(subset(pnsub,  (pnsub$forca=="Pessoas na força de trabalho") )$V1028 )
  #print(forca_lsir)
  taxa_lsir <- 100 * (1 - ocupadas_lsir/forca_lsir)
  nivel_lsir <- 100*ocupadas_lsir/total_lsir
  participacao_lsir <- 100*forca_lsir/total_lsir
  
  return (paste(total_lsir, ";", ocupadas_lsir, ";", forca_lsir, ";", taxa_lsir, ";", nivel_lsir, ";", participacao_lsir))
}

#print(taxa_desemprego("", "", "", ""))

renda <- function(local_, sexo_, faixa_etaria_, raca_,  contribuicao_, posicao_, atividade_)
{
  # Idade mínima
  pnsub <- subset(pnadc,  pnadc$idade >=14)
  
  # Local (estado ou Brasil)
  pnsub <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
  
  # Sexo (homem ou mulher)
  pnsub <- subset(pnsub,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
  
  # Faixa etária
  pnsub <- subset(pnsub,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
  
  if (raca_ == "Negra")
  {
    pnsub <- subset(pnsub,  ( (raca == "Preta") | (raca == "Parda" ) ) )
  }
  else if (raca_ == "Não Negra")
  {
    pnsub <- subset(pnsub,  ( (raca != "Preta") & (raca != "Parda" ) ) )
  }
  else
  {
    pnsub <- subset(pnsub,  ( (raca_ == "") | (raca == raca_ ) ) )
  }
  
  # Contribuição previdenciária    
  pnsub <- subset(pnsub,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
  
  # Posição da ocupação
  pnsub <- subset(pnsub,  ( (posicao_ == "") | (posicao == posicao_ ) ) )
  
  # Setor econômico da atividade
  pnsub <- subset(pnsub,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
  total_lsir <- nrow(subset(pnsub))
  renda_media <- weighted.mean(pnsub$rendatrabtotal, w = pnsub$V1028, na.rm = TRUE)
  
  return (renda_media)
  
}

print(renda("Ceará", "", "", "", "", "Diretores e gerentes", ""))

print(renda("Ceará", "", "", "", "Não contribuinte", "Trabalhador doméstico", ""))

todas_as_taxas <- function()
{
  todos_locais = c("Ceará", "Bahia", "Pernambuco", "Rio Grande do Norte", "")
  todos_sexos = c("Homem", "Mulher", "")
  todas_idades = c("Jovem", "Pleno", "Senior", "Idoso", "")
  todas_racas = c("Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não negra", "")
  todas_contribuicoes = c("Contribuinte", "Não contribuinte", "")
  todas_posicoes = c("Diretores e gerentes", 
                     "Profissionais das ciências e intelectuais", 
                     "Técnicos e profissionais de nível médio", 
                     "Trabalhadores de apoio administrativo", 
                     "Trabalhadores dos serviços, vendedores dos comércios e mercados", 
                     "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca", 
                     "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios", 
                     "Operadores de instalações e máquinas e montadores", 
                     "Ocupações elementares",
                     "Membros das forças armadas, policiais e bombeiros militares",
                     "")

  todas_atividades = c("Agricultura, pecuária, produção florestal, pesca e aquicultura",
                       "Indústria geral",
                       "Indústrias de transformação",
                       "Construção",
                       "Comércio, reparação de veículos automotores e motocicletas",
                       "Transporte, armazenagem e correio",
                       "Alojamento e alimentação",
                       "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                       "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                       "Serviços domésticos",
                       "Outros serviços",
                       "")

    
  cat ( paste ("Local;", "Sexo;", "Faixa etária;", "Raça;", "Previdência;", "Posição;", "Atividade;",
               "Força de trabalho;", "Pessoas ocupadas;", "Taxa de desemprego;", "Nível da ocupação;", "Participação;", "Renda habitual\n"),
        file = "resultados.csv",
        append = FALSE )
  posicao = ""
  atividade = "Serviços domésticos"
  for (local in todos_locais)
    for (sexo in todos_sexos)
      for (faixa in todas_idades)
        for (raca in todas_racas)
          for (contribuicao in todas_contribuicoes)
            #for (posicao in todas_posicoes)
            #  for (atividade in todas_atividades)
        {
          
          cat( paste (local, ";", sexo, ";", faixa, ";", raca, ";", contribuicao, ";", posicao, ";", atividade, ";",
                      taxa_desemprego(local, sexo, faixa, raca, contribuicao, posicao, atividade),";", 
                      renda(local, sexo, faixa, raca, contribuicao, posicao, atividade), "\n" ),
               file = "resultados.csv",
               append = TRUE)
        }
}

todas_as_taxas()




taxas <- function(pnadc, local_, sexo_, faixa_etaria_, raca_, contribuicao_, posicao_, atividade_)
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

rendas <- function(pnadc, local_, sexo_, faixa_etaria_, raca_,  contribuicao_, posicao_, atividade_)
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

  escolar <- weighted.mean(pnsub$escolaridade, w = pnsub$V1028, na.rm = TRUE)
  
  horas <- weighted.mean(pnsub$horas, w = pnsub$V1028, na.rm = TRUE)
    
  return (paste(renda_media, ";", escolar, ";", horas))
  
}



todas_as_taxas <- function(pnads)
{
  
  todos_locais = c("Ceará", "")
  todos_sexos = c("Homem", "Mulher", "")
  #todas_idades = c("Jovem", "Pleno", "Senior", "Idoso", "")
  todas_idades = c("")
  todas_racas = c("Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra", "")
  todas_contribuicoes = c("Sim", "Não", "")
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
  
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", "Carteira assinada;", "Posição na ocupação;",
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", "Taxa de desemprego;", "Nível da ocupação;", "Participação;", 
               "Renda habitual;", "Escolaridade;", "Horas trabalhadas",  "\n"),
        file = "resultados_trabalho_domestico.csv",
        append = FALSE )
  todas_posicoes = c("")
  todas_atividades = c("Trabalhador doméstico", "")
  
  for (pnadf in pnads)
  {
    
    pnadc <- preprocessamento(pnadf)
    
    
    for (local in todos_locais)
      for (sexo in todos_sexos)
        for (faixa in todas_idades)
          for (raca in todas_racas)
            for (contribuicao in todas_contribuicoes)
              for (posicao in todas_posicoes)
                for (atividade in todas_atividades)
                {
                  
                  cat( paste (pnadf, ";", local, ";", sexo, ";", faixa, ";", raca, ";", contribuicao, ";", atividade, ";",
                              taxas(pnadc, local, sexo, faixa, raca, contribuicao, posicao, atividade),";", 
                              rendas(pnadc, local, sexo, faixa, raca, contribuicao, posicao, atividade),";",
                              "\n" ),
                       file = "resultados_trabalho_domestico.csv",
                       append = TRUE)
                }
  }
}



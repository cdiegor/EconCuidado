


taxas <- function(pnadc)
{
  total_lsir <- sum(pnadc$V1028)
  ocupadas_lsir <- sum(subset(pnadc, (pnadc$ocupadas=="Pessoas ocupadas") & (pnadc$forca=="Pessoas na força de trabalho") )$V1028 )  
  forca_lsir <- sum(subset(pnadc,  (pnadc$forca=="Pessoas na força de trabalho") )$V1028 )
  taxa_lsir <- 100 * (1 - ocupadas_lsir/forca_lsir)
  nivel_lsir <- 100*ocupadas_lsir/total_lsir
  participacao_lsir <- 100*forca_lsir/total_lsir
  
  return (paste(total_lsir, ";", ocupadas_lsir, ";", forca_lsir, ";", taxa_lsir, ";", nivel_lsir, ";", participacao_lsir))
}

rendas <- function(pnadc)
{

  total_lsir <- nrow(subset(pnadc))
  renda_media <- weighted.mean(pnadc$rendatrabtotal, w = pnadc$V1028, na.rm = TRUE)

  escolar <- weighted.mean(pnadc$escolaridade, w = pnadc$V1028, na.rm = TRUE)
  
  horas <- weighted.mean(pnadc$horas, w = pnadc$V1028, na.rm = TRUE)
    
  return (paste(renda_media, ";", escolar, ";", horas))
  
}

todas_as_atividades <- function(pnads)
{
  
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "Jovem", "Junior", "Pleno", "Senior", "Idoso")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todas_contribuicoes = c("", "Contribuinte", "Não contribuinte")
  todas_posicoes = c("",
                     "Dirigentes e gerentes", 
                     "Profissionais das ciências e intelectuais", 
                     "Técnicos e profissionais de nível médio", 
                     "Trabalhadores de apoio administrativo", 
                     "Trabalhadores dos serviços, vendedores dos comércios e mercados", 
                     "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca", 
                     "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios", 
                     "Operadores de instalações e máquinas e montadores", 
                     "Ocupações elementares",
                     "Membros das forças armadas, policiais e bombeiros militares"
                     )
  
  todas_atividades = c("",
                       "Agricultura, pecuária, produção florestal, pesca e aquicultura",
                       "Indústria",
                       "Construção",
                       "Comércio, reparação de veículos automotores e motocicletas",
                       "Transporte, armazenagem e correio",
                       "Alojamento e alimentação",
                       "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                       "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                       "Outros serviços",
                       "Serviços domésticos"
                       )
  
  #todas_atividades = c("")
  todas_posicoes = c("")
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
               "Contribuição previdenciária?;", "Posição;", "Principal atividade;", 
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual;", "Escolaridade;", "Horas trabalhadas",  "\n"),
        file = "resultados_atividades.csv",
        append = FALSE )

  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubs <- subset(pnsubl,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (raca_ in todas_racas)
          {
            if (raca_ == "Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca == "Preta") | (raca == "Parda" ) ) )
            }
            else if (raca_ == "Não Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca != "Preta") & (raca != "Parda" ) ) )
            }
            else
            {
              pnsubr <- subset(pnsubf,  ( (raca_ == "") | (raca == raca_ ) ) )
            }
            
            for (contribuicao_ in todas_contribuicoes)
            {
              # Contribuição previdenciária    
              pnsubc <- subset(pnsubr,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
              
              for (posicao_ in todas_posicoes)
              {
                # Posição da ocupação
                pnsubp <- subset(pnsubc,  ( (posicao_ == "") | (posicao == posicao_ ) ) )
                
                for (atividade_ in todas_atividades)
                {
                  # Setor econômico da atividade
                  pnsuba <- subset(pnsubp,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
                  
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                              contribuicao_, ";", posicao_, ";", atividade_, ";",
                              taxas(pnsuba),";", 
                              rendas(pnsuba),";",
                              "\n" ),
                       file = "resultados_atividades.csv",
                       append = TRUE)
                }      
              }
            }
          }
        }
      }
    }  
  }
}

todas_as_posicoes <- function(pnads)
{
  
 
}

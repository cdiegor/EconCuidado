


taxas <- function(pnsub)
{

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

rendas <- function(pnsub)
{
  
  total_lsir <- nrow(subset(pnsub))
  renda_principal <- weighted.mean(pnsub$rendatrabprincipal, w = pnsub$V1028, na.rm = TRUE)
  renda_media <- weighted.mean(pnsub$rendatrabtotal, w = pnsub$V1028, na.rm = TRUE)

  escolar <- weighted.mean(pnsub$escolaridade, w = pnsub$V1028, na.rm = TRUE)
  
  horas_principal <- weighted.mean(pnsub$horas_principal, w = pnsub$V1028, na.rm = TRUE)
  horas_total <- weighted.mean(pnsub$horas_total, w = pnsub$V1028, na.rm = TRUE)
  
    
  return (paste(renda_principal, ";", renda_media, ";", escolar, ";", horas_principal, ";", horas_total))
  
}



todas_as_taxas <- function(pnads)
{
  
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  #todas_idades = c("", "Jovem", "Pleno", "Senior", "Idoso")
  todas_idades = c("")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todas_contribuicoes = c("", "Sim", "Não")
  
  
  
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", "Carteira assinada;", "Posição na ocupação;",
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desemprego;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda habitual total;", "Escolaridade;", 
               "Horas trabalhadas principal;", "Horas trabalhadas total",  "\n"),
        file = "resultados_trabalho_domestico.csv",
        append = FALSE )
  todas_posicoes = c("")
  todas_atividades = c("Trabalhador doméstico", "")
  
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
                              contribuicao_, ";", atividade_, ";", taxas(pnsuba),";", rendas(pnsuba),";",
                              "\n" ),
                       file = "resultados_trabalho_domestico.csv",
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



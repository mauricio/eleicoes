defaultEncoding <- "UTF8"
detalheVotacaoMunicipioZonaTypes <- c("character", "character", "character", "numeric", "character", 
                                      "character", "character", "numeric", "character", "numeric", 
                                      "numeric", "character", "numeric","numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "character", 
                                      "character")

governadoresFiles <- c("governador_2010.csv", "governador_2006.csv", "governador_2002.csv")
prefeitosFiles <- c("prefeito_2012.csv", "prefeito_2008.csv", "prefeito_2004.csv")

readDetalheVotacaoMunicipioZona <- function( fileName ) {
  fileConnection = file(fileName)
  contents <- readChar(fileConnection, file.info(fileName)$size)  
  close(fileConnection)
  contents <- gsub('"', "", contents)
  
  columnNames <- c("data_geracao", "hora_geracao", "ano_eleicao", "num_turno", "descricao_eleicao", 
                   "sigla_uf", "sigla_ue", "codigo_municipio", "nome_municipio", "numero_zona", 
                   "codigo_cargo", "descricao_cargo", "qtd_aptos", "qtd_secoes", "qtd_secoes_agregadas", 
                   "qtd_aptos_tot", "qtd_secoes_tot", "qtd_comparecimento", "qtd_abstencoes", "qtd_votos_nominais", 
                   "qtd_votos_brancos", "qtd_votos_nulos", "qtd_votos_legenda", "qtd_votos_anulados", "data_ult_totalizacao", 
                   "hora_ult_totalizacao")

  csvFile <- read.csv(text=contents, 
           colClasses=detalheVotacaoMunicipioZonaTypes,
           sep=";", 
           col.names=columnNames, 
           header=FALSE)
  
  interestVariables <- c("ano_eleicao", "num_turno", "sigla_uf",
    "nome_municipio", "numero_zona", "codigo_cargo", "descricao_cargo",
    "qtd_aptos_tot", "qtd_comparecimento", "qtd_abstencoes", "qtd_votos_nominais", 
    "qtd_votos_brancos", "qtd_votos_nulos", "qtd_votos_anulados")
  
  filtered <- csvFile[interestVariables]
  
  filtered
}

readPorCargo <- function(fileName, cargo=11, turno = 1)  {
  items <- readDetalheVotacaoMunicipioZona(fileName)
  
  cargos <- subset(items, codigo_cargo == cargo & num_turno == turno, 
                      select=c("nome_municipio", "descricao_cargo", "qtd_aptos_tot", "qtd_comparecimento", "qtd_abstencoes",
                               "qtd_votos_brancos", "qtd_votos_nulos", "qtd_votos_anulados"))  
  agruparPorCidade(cargos)  
}

porcento <- function( total, value ) {
  (value * 100) / total
}

agruparPorCidade <- function ( results ) {
  cidades <- unique(results$nome_municipio)
  sumVotes <- function( cidade ) {
    filtered <- subset(results, nome_municipio == cidade)
    
    aptos <- sum(filtered$qtd_aptos_tot)
    comparecimentos <- sum(filtered$qtd_comparecimento)
    abstencoes <- sum(filtered$qtd_abstencoes)
    brancos <- sum(filtered$qtd_votos_brancos)
    nulos <- sum(filtered$qtd_votos_nulos)
    invalidos <- abstencoes + brancos + nulos
        
    data.frame(cidade=cidade, 
         aptos=aptos,
         comparecimentos=comparecimentos,
         abstencoes=abstencoes,
         abstencoes_pc=porcento(aptos, abstencoes),
         brancos=brancos,
         brancos_pc=porcento(aptos, brancos),
         nulos=nulos,
         nulos_pc=porcento(aptos, nulos),
         invalidos=invalidos,
         invalidos_pc=porcento(aptos,invalidos),
         stringsAsFactors=FALSE
         )
  }
  rows <- lapply(cidades, sumVotes)
  frame <- Reduce(rbind, rows)
  frame[order(frame$abstencoes_pc, frame$brancos_pc, frame$nulos_pc),]
}

totaisParaGovernador <- function () {
  calcularTotais <- function( fileName ) {
    dados <- readPorCargo( fileName, 3, 1)
    data.frame(
      eleicao=fileName
      )
  }
  
  rows <- lapply(governadoresFiles, calcularTotais)
  Reduce(rbind,rows)
}










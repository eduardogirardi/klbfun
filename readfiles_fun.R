
#leitura do dados de campo
#esta função le os dados de campo. Os dados devem ser salvos em um arquivo formatdo csv e ter como delimitador o ";" e "," para separacao decimal. 
#sao 27 campos obrigatorios e devem estar ordenados conforme a variavel "dc_names" da funcao. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de dat estao definidos para serem utilizados como dd/mm/aaaa - padrao mais utilizado no Brasil
#o encording pre definido é o ISO-8859-1 - Latin alphabet - padrao nos computados da klabin

read_dc <- function(file = dc_pth , guess_max = 100000){
  dc <- read_csv2(file,
                  locale = locale(encoding = 'ISO-8859-1',
                                  date_format = "%d/%m/%Y"),
                  na = c(".", "NA", "NaN", ""),
                  guess_max = guess_max)
  
  #defini o nome das principais variaveis
  dc_names <- c("atividade","rf", "talhao", "ciclo", "rotacao", "dt_med", "lider", "auxiliar", "parcela", "tipo",
                "forma", "hr_ini", "hr_fim", "coordX", "coordY", "lado1", "lado2", "inc1", "inc2", "linha", "arvore",
                "cap", "alt", "cod1", "cod2", "codQ")
  
  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(dc)) == length(dc_names)) {
    names(dc) <- dc_names
  } else if (length(names(dc)) > length(dc_names)){
    dc_names <- c(dc_names, names(dc)[28:length(names(dc))])
    names(dc) <- dc_names
  } else {
    stop("Falta variaveis no arquivo de campo. Consultar documentação.")
  }
  
  coltype <- cols(
    atividade = col_character(),
    rf = col_character(),
    talhao = col_character(),
    ciclo = col_double(),
    rotacao = col_double(),
    dt_med = col_date(format = ""),
    equipe = col_character(),
    auxiliar = col_character(),
    parcela = col_double(),
    tipo = col_character(),
    forma = col_character(),
    hr_ini = col_time(format = ""),
    hr_fim = col_time(format = ""),
    coordX = col_double(),
    coordY = col_double(),
    lado1 = col_double(),
    lado2 = col_double(),
    inc1 = col_double(),
    inc2 = col_double(),
    linha = col_double(),
    arvore = col_double(),
    cap = col_double(),
    alt = col_double(),
    cod1 = col_character(),
    cod2 = col_character(),
    codQ = col_character())
  
  dc <- type_convert(dc,
               col_types = coltype)
  
  return(dc)
}



###########################
#leitura do dados de campo
#esta função le os dados de cadastrais dos talhoes. Os dados devem ser salvos em um arquivo formatdo csv e ter como separador o ";" e "," para separacao decimal.
#sao 12 campos obrigatorios e devem estar ordenados conforme a variavel "dc_names" da funcao. Não imposta a nomenclatura dos campos no arquivo csv, mas sim a posição.
#os campos de data estao definidos para serem utilizados como dd/mm/aaaa - padrao mais utilizado no Brasil
#o encording pre definido é o ISO-8859-1 - Latin alphabet - padrao nos computados da klabin
#demais campos podem ser adicionados apos a ultima coluna e podem ser usados para estratificação por exemplo - evite caracteres especiais e espacos em nome de variaveis!!!!!

read_cad <- function(file = dc_cad , guess_max = 30000){
  cad <- read_csv2(file,
                  locale = locale(encoding = 'ISO-8859-1',
                                  date_format = "%d/%m/%Y"),
                  na = c(".", "NA", "NaN", ""),
                  guess_max = guess_max)
  
  #defini as variaveis essenciais
  cad_names <- c("centro",	"rf",	"talhao",	"ciclo",	"rotacao",	"especie",	"matgen", "espacamento" ,"regime",	"dt_plt",	"dt_int",	"area_plt")
  
  #ajustando os nomes da variaveis conforme o numero de variais do input
  if (length(names(cad)) == length(cad_names)) {
    names(cad) <- cad_names
  } else if (length(names(cad)) > length(cad_names)){
    cad_names <- c(cad_names, names(cad)[13:length(names(cad))])
    names(cad) <- cad_names
  } else {
    stop("Falta variaveis no arquivo de cadastro. Consultar documentação.")
  }
  
  coltype <- cols(
    centro = col_character(),
    rf = col_character(),
    talhao = col_character(),
    ciclo = col_double(),
    rotacao = col_double(),
    especie = col_character(),
    matgen = col_character(),
    espacamento = col_character(),
    regime = col_character(),
    dt_plt = col_date(format = ""),
    dt_int = col_date(format = ""),
    area_plt = col_double())
  
  cad <- type_convert(cad,
               col_types = coltype)
  
  return(cad)
}

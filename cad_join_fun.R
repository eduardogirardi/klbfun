cad_join <- function(dc, cad, by = c("centro", "rf", "talhao", "ciclo", "rotacao"), ...){
  
  #remove dados duplicados no cadastro
  
  cad <- cad %>% 
    distinct(across(all_of(by)), .keep_all = T)
  
  
  #junta dados de campo com o cadastro
  dt <- dc %>% 
    left_join(cad, by = by, ...)
}

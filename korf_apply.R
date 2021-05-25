
korf_apply <- function(A1=1, A2= 1, m1=1, m2=1, IA=1, Y1=1, I1=1, I2=1){
  
  #modelo de 1 etapa
  Korf_e1 <- function(A=1, m=1, Y1=1, I1=1, I2=1) {
    Y2 <- A * (Y1/A) ^ ((I1/I2)^m)
    return(Y2)
  }
  
  #projeco no modelo de 2 etapas
  Korf_proj <- function(A1=1, A2= 1, m1=1, m2=1, IA=1, Y1=1, I1=1, I2=1) {
    YB <- A1 * (Y1/A1) ^ ((I1/IA)^m1)
    Y2 <- A2 * (YB/A2) ^ ((IA/I2)^m2)
    return(Y2)
  }
  
  #regrecao no modelo de 2 etapas
  Korf_regre <- function(A1=1, A2= 1, m1=1, m2=1, IA=1, Y1=1, I1=1, I2=1) {
    YB <- A2 * (Y1/A2) ^ ((I1/IA)^m2)
    Y2 <- A1 * (YB/A1) ^ ((IA/I2)^m1)
    return(Y2)
  }
  
  case_when((round(I1) < IA & IA >= I2) ~ (Korf_e1(A=A1, m=m1, Y1=Y1, I1=I1, I2=I2)),
            (round(I1) < IA & IA < I2) ~ (Korf_proj(A1=A1, A2=A2, m1=m1, m2=m2, IA=IA, Y1=Y1, I1=I1, I2=I2)),
            (round(I1) > IA & IA <= I2) ~ (Korf_e1(A=A2, m=m2, Y1=Y1, I1=I1, I2=I2)),
            (round(I1) > IA & IA > I2) ~ (Korf_regre(A1=A1, A2=A2, m1=m1, m2=m2, IA=IA, Y1=Y1, I1=I1, I2=I2)),
            (round(I1) == IA & IA > I2) ~ (Korf_e1(A=A1, m=m1, Y1=Y1, I1=I1, I2=I2)),
            (round(I1) == IA & IA <= I2) ~ (Korf_e1(A=A2, m=m2, Y1=Y1, I1=I1, I2=I2)))
}
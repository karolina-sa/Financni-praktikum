# 2. DOMAČA NALOGA: VREDNOTENJE EKSOTIČNIH OPCIJ
# Finančni praktikum 2022/23
# Karolina Šavli

library(combinat)
library(Rlab)
library(dplyr)

# 1. naloga ####################################################################
# a)
S0 <- 50
u <- 1.05
d <- 0.95
U <- 5
R <- 0.03 # oz. 3 %
T <- 3

S0 <- c(50, 50, 50, 50, 50)
S1 <- c(52.5, 52.5, 47.5, 47.5, 52.5)
S2 <- c(49.88, 55.12, 49.88, 45.12, 49.88)
S3 <- c(52.37, 57.88, 47.38, 47.38, 52.37)
S4 <- c(49.75, 60.78, 45.01, 49.75, 54.99)
S5 <- c(52.24, 63.81, 42.76, 52.24, 57.74)

# sestavljena tabela:
df <- data.frame(S0, S1, S2, S3, S4, S5)
# dodana nov stolpec: 'IzplaciloX' in 'IzplaciloY':
df <- df %>%
  mutate(IzplaciloX = pmax(pmax(df$S3, df$S4, df$S5) - pmax(df$S0, df$S1, df$S2), 0)) %>%
  mutate(IzplaciloY = pmax(pmin(df$S3, df$S4, df$S5) - pmin(df$S0, df$S1, df$S2), 0))

#-------------------------------------------------------------------------------

# b)
izplacilo <- function(vrsta, T, type) {
  if (type == "call") {
    izplacilo <- max(max(vrsta[-c(1:T)]) - max(vrsta[c(1:T)]), 0)
  }
  else {
    izplacilo <- max(min(vrsta[-c(1:T)]) - min(vrsta[c(1:T)]), 0)
  }
  return(izplacilo)
}

izplacilo(c(50,52.5,49.88,52.37,49.75,52.24),3,"call")
izplacilo(c(50,52.5,55.12,57.88,60.78,63.81),3,"put")
izplacilo(c(50,52.5,55.12,57.88,60.78,63.81),3,"call")
izplacilo(c(60,61.2,59.98,58.78,57.6,58.75,57.58),5,"put")
izplacilo(c(60,58.8,57.62,58.78,59.95,61.15,62.37),4,"call")
izplacilo(c(70,66.5,69.83,73.32,76.98,73.13,69.48),2,"put")
#           pri vseh mi vrne pravilno

# ==============================================================================
# ==============================================================================

# 2. naloga ####################################################################
# a)
binomski <- function(S0, u, d, U, R, T, type) {
  q <- (1 + R - d) / (u - d)
  cube <- hcube(rep(2, U)) - 1 
  # 0 zamenjam z d, 1 pa z u:
  cube[cube == 0] <- d
  cube[cube == 1] <- u
  # S0 spremenim v vektor:
  vek_S0 <- rep(S0, nrow(cube))
  # na začetek dodam nov stolpec S0
  cube <- cbind(vek_S0, cube)
  # komulativni produkt s funkcijo cumprod (da dobimo vse možne poti v binomskem drevesu):
  cube <- t(apply(cube, 1, cumprod))
  # verjetnosti za posamezne poti (vektor Q):
  cc <- hcube(rep(2, U)) - 1 # začetna cube
  st_u <- rowSums(cc)
  st_d <- U - st_u
  Q <- q^st_u * (1 - q)^st_d
  # izplačilo na posamezni vrstici:
  vek_izplacilo <- c()
  for (i in 1:nrow(cube)) {
    vek_izplacilo <- append(vek_izplacilo, izplacilo(cube[i,], T, type=type))
  }
  # povprečna verjetnost:
  povpr_Q <- sum(vek_izplacilo * Q)
  # diskontiramo:
  premija_opcije <- povpr_Q / ((1 + R) ^ U)
  return(premija_opcije)
}

binomski(50,1.05,0.95,5,0.03,3,"call")
binomski(50,1.05,0.95,5,0.03,3,"put")
binomski(50, 1.05, 0.9 , 10, 0.03, 5, "call")
binomski(60, 1.05, 0.95, 15, 0.01, 8, "put" )
binomski(70, 1.05, 1, 7, 0, 5, "call")           ## !! TALE NE VRNE PRAVILNO - vrne 13.41159 namesto 0 !!
binomski(80, 1.1, 0.95,  9, 0.05, 4, "put" )
binomski(90, 1.15, 0.8, 10, 0.01, 3, "call")

#-------------------------------------------------------------------------------

# b)
S0 <- 60
u <- 1.05
d <- 0.95
U <- 15
R <- 0.01
T <- 8
type = "put"
N1 <- 10
N2 <- 100
N3 <- 1000

monte <- function(S0, u, d, U, R, T, type, N) {
  q <- (1 + R - d) / (u - d)
  # konstrukcija nove kocke:
  bin_matrika <- matrix(rbinom(N*U, 1, q), nrow = N, ncol = U)

  # st_u in st_d potrebujem na koncu, ker pa se rbinom spreminja število enk
  #         oziroma število ničel shranim že zdaj
  st_u <- rowSums(bin_matrika)
  st_d <- U - st_u

  vek_S0 <- rep(S0, N)
  bin_matrika[bin_matrika == 0] <- d
  bin_matrika[bin_matrika == 1] <- u
  bin_matrika <- cbind(vek_S0, bin_matrika)
  bin_matrika <- t(apply(bin_matrika, 1, cumprod))
  vek_izplacilo <- apply(bin_matrika, 1, function(x) izplacilo(x, T, type))
  povpr_Q <- sum(vek_izplacilo)/ N
  premija_opcije <- povpr_Q / ((1 + R) ^ U)
  return(premija_opcije)
}

monte(50, 1.05, 0.9, 10, 0.03, 5, "call", 100)
monte(70, 1.05, 1, 7, 0, 5, "put", 2000) ## !! TALE NE VRNE PRAVILNO - vrne VEDNO 19.3397 namesto 0 !!
monte(90, 1.15, 0.8 , 10, 0.01, 3, "call", 50000)

# ==============================================================================
# ==============================================================================

# 3. naloga ####################################################################
# a) in b)
S0 <- 60
u <- 1.05
d <- 0.95
U <- 15
R <- 0.01 # oz. 1 %
T <- 8
type = "put"
N1 <- 10
N2 <- 100
N3 <- 1000

# funkcija, ki vrne želen histogram odvisno od S0, u, d, U, R, T, type, N in M
histogram <- function(S0, u, d, U, R, T, type, N, M) {
  # ponovi funkcijo monte M-krat in nam vrne vektor 
  #          M vrednosti funkcije monte:
  vek <- c()
  while (M > 0) {
    vek <- append(vek, monte(S0, u, d, U, R, T, type, N))
    M <- M - 1
  }
  # vrednost dobljena s funkcijo binomski
  bin <- binomski(S0, u, d, U, R, T, type)
  # risanje histograma:
  # narisan histogram, urejene osi ter naslov:
  hist <- hist(vek, 
               main = paste0("Monte carlo: N =", N), 
               col = "yellow", 
               xlab = "Premija",
               ylab = "Frequency", 
               xlim = c(0, 10),
               ylim = c(0, 30)
  )
  # dodani vodoravna premici, ki prikazuje povprečno oceno ter vrednost
  #     dobljeno s funkcijo binomski
  abline(v = c(mean(vek), bin), col = c("green", "red"), lty = c(1, 3), lwd = 2)
  # dodani puščici (v desno in levo)
  arrows(mean(vek) , 0, mean(vek) + sd(vek),0, col = "green", lwd=2, length = 0.07)
  arrows(mean(vek) , 0, mean(vek) - sd(vek),0, col = "green", lwd=2, length =  0.07)
  # dodana legenda:
  legend("topright", legend = c("Monte Carlo", "analiza modela"),  box.lty = 0, 
         col = c("green","red"), 
         lty = c(1, 3), lwd=1.5)
}

M <- 100
histogram(S0, u, d, U, R, T, type, N1, M)
histogram(S0, u, d, U, R, T, type, N2, M)
histogram(S0, u, d, U, R, T, type, N3, M)


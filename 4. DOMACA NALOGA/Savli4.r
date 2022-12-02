# 4. DOMAČA NALOGA: GLAJENJE ČASOVNIH VRST 
# Finančni praktikum 2022/23
# Karolina Šavli

library(readr)
library(dplyr)

# 1. naloga ####################################################################
# a)
srebro <- read_csv("srebro22.csv")
srebro <- srebro[1:113,]

#-------------------------------------------------------------------------------

# b)
casovna_vrsta <- ts(srebro$Close)
casovna_vrsta <- as.numeric(gsub("\\$", "", casovna_vrsta))

ts.plot(casovna_vrsta, 
        ylab = "USD", 
        main = "Srebro")
points(casovna_vrsta, 
       pch = 20)

# ==============================================================================
# ==============================================================================

# 2. naloga ####################################################################
# a)
G <- function(vrsta, k){
  # funckija G časovni vrsti priredi zglajene vrednosti
  T <- length(vrsta)
  zglajene <- c()
  for (i in (1 + k) : (T + 1)){
    zglajene[i] <- sum(vrsta[(i - 1) : (i - k)]) / k
  }
  zglajene
}

#-------------------------------------------------------------------------------

# b)
# drseče povprečje reda 5 (k = 5)
napovedovanje <- function(vrsta, k){
  zglajena_vrsta <- G(vrsta, k)
  napoved <- rep(tail(zglajena_vrsta, n = 1), 10)
  napoved_vrsta <- ts(c(zglajena_vrsta, napoved))
  return(napoved_vrsta)
}

napoved <- napovedovanje(casovna_vrsta, 5)

#-------------------------------------------------------------------------------

# c)
ts.plot(ts(casovna_vrsta),
        ts(napoved),
        main = "Drseče povprečje", 
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)

#-------------------------------------------------------------------------------

# d) SREDNJE KVADRATNA NAPAKA oz. MSE
srednja_kvadratna_napaka <- function(vrsta, k){
  T <- length(vrsta)
  napaka <- 0
  for (i in k : (T - 1)){
    napaka <- napaka + (vrsta[i + 1] - G(vrsta, k)[i + 1]) ^ 2
  }
  return(napaka / (T - k))
}

srednja_kvadratna_napaka(casovna_vrsta, 5)
# 0.4226481

#-------------------------------------------------------------------------------

# e)
# red glajenje k = 15
# točka b) za k = 15:
napoved15 <- napovedovanje(casovna_vrsta, 15)
napoved15
# točka d) za k = 15:
srednja_kvadratna_napaka(casovna_vrsta, 15)
# 1.050176

# red glajenje k = 30
# točka b) za k = 30:
napoved30 <- napovedovanje(casovna_vrsta, 30)
napoved30
# točka d) za k = 30:
srednja_kvadratna_napaka(casovna_vrsta, 30)
# 1.200601

# točka c) za k = 15 in k = 30. Grafa morata biti narisana drug poleg drugega
par(mfrow=c(2,2))

graf1 <- ts.plot(ts(casovna_vrsta),
        ts(napoved),
        main = "Drseče povprečje reda 5", 
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)

graf2 <- ts.plot(ts(casovna_vrsta),
        ts(napoved15),
        main = "Drseče povprečje reda 15", 
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)

graf3 <- ts.plot(ts(casovna_vrsta),
        ts(napoved30),
        main = "Drseče povprečje reda 30", 
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)

# ------------------------

par(mfrow=c(1,1))

# ==============================================================================
# ==============================================================================

# 3. naloga ####################################################################
# a)
EG <- function(vrsta, alpha){
  T <- length(vrsta)
  glajene_vrednosti <- vrsta[1]
  for (i in 2 : T){
    glajene_vrednosti[i] <- alpha * vrsta[i] + (1 - alpha)*glajene_vrednosti[i - 1]
  }
  vrsta <- ts(glajene_vrednosti)
  return(vrsta)
}

#-------------------------------------------------------------------------------

# b)
# alpha <- 0.2 (izbiram iz intervala [0.1, 0.3])
zglajena_vrsta1 <- EG(casovna_vrsta, 0.2)
# napoved:
last(zglajena_vrsta1)
# 21.94184

# graf:
ts.plot(ts(casovna_vrsta),
        ts(zglajena_vrsta1), 
        main = "Eksponentno glajenje",
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)

#-------------------------------------------------------------------------------

# c)
# alpha <- 0.8 (izbiram iz intervala [0.7, 0.9])
zglajena_vrsta2 <- EG(casovna_vrsta, 0.8)
# napoved:
last(zglajena_vrsta2)
# 21.93306

# graf:
ts.plot(ts(casovna_vrsta),
        ts(zglajena_vrsta2), 
        main = "Eksponentno glajenje",
        ylab = "USD", 
        col = c("black","red"))
points(casovna_vrsta, pch = 20)


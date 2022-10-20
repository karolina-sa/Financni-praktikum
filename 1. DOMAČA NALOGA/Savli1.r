# 1. DOMAČA NALOGA: OBRESTNE KRIVULJE
# Finančni praktikum 2022/23
# Karolina Šavli

# 1. naloga ####################################################################
# a)
# Izbrala sem casovno obdobje 2014-2016
# uvoz podatkov (.csv datotek):
leto2014 <- read.csv("hist_EURIBOR_2014.csv")
leto2015 <- read.csv("hist_EURIBOR_2015.csv")
leto2016 <- read.csv("hist_EURIBOR_2016.csv")

#-------------------------------------------------------------------------------

# b)
library(stringr) # potrebujem za funkcijo str_replace_all
library(dplyr) # za funkcijo mutate

# funkcija za iskanje zaporednih številk vrstic prvih delovnih dni v mesecu:
stevilke_vrstic <- function(tabela) {
  # omejim se samo na prvi stolpec (torej imam vektor). Uporabila sem data.frame, da bom
  #     lahko z rekurzivnimi izrazi obravnavala tabelo
  vec <- data.frame(rownames(tabela))
  colnames(vec) <- "mm"
  # v stolpcu potrebujem zgolj številko meseca - zanimala me bo prva pojavitev vsakega meseca
  # uporabim ragularni izraz:
  vec <- vec %>%
    mutate(mm = str_replace_all(mm, "X(\\d{1,2}).(\\d{2}).(\\d{4})", "\\2"))
  # s funkcijo match najdem prve pojavitve mesecev in jih spravim v vektor "st":
  st <- c()
  meseci <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  for (m in meseci) {
    st <- append(st, match(m, vec$mm))
  }
  return(st)
}

# Urejanje tabel v predpisano obliko:
# zamenjava vrstic in stolpcev:
leto2014.p <- data.frame(t(leto2014[-1]))
colnames(leto2014.p) <- leto2014[,1]
# izbira samo vrstic, ki predstavljajo prvi dan posameznega meseca:
leto2014.p <- leto2014.p[stevilke_vrstic(leto2014.p),]

leto2015.p <- data.frame(t(leto2015[-1]))
colnames(leto2015.p) <- leto2015[,1]
leto2015.p <- leto2015.p[stevilke_vrstic(leto2015.p),]

leto2016.p <- data.frame(t(leto2016[-1]))
colnames(leto2016.p) <- leto2016[,1]
leto2016.p <- leto2016.p[stevilke_vrstic(leto2016.p),]

# zdruzim tabele treh zaporednih let:
obdobje_treh_let <- rbind(rbind(leto2014.p, leto2015.p), leto2016.p)

#-------------------------------------------------------------------------------

# c)
# T = 6; U = 12
# spremenim poimenovanje 1. stolpca:
poim <- rownames(obdobje_treh_let)

poim <- 
  str_replace_all(poim, "(X\\d{1,2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2")
rownames(obdobje_treh_let) <- poim
  
# priprava podatkov za graf:
timeseries1 <- ts(obdobje_treh_let$`6m`, start=c(2014, 1), end=c(2016, 12), frequency=12)
timeseries2 <- ts(obdobje_treh_let$`12m`, start=c(2014, 1), end=c(2016, 12), frequency=12)
# graf z legendo:
par(pty="m")
plot.ts(cbind(timeseries1, timeseries2),
        plot.type = "single",
        main = "Spreminjanje 6 mesečne in 12 mesečne \nobrestne mere med leti 2014 in 2016", 
        xlab = "Čas [leta]", 
        ylab = "Obrestna mera [%]",
        col = c("red", "blue")
        )
legend(2016.2, 0.55, legend=c("6m", "12m"),
       col = c("red", "blue"),
       lty=1:1, cex=0.8,
       box.lty=0)

# ==============================================================================
# ==============================================================================

# 2. naloga ####################################################################
# a)
# izbrani datumi: 2014-05 ; 2014-12 ; 2016-03

# b)
time <- c(0.25, 0.5, 1, 2, 3, 6, 9, 12)
plot(time, obdobje_treh_let[5,],
     type = "o",
     pch = 19,
     xlim=c(0,12),
     ylim=c(-0.34,0.65),
     col = "black",
     main = "Časovna struktura obrestnih mer \nna izbrane datume", 
     xlab = "Dospetje [mesec]", 
     ylab = "Obrestna mera [%]",
     )
lines(time, obdobje_treh_let[12,],
      type = "o",
      pch = 19,
      col = "red")
lines(time, obdobje_treh_let[27,],
      type = "o",
      pch = 19,
      col = "blue")
legend(5, 0.7, legend = "2014-05", bty = "n", text.col="black")
legend(8, 0.5, legend = "2015-01", bty = "n", text.col="red")
legend(8, 0.17, legend = "2016-03", bty = "n", text.col="blue")

# OBLIKA PRIKAZANIH KRIVULJ:
# Vse tri krivulje imajo obliko normalne krivulje časovne strukture obrestnih 
#   mer (Normal yield curve), saj imajo kratkoročne obrestne mere manjšo 
#   donosnot, kot obrestne mere s kasnejšim dospetjem. 
#   Krivulja je zaradi slednjega naraščajoča. Naklon krivulje "napoveduje"
#   pričakovanje višjih obrestnih mer v prihodnosti. 

# ==============================================================================
# ==============================================================================

# 3. naloga ####################################################################
# a)
# funkcija za izračun terminske obrestne mere:
library(readr)
T <- 0.5
U <- 1
# da bom lahko podatke med seboj seštevala, množila in delila:
obdobje_treh_let$`6m` <- as.character(obdobje_treh_let$`6m`)
obdobje_treh_let$`6m` <- parse_number(obdobje_treh_let$`6m`)
obdobje_treh_let$`12m` <- as.character(obdobje_treh_let$`12m`)
obdobje_treh_let$`12m` <- parse_number(obdobje_treh_let$`12m`)

# dodan nov stolpec termin, ki predstavlja terminsko obrestno mero L(0,T,U):
obdobje_treh_let <- obdobje_treh_let %>%
  mutate(termin = (1 / (U - T)) * (((1 + U * obdobje_treh_let$`12m`) / (1 + T * obdobje_treh_let$`6m`)) - 1))

# b)
# za lažjo obravnavo omejim tabelo:
obdobje_treh_let_terminske <- obdobje_treh_let[,c(6,8,9)]
# zadnji stolpec v obdobje_treh_let_terminske zamankem za 6 dol in
#       prvih šest vrstic nadomestim z NA
obdobje_treh_let_terminske$Napoved6m <- 
  c(c(NA, NA, NA, NA, NA, NA), obdobje_treh_let_terminske[1:30, 3])

# c)
obdobje_treh_let_terminske$leto <- rownames(obdobje_treh_let_terminske)
# dodam nov stolpec leto, da bom lažje grupirala po različnih letih:
obdobje_treh_let_terminske <- obdobje_treh_let_terminske %>%
  mutate(leto = str_replace_all(leto, "(\\d{4}).(\\d{2})", "\\1")) %>%
  na.omit()
# da bom delala s številkami:
obdobje_treh_let_terminske$Napoved6m <- as.character(obdobje_treh_let_terminske$Napoved6m)
obdobje_treh_let_terminske$Napoved6m <- parse_number(obdobje_treh_let_terminske$Napoved6m)

par(pty="s") # za enako razmerje osi
plot(obdobje_treh_let_terminske$Napoved6m, 
     obdobje_treh_let_terminske$`6m`,
     col=factor(obdobje_treh_let_terminske$leto),
     main = "6m Euribor 2014-2016", 
     xlab = "Napoved", 
     ylab = "Opazovano",
     xlim = c(-0.25, 0.7),
     ylim = c(-0.25, 0.7),
     pch = 20
     )
lines(x = c(-10,100), y = c(-10,100), lty=2)
legend("topleft",
       bty ="n",
       legend = levels(factor(obdobje_treh_let_terminske$leto)),
       pch = 19,
       cex=0.8,
       col = factor(levels(factor(obdobje_treh_let_terminske$leto))))
abline(lm(obdobje_treh_let_terminske$`6m` ~ obdobje_treh_let_terminske$Napoved6m))

# d)
# leto 2014:
obdobje_treh_let_terminske14 <- obdobje_treh_let_terminske %>%
  filter(leto == 2014)
par(pty="s")
plot(obdobje_treh_let_terminske14$Napoved6m, 
     obdobje_treh_let_terminske14$`6m`,
     main = "6m Euribor 2014", 
     xlab = "Napoved", 
     ylab = "Opazovano",
     xlim = c(-0.25, 0.7),
     ylim = c(-0.25, 0.7),
     pch = 20,
     col = "red"
)
lines(x = c(-10,100), y = c(-10,100), lty=2)
abline(lm(obdobje_treh_let_terminske$`6m` ~ obdobje_treh_let_terminske$Napoved6m),
       col = "red")

# leto 2015:
obdobje_treh_let_terminske15 <- obdobje_treh_let_terminske %>%
  filter(leto == 2015)
par(pty="s")
plot(obdobje_treh_let_terminske15$Napoved6m, 
     obdobje_treh_let_terminske15$`6m`,
     main = "6m Euribor 2015", 
     xlab = "Napoved", 
     ylab = "Opazovano",
     xlim = c(-0.25, 0.7),
     ylim = c(-0.25, 0.7),
     pch = 20,
     col = "blue"
)
lines(x = c(-10,100), y = c(-10,100), lty=2)
abline(lm(obdobje_treh_let_terminske$`6m` ~ obdobje_treh_let_terminske$Napoved6m),
       col = "blue")

# leto 2016:
obdobje_treh_let_terminske16 <- obdobje_treh_let_terminske %>%
  filter(leto == 2016)
par(pty="s")
plot(obdobje_treh_let_terminske16$Napoved6m, 
     obdobje_treh_let_terminske16$`6m`,
     main = "6m Euribor 2016", 
     xlab = "Napoved", 
     ylab = "Opazovano",
     xlim = c(-0.25, 0.7),
     ylim = c(-0.25, 0.7),
     pch = 20,
     col = "green"
)
lines(x = c(-10,100), y = c(-10,100), lty=2)
abline(lm(obdobje_treh_let_terminske$`6m` ~ obdobje_treh_let_terminske$Napoved6m),
       col = "green")

# e)
# HIPOTEZA PRIČAKOVANJ TRGA
# Hipoteza pričakovnj trga ne velja, saj bi se sicer grafikoni morali prilegati 
#   simetrali lihih kvadrantov - to bi pomenilo, da je napovedana obrestna 
#   mera enaka opazovani, saj hipoteza pričakovanj trga trdi, da
#   lahko terminsko obrestno mero primerjamo z dejansko obrestno mero, ki je veljala
#   na trgu U - T = 12 - 6 = 6 mesecev kasneje. 


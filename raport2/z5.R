library(DescTools)


dane <- read.csv("Reakcja.csv", sep = ";")
dane

alpha <- 0.05


dane$Reakcja <- as.factor(dane$Reakcja)
dane$Dawka <- as.factor(dane$Dawka)
dane$Rodzaj <- as.factor(dane$Rodzaj)
dane$Miejsce <- as.factor(dane$Miejsce)


skutecznosc_dawka <- matrix(c(21,25,32,32,37,19,15,8,8,3), nrow = 2, byrow = TRUE,
                            dimnames = list(Reakcja = c("0 Nie nastąpiła poprawa", "1 Nastąpiła poprawa"),
                                            Dawka = c("Dawka:-2","Dawka:-2,301","Dawka:-2,602","Dawka:-2,903","Dawka:-3,204")))

skutecznosc_miejsce <- matrix(c(86,61,14,39), nrow = 2, byrow = TRUE,
                              dimnames = list(Reakcja = c("0 Nie nastąpiła poprawa", "1 Nastąpiła poprawa"),
                                              Miejsce = c("0","1")))



tau_Reakcja_Dawka_C <- GoodmanKruskalTau(skutecznosc_dawka, direction="column")
print(paste("Współzmienność między Reakcją a Dawką (kolumna):", tau_Reakcja_Dawka_C))

tau_Reakcja_Dawka_R <- GoodmanKruskalTau(skutecznosc_dawka, direction="row")
print(paste("Współzmienność między Reakcją a Dawką (wiersz):", tau_Reakcja_Dawka_R))



tau_Reakcja_Miejsce_C <- GoodmanKruskalTau(skutecznosc_miejsce, direction="column")
print(paste("Współzmienność między Reakcją a Miejscem (kolumna):", tau_Reakcja_Miejsce_C))

tau_Reakcja_Miejsce_R <- GoodmanKruskalTau(skutecznosc_miejsce, direction="row")
print(paste("Współzmienność między Reakcją a Miejscem (wiersz):", tau_Reakcja_Miejsce_R))

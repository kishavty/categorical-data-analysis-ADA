#    metoda Bowkera
oceny_bowker <- matrix(c(5, 2, 1, 0, 0,
                            6, 3, 2, 2, 0,
                            1, 4, 5, 5, 2,
                            0, 10, 15, 18, 5,
                            1, 2, 5, 3, 2),
                        nrow = 5,
                        dimnames = list("Badanie1" = c("-2", "-1", "0", "1", "2"),
                                        "Badanie2" = c("-2", "-1", "0", "1", "2")))

print(oceny_bowker)

result_bowker_true <- mcnemar.test(oceny_bowker, correct=TRUE)
print(result_bowker_true)

result_bowker_false <- mcnemar.test(oceny_bowker, correct=FALSE)
print(result_bowker_false)

##    metoda IW

library(gnm)

oceny <- c(5, 2, 1, 0, 0,
           6, 3, 2, 2, 0,
           1, 4, 5, 5, 2,
           0, 10, 15, 18, 5,
           1, 2, 5, 3, 2)
bad1<-gl(5,5,labels=c("-2", "-1", "0", "1", "2"))
bad2<-gl(5,1,labels=c("-2", "-1", "0", "1", "2"))
DaneP1 <- data.frame(bad1,bad2,oceny)
DaneP1

symmetry <- glm(oceny ~ Symm(bad1, bad2), data=DaneP1, family = poisson)
summary(symmetry)

x = symmetry$deviance
x

#liczba stopni swobody
r = 10
p = 1-pchisq(x,r)
p



## quasi-symetria


quasi.symm <- glm(oceny ~ bad1+bad2 + Symm(bad1,bad2),data=DaneP1, family =poisson)
summary(quasi.symm)


x2=quasi.symm$deviance
x2

#Liczba stopni swobody
r2=6
p=1-pchisq(x2,r2)
p



## symetria pod warunkiem quasi symetrii


comparison <- anova(symmetry, quasi.symm)
comparison


p=1-pchisq(17.614, 4)
p









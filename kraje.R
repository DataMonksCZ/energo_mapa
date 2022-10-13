rm(list = ls())

library(RCzechia) #dostupná včetně návodů z https://www.jla-data.net/cze/package-rczechia/
library(ggplot2)
library(dplyr)
library(czso)
library(readxl)
library(colorspace) #hclwizard()
library(sf)

############# Import ###############
#dostupné z https://www.czso.cz/csu/czso/14-energetika-pno7t8lj6g
#14-101. Instalovaný výkon elektrizační soustavy podle krajů v roce 2020 
dt_kraje <- read_excel("data/data_kraje_energetika.xlsx", na = "-") 
dt_kraje[,2:ncol(dt_kraje)] <- apply(dt_kraje[,2:ncol(dt_kraje)], MARGIN = c(1,2), as.numeric)


########### Data transformace ############
#vytvoření colnames
el_kraje <- data.frame(t(dt_kraje))
names(el_kraje) <- el_kraje[1,]

#oddělání souhrnných dat za ČR do proměnné el_cr
el_cr <- el_kraje[2,]

#odebrání prvních dvou řádků (colnames a data cr) a prvního sloupce s % hodnotami dle krajů
el_kraje <- el_kraje[-1:-2,-1]


#vytvoří sloupec s názvy krajů
el_kraje <- el_kraje %>% mutate(nazev = rownames(el_kraje)) %>% relocate(nazev, .before = names(el_kraje)[1])
el_kraje$nazev <- rownames(el_kraje)

#doplní slovo kraj za názvy krajů
el_kraje$nazev <- c(el_kraje$nazev[1], paste(el_kraje$nazev[2:nrow(el_kraje)], "kraj"))
el_kraje$nazev[which(el_kraje$nazev == "Vysočina kraj")] <- "Kraj Vysočina"


################## Výpočty #################

#ze všeho krom názvu numeric data typ
el_kraje[,-1] <- apply(el_kraje[,-1], MARGIN = c(1,2), as.numeric, na.rm = TRUE)
el_kraje$suma <- round(apply(el_kraje[,-1], MARGIN = 1, sum, na.rm = TRUE),0)
#filter toho, co potřebujeme
el_kraje_f <- el_kraje %>% select(nazev, suma)

#funkce package RCzechia

of_kraje <- kraje("low") %>%
  inner_join(el_kraje_f, by = c("NAZ_CZNUTS3" = "nazev"))#spojíNAZ označení s názvy krajů z xlsx

#vytvoří intervaly pro zahrnutí dle instalovaného výkonu, bude sloužit pro legendu grafu a barevného odlišení
options(scipen = 9999)
of_kraje_cut <- of_kraje %>% mutate(value_cut = cut(suma, breaks = c(200, 700, 1200, 1700, 2800, 3200, 5700),
                                                    include.lowest=TRUE,
                                                    dig.lab = 10))

########################### Vizualizace ###########################

## Na míru vytvořená paleta skrz perfektní funkci hclwizard()
cols <- sequential_hcl(n = 6, h = 274, c = c(58, NA, NA), l = c(18, 98), power = 1.85, rev = TRUE, register = )


ggplot(data = of_kraje_cut)+
  geom_sf(data = republika(), color = "grey30", fill = NA)+ #vykreslí mapu ČR
  geom_sf(data = of_kraje_cut, aes(fill = value_cut), color = "black", size = 0.4)+ #vykreslí kraje a vyplní barvama
  geom_sf_label(aes(label = NAZ_CZNUTS3),fill = "white", fun.geometry = sf::st_centroid, size = 4,
                nudge_y = c(-0.2, rep(0,9), -0.1, rep(0,3)))+ #posune názvy středočeského a olomouckého kraje
  labs(fill = "Instalovaný výkon\n")+ #titulek legendy
  scale_fill_manual(values = cols, labels = c("200 - 700 MW",
                                              "700 - 1200 MW",
                                              "1200 - 1700 MW",
                                              "2200 - 2800 MW",
                                              "2800 - 3200 MW",
                                              "5200 - 5700 MW"), drop = FALSE)+ #popisky legendy
  theme_void() +
  theme(
    legend.position=c(0.75,1), #mění pozici legendy
    legend.justification=c(0, 1),
    legend.key.size = unit(0.8, 'cm'),
    legend.key.height = unit(0.6, 'cm'), #změní výšku popisků legendy
    legend.key.width = unit(1.1, 'cm'), #změní šíři popisků legendy
    legend.title = element_text(size=15), #změní velikost písma titulku
    legend.text = element_text(size=12), #změní velikost písma popisků
    legend.spacing.y = unit(0.3, 'cm')) + #změní rozestup položek legendy
      guides(fill = guide_legend(byrow = TRUE), #nutné, aby fungoval rozestup
  )
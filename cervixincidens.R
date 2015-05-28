##########################################################################################
#                                                                                        #
# Purpose:       Incidenstrend för cervixcnacer                                          #
#                                                                                        #
# Author:        Erik Bülow                                                              #
# Contact:       erik.bulow@rccvast.se                                                   #
# Client:        Björn Strandfer                                                         #
#                                                                                        #
# Code created:  2015-05-28                                                              #
# Last updated:  2015-05-28                                                              #
# Source:        /Users/erikbulow/Documents/cervix/bjorn                                 #
#                                                                                        #
# Comment:       Skript för att skapa ggplot2-graf med incidenstrender för cervixcancer .#
#                Björn Strander vill jämföra VGR (där signifikant                        #
#                förbättring/minskning finns) mot exempelvis hela riket, där en          #
#                minskning inte nödvändigtvis finns. Detta i syfte att illustrera att    #
#                VGR:s/Björns arbete har gett resultat.                                  #
#                                                                                        #
##########################################################################################



##########################################################################################
#                                                                                        #
#                                     Förberedelser                                      #
#                                                                                        #
##########################################################################################

checkpoint::checkpoint("2015-05-28")
library(dplyr)
library(lazyeval)
library(ggplot2)



##########################################################################################
#                                                                                        #
#                                          Data                                          #
#                                                                                        #
##########################################################################################

# Källa: Socialstyrelsens statistikdatabas

incidenser <- list(
  VGR         = c(10.76,  11.13,	10.17,	12.9,	9.08,	7.75,	10.72,	9.44,	8.33,	7.15,	8.16,	4.95),
  riket       = c(9.76,  9.63,	9.99,	10.02,	9.31,	9.12,	9.41,	10.01,	9.93,	9.29,	9.13,	8.79),
  Halland     = c(10.27, 3.42,	10.5,	6.41,	5.75,	3.85,	11.36,	6.43,	6.79,	9.86,	6.54,	9.56),
  Uppsala     = c(9.32,  10.64,	9.84,	12.75,	8.63,	16.95,	7.13,	7.52,	13.62,	14.81,	14.04,	15.06)
)

STARTAR <- 2000

##########################################################################################
#                                                                                        #
#                                    Data management                                     #
#                                                                                        #
##########################################################################################

# För vilka år gäller tidsserien?
ÅR <- seq(STARTAR, STARTAR + length(incidenser[[1]]) - 1, 1)

# Hjälpfunktion för data.frame för resp region
tidsserie <- function(incidens, region, år = ÅR) {
    data_frame(incidens = unname(unlist(incidens)), region, år)
}

# Skapa data.frame med data för alla regioner
incidens_data <- Map(tidsserie, incidenser, names(incidenser)) %>%
  rbind_all()



##########################################################################################
#                                                                                        #
#                                    Graf mha ggplot2                                    #
#                                                                                        #
##########################################################################################

ggplot(
    data = incidens_data,
    aes(x = factor(år), y = incidens, group = region, colour = region)) +
  xlab("Diagnosår") +
  ylab("Incidens") +
  ggtitle("Incidenstrend för cervixcancer") +
  geom_line(size = I(1)) +
  geom_point() +
  stat_smooth(method = lm, aes(fill = region)) +
  scale_y_continuous(breaks = seq(0, 18, 1), limits = c(0, 18)) +
  theme_gray(15)

ggsave("cervix_incidens.pdf")

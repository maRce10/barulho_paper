library(warbleR)
library(seewave)
library(tuneR)
library(baRulho)
library(Rraven)


wav_path <- "C:/Users/marco/OneDrive - Universidad de Costa Rica/Desktop/cosas marcelo/R PaLo/Test baRulho/precise_cuts_3"
alg.tests <- imp_raven(path = "C:/Users/marco/OneDrive - Universidad de Costa Rica/Desktop/cosas marcelo/R PaLo/Test baRulho/precise_cuts_3", 
                       files = "selection_table.txt", warbler.format = TRUE, all.data = TRUE)


unique(alg.tests$sound.files)
names(alg.tests)[17] <- "distance"
names(alg.tests)[19] <- "Transect"
names(alg.tests)
alg.tests$sound.id <- gsub(".wav", "", alg.tests$sound.files)
alg.tests$sound.id <- sub(".*-", "", alg.tests$sound.id)
alg.tests$reference <- ifelse(alg.tests$distance == 1, "control", "test")

options(sound.files.path = "C:/Users/marco/OneDrive - Universidad de Costa Rica/Desktop/cosas marcelo/R PaLo/Test baRulho/precise_cuts_3")

br <- blur_ratio(alg.tests)
ea <- excess_attenuation(alg.tests, gain = 82, type = 1)
sa <- signal_to_noise_ratio(alg.tests, noise.ref = "adjacent", mar = 0.01)
tsr <- tail_to_signal_ratio(alg.tests, type = 1, mar = 0.05)

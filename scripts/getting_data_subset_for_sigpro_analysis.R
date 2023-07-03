
library(warbleR)

# read extended selection table with re-recorded signals
est.alg.sim.all <- readRDS("~/Dropbox/Projects/degradation_simulation/extended_sel_tab_tests_simulated_tests_sounds.RDS")

# all from transect 1 or 1 m distance
est.alg.sim.all <- est.alg.sim.all[est.alg.sim.all$Transecto == 1 | est.alg.sim.all$`Distancia (m)` == 1, ]

# all from closed habitata
est.alg.sim.all <- est.alg.sim.all[est.alg.sim.all$Vegetacion == "cerrado" | est.alg.sim.all$`Distancia (m)` == 1,]


# add column with original sound file name
est.alg.sim.all$org.sf <- sapply(strsplit(as.character(est.alg.sim.all$sound.files), ".wav",fixed=T), "[[", 1)


# should show count of signals per file names by distance
table(est.alg.sim.all$org.sf, est.alg.sim.all$`Distancia (m)`)

#this are the sound files to analized
unique(est.alg.sim.all$org.sf)

# and this is the 1 m control
"200303-000.WAV"

# get subset of signals to analyze
sub_est <- est.alg.sim.all[est.alg.sim.all$orig.sound.file %in% est.alg.sim.all$orig.sound.file[1:20], ]

# should be 100
nrow(sub_est)

# new name for output waves
sub_est$new.file.name <-  paste0("dist.", sub_est$`Distancia (m)`)
sub_est$new.selec <- sapply(strsplit(as.character(sub_est$sound.files), ".wav_",fixed=T), "[[", 2)

# rename
sub_est <- rename_est_waves(X = sub_est, new.sound.files = paste(sub_est$new.file.name,  sub_est$new.selec, sep = "-"), new.selec = 1)

# export waves
# st <- exp_est(X = sub_est, path = "/home/neuro/Dropbox/estudiantes/Marcos Quiroz/sigpro_measuring/1st_transect_recordings/precise_cuts/", single.file = FALSE, file.name = "selection_table.txt")


# export rerecorded signals adding 1 snoise after and removing pre-margin 
out <- sapply(which(sub_est$`Distancia (m)` != 1), function(x){
  
  wv <- read_sound_file(X = sub_est, x,  to = sub_est$end[x] + 0.04)
  wv <- normalize(wv, unit = "16", rescale = FALSE)
  
  
  ns <- read_sound_file(X = sub_est, x, from = 0, to = 0.03)
  ns <- normalize(ns, unit = "16", rescale = FALSE)
  
  for(i in 1:6)  
    ns <- pastew(wave1 = ns, wave2 = ns, output = "Wave")
  
  wv <- pastew(cutw(ns, from = 0, to =  1, output = "Wave"), wv, output = "Wave")

  writeWave(wv, file.path("./data/raw/precise_cuts2/", paste0(sub_est$sound.files[x], ".wav")), extensible = FALSE)
  

})


# export 1 m controls removing margins
out <- sapply(which(sub_est$`Distancia (m)` == 1), function(x){
  
  wv <- read_sound_file(X = sub_est, x)

  wv <- normalize(wv, unit = "16", rescale = FALSE)
  
  
  writeWave(wv, file.path("/home/neuro/Dropbox/estudiantes/Marcos Quiroz/sigpro_measuring/controls/", paste0(sub_est$sound.files[x], ".wav")), extensible = FALSE)
  
})


# export full rerecorded recodings as mono
rerecs <- list.files("/home/neuro/Dropbox/estudiantes/Marcos Quiroz/sigpro_measuring/full_files/", full.names = TRUE)

out <- sapply(rerecs, function(x){
  
  wv <- read_sound_file(x)
  
  wv <- normalize(wv, unit = "16", rescale = FALSE)
  
  writeWave(wv, file.path("/home/neuro/Dropbox/estudiantes/Marcos Quiroz/sigpro_measuring/full_files/", gsub("\\.wav$", "-MONO.wav", basename(x))), extensible = FALSE)
  
})

# check
info_sound_files(path = "/home/neuro/Dropbox/estudiantes/Marcos Quiroz/sigpro_measuring/controls/")

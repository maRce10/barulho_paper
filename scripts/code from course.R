### Fundamentos de Bioacústica y Análisis de Sonidos Animales en R

equ <- function(a, b, c){
  
  operacion <- a * b * c
  
  operacion2 <- log(operacion)
  
  return(operacion2)
}

equ <- function(a, b, c) log(a * b * c)


equ(1, 2, 6)


# ejercicio 3.2 FUnciones


par(mfrow = c(3, 1))

out <- lapply(unique(iris$Species), function(x) 
  plot(iris$Sepal.Length[iris$Species == x], iris$Petal.Length[iris$Species == x], main = x)
)
#devoff()


# 3.7 

par(mfrow = c(3, 1))

out <- lapply(unique(iris$Species), function(x){ 
  
  X <- iris[iris$Species == x, ]
  
  cor.coef <- cor(X$Sepal.Length, X$Petal.Length)
  
  plot(X$Sepal.Length, X$Petal.Length, main = paste(x, " Cor. coef= ", round(cor.coef, 2)), cex = cor.coef * 7, pch = 20)
})


##########################################

# cortar primera nota
n1 <- cutw(tico, from=0.1, to=0.4, output="Wave")

# cortar segunda nota
n2 <- cutw(tico, from=0.6, to=0.9, output="Wave")

# cortar tercer nota
n3 <- cutw(tico, from=0.9, to= 0.9 + duration(n1), output="Wave")


n3@left <- n3@left[-length(n3@left)]

c12 <- corenv(n1, n2, f = 22050)
c13 <- corenv(n1, n3, f = 22050)
c23 <- corenv(n3, n2, f = 22050)

sp1 <- spec(n1)
sp2 <- spec(n2)
sp3 <- spec(n3)

cs12 <- corspec(sp1, sp2)
cs13 <- corspec(sp1, sp3)
cs23 <- corspec(sp3, sp2)

cvs12 <- covspectro(n1, n2, n = 11)
cvs13 <- covspectro(n1, n3, n = 11)
cvs32 <- covspectro(n3, n2, n = 11)

c(c12 = c12$rmax, c13 = c13$rmax, c23 = c23$rmax)

c(cs12 = cs12$rmax, cs13 = cs13$rmax, cs23 = cs23$rmax)

c(cvs12 = cvs12$covmax, cvs13 = cvs13$covmax, cvs23 = cvs32$covmax)


# enlace a Rraven   https://marce10.github.io/Rraven/articles/Rraven.html



### crear selecciones vacias

phae <- quer_xc(qword = "Phaethornis", download = FALSE)

table(phae$Specific_epithet)

subphae <- phae[phae$Specific_epithet %in% c("aethopygus", "idaliae", "mexicanus"), ]

table(subphae$Specific_epithet)


quer_xc(X = subphae, path = "C:/Users/posgrado/Documents/grab_phae")


mp32wav(path = "C:/Users/posgrado/Documents/grab_phae", samp.rate = 22.05, overwrite = TRUE)

wi <- wav_info(path = "C:/Users/posgrado/Documents/grab_phae")

library(tidyr)

wi <- separate(data = wi, col = "sound.files", sep = "-", into = c("genus", "epithet", "XCid"), remove = FALSE)


for(i in unique(wi$epithet))
  exp_empty_sels(path ="C:/Users/posgrado/Documents/grab_phae", file.name = i, sound.files = grep(i, wi$sound.files, value = TRUE))



### simular sonidos
#h ttps://marce10.github.io/2018/02/22/Simulating_animal_vocalizations.html
# https://marce10.github.io/2020/02/13/barulho.html


### leaflet maps

out <- quer_xc(qword = "Phaethornis guy", download = FALSE)

out$Latitude <- as.numeric(out$Latitude)
out$Longitude <- as.numeric(out$Longitude)
out <- out[!is.na(out$Longitude), ]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}
df.20 <- quakes[1:20,]

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = heat.colors(5)[as.numeric(as.factor(out$Country))]
)

leaflet(data = out) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Recording_ID), label = ~as.character(Recording_ID),  icon= icons)

####   ejemplo de simulacion de sonidos

# synthesize
synth.l <- sim_songs(n = 4, 
                     durs = 0.1, 
                     freqs = runif(n = 4, min = 1, max = 5),
                     harms = 1, 
                     gaps = 0.5,
                     diff.fun = "pure.tone", 
                     selec.table = TRUE, 
                     path = tempdir(), 
                     sig2 = 0.001,
                     steps = 3,  
                     file.name = "pure.tone",
                     bgn = 0.2)

# plot spectro
spectro(synth.l$wave, scale = FALSE, palette = reverse.topo.colors, 
        grid = FALSE, flim = c(0,8), collevels = seq(-20, 0, 1))


# make an extended selection table
synth.est <- selection_table(X = st, extended = TRUE, pb = FALSE,
                             confirm.extended = FALSE, path = tempdir())

library(baRulho)
# create master sound file
synth.master.sf <- master_sound_file(X = synth.est, file.name = "synthetic_master", 
                                     dest.path = tempdir(), gap.duration = 0.1)

# instalar ghostscript
https://www.ghostscript.com/download.html

Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.50/bin/gswin64c.exe")



last.img <- function() system(paste("eog", list.files(path = tempdir(), pattern = "\\.tiff$|\\.jpeg$|\\.jpg$", ignore.case = T, full.names = TRUE)[which.max(file.mtime(list.files(path = tempdir(), pattern = "\\.tiff$|\\.jpeg$|\\.jpg$", ignore.case = T, full.names = TRUE)))]
))




############## simul playback ####
# simulation parameters
gap <- 0.05
seed <- 15
set.seed(seed)
nharms <- 3
# frqs <- runif(n = 30, min = 0.5, max = 10)
frqs <- sample(rep(seq(0.5, 10, length.out = 20), 3))
steps <- 11
sig2 <- 0.8
# amplitude modulation with 2 peaks
mod.amps <- c(0.5, 1, 2, 3, 2, 1, 2, 3, 2, 1, 0.5) 

# harmonics amplitude
harm.amp <- c(1, rep(0.5, nharms - 1))

# make all posible combinations
eg <- expand.grid(dur = c(0.1, 0.2), 
                  fm = c("pure.tone", "BB"), 
                  am = c("no.am", "am"),
                  harm = c("no.harm", "harm"),
                  stringsAsFactors = FALSE
)

pboptions(type = "timer")

setwd("./simulated_songs")

# simulate songs
sim.songs <- pblapply(1:nrow(eg), function(x){
  
  if (eg$am[x] == "no.am") am.amps <- 1 else am.amps <- mod.amps
  if (eg$harm[x] == "no.harm") harms <- 1 else harms <- nharms
  
  sm.sng <- sim_songs(n = length(frqs),
                      durs = eg$dur[x], 
                      freqs = frqs,
                      harms = harms,
                      harm.amps = harm.amp,
                      gaps = gap,
                      am.amps = am.amps, 
                      diff.fun = eg$fm[x], 
                      selec.table = TRUE, 
                      sig2 = sig2,
                      steps = steps,  
                      file.name = paste(eg[x,], collapse = "_"),
                      bgn = 0,  
                      seed = seed)
  
  # add freq room if pure tone
  if (eg$fm[x] == "pure.tone") {
    sm.sng$selec.table$bottom.freq <- sm.sng$selec.table$bottom.freq - 0.2
    sm.sng$selec.table$top.freq <- sm.sng$selec.table$top.freq + 0.2
  } 
  
  if (eg$harm[x] == "harm") {
    sm.sng$selec.table$top.freq <- sm.sng$selec.table$top.freq * 1.5
  } 
  
  
  sm.sng$selec.table$bottom.freq[sm.sng$selec.table$bottom.freq < 0] <- 0.1
  
  return(sm.sng)
  
})

https://marce10.github.io/baRulho/articles/baRulho_quantifying_sound_degradation.html



#### ruidos

wn <- noise(kind = c("white"), duration = 2,
            samp.rate = 22050, bit = 16, stereo = FALSE,
            xunit = c("time"), alpha = 1)

spectro(wn,scale = F)

pn <- noise(kind = c("pink"), duration = 2,
            samp.rate = 22050, bit = 32, stereo = FALSE,
            xunit = c("time"), alpha = 0.5)

spectro(pn,scale = F)
seewave::spec(pn)

seewave::spec(wn)

n <- pastew(wn, pn, output = "Wave")
spectro(n,scale = F, collevels = seq(-80, 0, 5))
writeWave(n, "ruidos.wav", extensible = FALSE)
open_wd()




#### simular paisajes
library(warbleR)

# parametros

samp.rate <- 44100
dur <- 300 # 5 min

# cargar biofonias
data(tico)
data("sheep")
data("orni")
tico <- normalize(tico, unit = "16")
tico <- resamp(tico, g = 44100, output = "Wave")

sheep <- normalize(sheep, unit = "16")
sheep <- resamp(sheep, g = 44100, output = "Wave")

orni <- normalize(orni, unit = "16")
orni <- resamp(orni, g = 44100, output = "Wave")

bios <- list(tico, sheep, orni)


# crear ruidos
wn <- noise(kind = c("white"), duration = dur,
            samp.rate = samp.rate, bit = 32, stereo = FALSE,
            xunit = c("time"), alpha = 1)

wn <- normalize(wn, unit = "16")

pn <- noise(kind = c("pink"), duration = dur,
            samp.rate = samp.rate, bit = 32, stereo = FALSE,
            xunit = c("time"))

pn <- normalize(pn, unit = "16")
noises <- list(pn = pn, wn = wn)

lluvia <- readWave("C:/Users/posgrado/Documents/curso_bioacustica_R/samantha/242894__mshahen__gentle-soft-rain-night.wav")

viento <- readWave("C:/Users/posgrado/Documents/curso_bioacustica_R/samantha/181248__mario1298__weak-wind.wav")
viento <- resamp(viento, g = 44100, output = "Wave")

while(duration(viento) < 300)
  viento <- pastew(viento, viento, output = "Wave")

olas <- readWave("C:/Users/posgrado/Documents/curso_bioacustica_R/samantha/48412__luftrum__oceanwavescrushing.wav")

while(duration(olas) < 300)
  olas <- pastew(olas, olas, output = "Wave")

rio <- readWave("C:/Users/posgrado/Documents/curso_bioacustica_R/samantha/469009__innorecords__relaxing-mountains-rivers-streams-running-water.wav")

while(duration(rio) < 300)
  rio <- pastew(rio, rio, output = "Wave")

no.bio <- list(lluvia, viento, olas, rio)

no.bio <- lapply(no.bio, function(x) normalize(x, unit = "16"))

no.bio <- sapply(no.bio, function(x) cutw(x, from = 0, to = dur, output = "Wave"))


sapply(no.bio, duration)

# pista de fondo
pista.sil <- silence(duration = dur, samp.rate = samp.rate, xunit = "time")

pista.sil <- normalize(pista.sil, unit = "16")


add.wave <- function(wl, ws, pos.time, sd = 1){
  
  pos.sample <- pos.time * wl@samp.rate
  
  ws@left <- c(rnorm(n = pos.sample - 1, mean = 0, sd = sd), ws@left)  
  ws@left <- c(ws@left, rnorm(n = length(wl@left) - length(ws@left), mean = 0, sd = sd))
  ws@left <- ws@left[1:length(wl@left)]
  
  # ruido.suave@left <- ruido.suave@left  * noise.lev
  
  out.w <- wl + ws #+ ruido.suave
  out.w <- normalize(out.w, unit = "16")
  
  return(out.w)
}


poss <- sample(0:dur, 100)


for(i in poss)
{  if (i == poss[1])
  p1 <- add.wave(wl = pista.sil, ws = bio[[sample(1:3, 1)]], pos.time = i, sd = 1) else
    p1 <- add.wave(wl = p1, ws = bio[[sample(1:3, 1)]], pos.time = i, sd = 1)
}

writeWave(p1, filename = "sim-landscape.wav", extensible = FALSE)
# open_wd()


###### all possible combs
rds <- c("wn", "pn")

nobio <- paste0("nobio-", 1:4)

nobio <- c(rds, nobio)

bio <- paste0("bio-", 1:3)

bio <- c(rds, bio)

eg <- expand.grid(nobio = nobio, bio = bio)
eg2 <- eg[grepl("bio", eg$nobio) | grepl("bio", eg$bio), ]
eg3 <- eg[c(1, 8), ]
eg4 <- rbind(eg3, eg2)
eg4



eg4$treat <- paste(ifelse(grepl("nobio", eg4$nobio), "P", "N"),
                   ifelse(grepl("bio", eg4$bio), "P", "N"), sep = "-")

eg4 <- eg4[order(eg4$treat), ]


n.sounds <- 100

out <- pbapply::pblapply(1:nrow(eg4), function(x){
  
  print(x)
  # extraer valores para simulacion
  lin <- as.character(unlist(eg4[x, 1:2]))
  
  # definir pista ruido o silencio
  pista <- if (any(lin %in% c("wn", "pn"))) noises[[lin[lin %in% c("wn", "pn")][1]]] else pista.sil
  
  if (grepl("nobio", lin[1])) 
    pista <- pista + no.bio[[sample(1:length(no.bio), 1)]]
  
  # anadir bio o nobios si no es solo ruidos
  if (grepl("bio", lin[2])){
    
    wav.samp <-  bios
    
    poss <- sample(0:dur, n.sounds)
    
    for(i in poss)
    {  if (i == poss[1])
      simtrack <- add.wave(wl = pista, ws = wav.samp[[sample(1:length(wav.samp), 1)]], pos.time = i, sd = 1) else
        try(simtrack <- add.wave(wl = simtrack, ws = wav.samp[[sample(1:length(wav.samp), 1)]], pos.time = i, sd = 1), silent = TRUE)
    } 
    
  } else simtrack <- pista
  
  writeWave(simtrack, filename = paste0("./sim.tracks/sim-landscape_", paste0(lin, collapse = "-"), ".wav"), extensible = FALSE)
  
})



####### modelos mixtos degradacion

# model xcorr
xcm <- lmer(cross.correlation ~  # response
              duration + harmonics + fm + am + # signal features 
              + Temperatura + Humedad + # abiotic features
              distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
              (1 |  signal.type), 
            data = degrd,
            REML = FALSE)

# summary(xcm)

options(na.action = "na.fail")

ddxc <- dredge(xcm)

subset(ddxc, delta < 4)






#######

degrd <- read.csv("C:/Users/posgrado/Downloads/degradation_metrics_simulations.csv", stringsAsFactors = FALSE)

degrd$spectral.blur.ratio <- NULL

library(MuMIn)
library(lme4)
library(tidyr)

degrd <- separate(data = degrd, col = "signal.type", sep = "_", into = c("duration", "fm", "am", "harmonics", "borrar"), remove = FALSE)

degrd$duration <- ifelse(degrd$duration == "0.1", "short", "long")
degrd$fm <- ifelse(degrd$fm == "BB", "mod", "pure.tone")
degrd$harmonics <- gsub(".wav", "" , degrd$harmonics)
degrd$borrar <- NULL
str(degrd)


table(degrd$duration)
table(degrd$harmonics)
table(degrd$fm)
table(degrd$am)
table(degrd$harmonics)


# model xcorr
xcm <- lmer(cross.correlation ~  # response
              duration + harmonics + fm + am + # signal features 
              + Temperatura + Humedad + # abiotic features
              distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
              (1 |  signal.type), 
            data = degrd,
            REML = FALSE)

# summary(xcm)

options(na.action = "na.fail")

ddxc <- dredge(xcm)

subset(ddxc, delta < 4)

#'Best' model
bmxc <- (get.models(ddxc, 1)[[1]])
summary(bmxc)
confint(bmxc)



# model envelope.correlation
ecm <- lmer(envelope.correlation ~  # response
              duration + harmonics + fm + am + # signal features 
              + Temperatura + Humedad + # abiotic features
              distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
              (1 |  signal.type), 
            data = degrd,
            REML = FALSE)

# summary(xcm)

ddec <- dredge(ecm)

subset(ddec, delta < 4)

#'Best' model
bmec <- (get.models(ddec, 1)[[1]])
summary(bmec)
confint(bmec)


# model blur.ratio
brm <- lmer(blur.ratio ~  # response
              duration + harmonics + fm + am + # signal features 
              + Temperatura + Humedad + # abiotic features
              distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
              (1 |  signal.type), 
            data = degrd,
            REML = FALSE)

# summary(xcm)

ddbr <- dredge(brm)

subset(ddbr, delta < 4)

#'Best' model
bmbr <- (get.models(ddbr, 1)[[1]])
summary(bmbr)
confint(bmbr)

# model SNR
snrm <- lmer(snr.attenuation ~  # response
               duration + harmonics + fm + am + # signal features 
               + Temperatura + Humedad + # abiotic features
               distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
               (1 |  signal.type), 
             data = degrd,
             REML = FALSE)

# summary(xcm)

ddbsnr <- dredge(snrm)

subset(ddbsnr, delta < 4)

#'Best' model
bmsnr <- (get.models(ddbsnr, 1)[[1]])
summary(bmsnr)
confint(bmsnr)

# model spectral correlation
scm <- lmer(spectral.correlation ~  # response
              duration + harmonics + fm + am + # signal features 
              + Temperatura + Humedad + # abiotic features
              distance + Vegetacion + Transecto  + distance * Vegetacion +        # habitat features
              (1 |  signal.type), 
            data = degrd,
            REML = FALSE)

# summary(xcm)

ddsc <- dredge(scm)

subset(ddsc, delta < 4)

#'Best' model
bm <- (get.models(ddsc, 1)[[1]])
summary(bm)
confint(bm)





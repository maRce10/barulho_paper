warbleR_options(wav.path = "./data/raw/tlalpan_data", collevels = seq(-60, 0, 5))

check_wavs()

metadat <- readxl::read_excel("./data/raw/tlalpan_data/base_datos_regrabados_tlalpan.xlsx")

metadat$sound.files <- paste0(metadat$sound.files, ".wav")


all(file.exists(file.path("./data/raw/tlalpan_data", metadat$sound.files)))

master.sf <- read.csv("./data/raw/tlalpan_data/master_annotations.csv", stringsAsFactors = FALSE)

master.sf$sound.files <- "master.wav"

master.sf <- master.sf[c(1:961, nrow(master.sf)), ]

# infer original frequency
possb_freqs <- seq(0.5, 10, length.out = 20)
# 
# master.sf$freq <- sapply(1:nrow(master.sf), function(x)
#   possb_freqs[which.min(abs((master.sf$top.freq[x] + master.sf$bottom.freq[x]) / 2 - possb_freqs))]  
# )

freq_contour <- freq_ts(master.sf, length.out = 20, parallel = 22, img = FALSE, path = "./data/raw/tlalpan_data", pb = FALSE)


master.sf$mean.freq <- rowMeans(freq_contour[, 3:4])
master.sf$frequency <- sapply(master.sf$mean.freq, function(x)
    possb_freqs[which.min(abs(x - possb_freqs))]  
)

# sd(table(master.sf$freq))
sd(table(master.sf$frequency))

master.sf$sound.id <- gsub(".wav", "", master.sf$orig.sound.file)
master.sf$sound.id <- gsub("0.1_", "dur:0.1;", master.sf$sound.id)
master.sf$sound.id <- gsub("0.2_", "dur:0.2;", master.sf$sound.id)

master.sf <- master.sf[grep("no.harm|marker", master.sf$sound.id), ]
master.sf$sound.id <- gsub("_no.harm", "", master.sf$sound.id)
master.sf$sound.id <- gsub("no.am_", "flat;", master.sf$sound.id)
master.sf$sound.id <- gsub("BB_", "fm;", master.sf$sound.id)
master.sf$sound.id <- gsub("pure.tone_", "tonal;", master.sf$sound.id)
master.sf$sound.id <- gsub("am_", "am;", master.sf$sound.id)
master.sf$sound.id <- paste0("freq:", master.sf$frequency, ";", master.sf$sound.id)

master.sf$sound.id <- sapply(strsplit(master.sf$sound.id, ";"),  function(x) paste(x[-length(x)], collapse = ";"))

master.sf$sound.id[2:(nrow(master.sf) -1)] <- sapply(strsplit(master.sf$sound.id[2:(nrow(master.sf) -1)], ";"),  function(x) paste(x[c(2, 1, 3, 4)], collapse = ";"))

master.sf$num <- 1

for(i in 2:nrow(master.sf))
    master.sf$num[i] <- sum(master.sf$sound.id[1:i] == master.sf$sound.id[i])

master.sf$sound.id <- paste(master.sf$sound.id, master.sf$num, sep = "_")
grep("dur:0.1;freq:3.5;tonal;am", master.sf$sound.id, value = T)

master.sf$sound.id[1] <- "start_marker"
master.sf$sound.id[nrow(master.sf)] <- "end_marker"


master.sf$frequency <- master.sf$orig.sound.file <- master.sf$mean.freq <- master.sf$num  <- master.sf$freq <- NULL 

write.csv(master.sf, "/home/m/Dropbox/Projects/barulho_paper/data/raw/tlalpan_data/curated_master_annotations.csv", row.names = FALSE)

treat <- sapply(strsplit(master.sf$sound.id, "_"),  function(x) paste(x[-length(x)], collapse = "_"))

mean(table(treat[!treat %in% c("end", "start")]))

(length(treat) - 2) / (length(unique(treat)) - 2)

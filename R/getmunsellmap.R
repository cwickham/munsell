### explore the mapping 
library(colorspace)
col.map <- read.table("data/real.dat",  header = TRUE)

#convert to XYZ
col.map <- within(col.map, {
	Y <- Y/100
	X <- x * Y / y
	Z <- ((1 - x -y) * Y) / y
})
cols <- c("R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP")
ints <- seq(2.5, 10, 2.5)

col.map$h <- factor(col.map$h,  levels = paste(rep(ints, 10), 
  rep(cols, each = 4), sep = ""))

col.map$hex <- hex(XYZ(100 * as.matrix(col.map[, c("X", "Y", "Z")])))

grey.map <- read.table("data/greys.dat",  header = TRUE)
grey.map$hex <-  hex(RGB(as.matrix(1/255 * grey.map[, c("r", "b", "g")])))

munsell.map <- rbind(grey.map[, c("h", "C", "V", "hex")], 
  col.map[, c("h", "C", "V", "hex")])
names(munsell.map) <- c("hue", "chroma", "value", "hex")
munsell.map$name <- paste(munsell.map$hue, " ", munsell.map$value, "/", munsell.map$chroma,  sep = "") 
munsell.map$name[is.na(munsell.map$hex)] <- NA

not.miss <- subset(munsell.map, !is.na(hex))
not.miss <- cbind(not.miss, as(hex2RGB(not.miss$hex), "LUV")@coords)
munsell.map <- merge(munsell.map, not.miss,  all.x = TRUE)
munsell.map[munsell.map$name == "N 0/0" & !is.na(munsell.map$name), 
  c("L", "U", "V")] <- c(0, 0, 0)

save(munsell.map,  file  = "data/munsell_map.rdata")


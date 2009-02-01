### explore the mapping 
library(colorspace)
col.map <- read.table("real.dat",  header = TRUE)

#convert to XYZ
col.map <- within(col.map, {
	Y <- Y/100
	X <- x * Y / y
	Z <- ((1 - x -y) * Y) / y
})
cols <- c("R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP")
ints <- seq(2.5, 10, 2.5)

col.map$h <- factor(col.map$h,  levels = paste(rep(ints, 10), rep(cols, each = 4), sep = ""))

col.map$hex <- hex(XYZ(100 * as.matrix(col.map[, c("X", "Y", "Z")])))

grey.map <- read.table("greys.dat",  header = TRUE)
grey.map$hex <-  hex(RGB(as.matrix(1/255 * grey.map[, c("r", "b", "g")])))

munsell.map <- rbind(grey.map[, c("h", "C", "V", "hex")], col.map[, c("h", "C", "V", "hex")])
names(munsell.map) <- c("hue", "chroma", "value", "hex")
munsell.map$name <- paste(munsell.map$hue, " ", munsell.map$value, "/", munsell.map$chroma,  sep = "") 
munsell.map$name[is.na(munsell.map$hex)] <- NA

save(munsell.map,  file  = "munsell_map.rdata")

text.colour <- merge(subset(munsell.map, hue == "N"), munsell.map["value"]

qplot(hue,1,data = subset(munsell.map, chroma == 6 & value == 6), fill = hex, geom = "tile") + scale_fill_identity()
qplot(hue,1,data = subset(munsell.map, chroma == 6 & value == 6), fill = hex, geom = "tile") + scale_fill_identity() + coord_polar()

qplot(hue, chroma, data = subset(munsell.map, value == 6 & hue != "N"), fill = hex, geom = "tile") + scale_fill_identity()+ coord_polar()

qplot(hue, chroma, data = munsell.map, fill = hex, geom = "tile") + scale_fill_identity()+ coord_polar() +facet_wrap(~ value)



qplot(chroma, value, data = subset(munsell.map, hue == "5Y"), fill = hex, geom = "tile") + scale_fill_identity()

qplot(chroma, value, data = munsell.map, fill = hex, geom = "tile") + scale_fill_identity() + facet_wrap(~ hue)

qplot(hue, value, data = subset(munsell.map, chroma == 12), fill = hex, geom = "tile") + scale_fill_identity() 


# functions needed
# takes munsell specification and produces hex code
# 	easy for integer values - interpolate for others?
#	need something since technically 10 steps for hue?????	

munsell <- function(hue, value, chroma){
	selected <- data.frame(hue = hue,  value = value,  chroma = chroma)
	hex.vals <- merge(selected, munsell.map, all.x = TRUE)$hex
	if (any(is.na(hex.vals))) warning("Some colours were mispecified or 
		not available (in RGB) and assigned NA")
	return(hex.vals)
}

#examples 
munsell("5Y", 4, 8)
plot(1, 1, col = munsell("5Y", 4, 6), pch = 19, cex = 10)

munsell.text <- function(colour.spec){
	hcv <- unlist(strsplit(strsplit(colour.spec, " ")[[1]],  "/"))
	munsell(toupper(hcv[1]), as.numeric(hcv[2]), as.numeric(hcv[3]))
}

#example
munsell.text("5Y 4/8")

# wrappers for producing plots of slices

# different plots
# all colours - horizontal slices facetted by value
#	- vertical slices factted by hue
#	- vertical with a pane for each complimentary pair
# 	- vertical faceted by chroma

hue.slice <- function(hue.name = "all"){
	if (hue.name == "all") {
		return(ggplot(aes(x = factor(chroma), y = factor(value)), 
			data = munsell.map) +
		 	geom_tile(aes(fill = hex), colour = "white") +
		 	scale_fill_identity() + 
			opts(panel.grid.major = theme_blank(), 
				panel.grid.minor = theme_blank(), 
				panel.background =theme_blank()) +
			scale_x_discrete("Chroma") + 
			scale_y_discrete("Value") +
			facet_wrap(~ hue))
	}
	else {
		if ( ! hue.name %in% munsell.map$hue) stop("invalid hue name")
	ggplot(aes(x = factor(chroma), y = factor(value)), 
		data = subset(munsell.map, hue == hue.name)) +
	 	geom_tile(aes(fill = hex), colour = "white", size = 1) +
	 	scale_fill_identity() + 
		opts(panel.grid.major = theme_blank(), 
			panel.grid.minor = theme_blank(), 
			panel.background =theme_blank()) +
		scale_x_discrete("Chroma") + 
		scale_y_discrete("Value")
	}
}

#example
hue.slice("5Y")
hue.slice()

value.slice <- function(value.name){
	if ( ! value.name %in% munsell.map$value) stop("invalid Value")
	ggplot(aes(x = hue, y = chroma), 
		data = subset(munsell.map, value == value.name & hue != "N")) +
	 	geom_tile(aes(fill = hex), colour = "white") +
	 	scale_fill_identity() + 
		coord_polar() +
		opts(panel.grid.major = theme_blank(), 
			panel.grid.minor = theme_blank(), 
			panel.background = theme_blank()) +
		scale_x_discrete("Hue") + 
		scale_y_continuous("Chroma") 
}

value.slice(4)

#this one is hard to picture - think about wrapping plot into cylinder
chroma.slice <- function(chroma.name){
	if ( ! chroma.name %in% munsell.map$chroma) stop("invalid Chroma")
	ggplot(aes(x = hue, y = value), 
		data = subset(munsell.map, chroma == chroma.name & hue != "N")) +
	 	geom_tile(aes(fill = hex), colour = "white") +
	 	scale_fill_identity() + 
		opts(panel.grid.major = theme_blank(), 
			panel.grid.minor = theme_blank(), 
			panel.background = theme_blank(), 
			axis.text.x=theme_text(angle = 45,hjust=1), 
			aspect.ratio = 1/4) +
		scale_x_discrete("Hue") + 
		scale_y_continuous("Value")	
}

chroma.slice(8)
chroma.slice(18)

complement.slice <- function(hue.name){
	if ( ! hue.name %in% munsell.map$hue) stop("invalid hue name")
	hues <- levels(munsell.map$hue)[-1]
	index <- which(hues == hue.name)
	complement <- hues[(index + 20) %% 40]
	munsell.sub <- subset(munsell.map, 
		hue == "N" | hue == hue.name | hue == complement)
	munsell.sub <- within(munsell.sub, {
		chroma[hue == complement] <- -chroma[hue == complement]
		hue <- factor(hue, levels = c(complement, "N", hues[index]))
		})
	
	ggplot(aes(x = factor(chroma), y = value), 
		data = munsell.sub) + 
	 	geom_tile(aes(fill = hex), colour = "white", size = 1) +
	 	scale_fill_identity() + 
		opts(panel.grid.major = theme_blank(), 
			panel.grid.minor = theme_blank(), 
			panel.background =theme_blank()) +
		scale_x_discrete("Chroma") + 
		scale_y_continuous("Value") +
		facet_grid(. ~ hue,  scales = "free_x",  space = "free") 
}

complement.slice("5Y")
complement.slice("5PB")

munsell.all <- function(){
	qplot(chroma, value, data = munsell.map, fill = hex, geom = "tile") + scale_fill_identity() + facet_wrap(~ hue)	
}

library("ggplot2")
load("munsell_map.rdata")

# takes hue (character), value(numeric) and chroma(numeric)
# outputs hex
munsell <- function(hue, value, chroma){
	selected <- data.frame(hue = hue,  value = value,  chroma = chroma)
	hex.vals <- merge(selected, munsell.map, all.x = TRUE)$hex
	if (any(is.na(hex.vals))) warning("Some colours were mispecified or 
		not available (in RGB) and assigned NA")
	return(hex.vals)
}

#take standard munsell color i.e. "5PB 5/10"
munsell.text <- function(colour.spec){
	hcv <- unlist(strsplit(strsplit(colour.spec, " ")[[1]],  "/"))
	munsell(toupper(hcv[1]), as.numeric(hcv[2]), as.numeric(hcv[3]))
}

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

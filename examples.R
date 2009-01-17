munsell("5Y", 5, 6)
plot(1, 1, col = munsell("5Y", 4, 6), pch = 19, cex = 10)

munsell.text("5Y 5/6")

hue.slice("5Y")
hue.slice()

value.slice(4)

chroma.slice(8)
chroma.slice(18)

complement.slice("5Y")
complement.slice("5PB")

rgb2munsell(0.1, 0.1, 0.3)
rgb2munsell(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))

plot.closest(0.1, 0.1, 0.3)
plot.closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
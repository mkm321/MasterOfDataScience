birthwt = read.csv("datasets/birthwt.csv")
with(birthwt, qqPlot(bwt[smoke=="yes"], main="Smokers"))
with(birthwt, qqPlot(bwt[smoke=="no"], main="Non Smokers"))

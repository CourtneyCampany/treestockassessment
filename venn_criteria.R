###keep track of specific data findings for tree stock balance
library(VennDiagram)

venn_dat <- read.csv("data/testing_criteria_venn.csv")
venn_dat <- read.csv("data/testing_criteria_venn2.csv")

nrow_dat <- nrow(venn_dat)

##number of rows for each country where criteria == 1
usa_no <- nrow(subset(venn_dat, usa == 1))
aus_no <- nrow(subset(venn_dat, australia == 1))
eur_no <- nrow(subset(venn_dat, europe == 1))

##areas of 2 group overlap
usa_aus <- nrow(subset(venn_dat, usa == 1 & australia == 1))
usa_eur <- nrow(subset(venn_dat, usa == 1 & europe == 1))
eur_aus <- nrow(subset(venn_dat, europe == 1 & australia == 1))

##areas of 3 group overlap
eur_aus_usa <- nrow(subset(venn_dat, europe == 1 & australia == 1 & usa == 1))


##plot venn
draw.single.venn(area = nrow_dat, category = "Australia", alpha=0.5,  fill= "forestgreen", lty="blank")

windows(12,8)
grid.newpage()
draw.pairwise.venn(area1 = aus_no, area2 = usa_no, cross.area = usa_aus, category = c("Australia", "Usa"))


###lets plot non=overlapped circles to show the number of criteria used ###cant do with 3
###then use Venn to see degree of overlap


##colors
eurcol <- "#3B9AB2"
auscol <- "#05943f"
usacol <- "#cc0000"

#aus vs usa
grid.newpage()
draw.pairwise.venn(area1 = aus_no, area2 = usa_no, cross.area = 0, category = c("Australia",
          "USA"), lty = rep("solid", 2), fill = c(auscol, usacol),cat.cex = rep(1.5, 2), cex=1.5,
          alpha = rep(0.8, 2), cat.pos = c(0, 180), euler.d = TRUE, sep.dist = 0.03,
           rotation.degree = 45)

#aus vs europe
grid.newpage()
draw.pairwise.venn(area1 = aus_no, area2 = eur_no, cross.area = 0, category = c("Australia",
                   "Europe"), lty = rep("solid", 2), fill = c(auscol, eurcol), cat.cex = rep(1.5, 2), cex=1.5,
                   alpha = rep(0.8, 2), cat.pos = c(0, 180), euler.d = TRUE, sep.dist = 0.03,
                   rotation.degree = 45)


##full venn
grid.newpage()
draw.triple.venn(area1 = aus_no, area2 = usa_no, area3 = eur_no, n12 = usa_aus, n23 = usa_eur, n13 = eur_aus, 
                 n123 = 1, category = c("Australia", "USA", "Europe"), lty = "blank", 
                 fill = c("yellow", "red", "blue"))



####different package
# install.packages('venneuler')
library(venneuler)

# Using d[-1] to remove 'Participant' column, venneuler doesn't need it.
plot(venneuler(d[-1]))


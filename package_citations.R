

# packages
installed.packages()->inst_pack

pkg_cit=list()
for pkg in rownames(inst_pack)
  pkg_cit=toBibtex(citation(pkg))

#manual
# R
citation()

citation("tidyverse")
citation("caret")
citation("prospectr")
citation("simplerspec")
citation("randomForest")
citation("e1071")
citation("pls")
citation("corrplot")
citation("Cubist")
citation("devtools")
citation("roxygen2")
citation("ggpubr")
citation("opusreader2")
citation("mdatools")
citation("mlr3")
citation("ggtern")
citation("soiltexture")
citation("resemble")



# Used in TUBAFsoilPkg

print(citation("Cubist"),bibtex=T)->tmp
c(
citation("DescTools"),
citation("caret"),
citation("e1071"),
citation("ggpubr"),
citation("lifecycle"),
citation("plotly"),
citation("pls"),
citation("progress"),
citation("prospectr"),
citation("resemble"),
citation("simplerspec"),
citation("tidyverse"),
citation("useful"),
citation("utils"),
citation("devtools"),
citation("pkgdown"),
citation("roxygen2")
)%>%toBibtex()



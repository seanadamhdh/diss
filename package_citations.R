

# packages
installed.packages()->inst_pack

pkg_cit=c()
for (pkg in rownames(inst_pack)){
  pkg_cit=c(pkg_cit,citation(pkg))
}

toBibtex(citation(pkg))
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


###################

# Get all installed packages
pkgs <- installed.packages()[, "Package"]


#manual selection
pkgs = unique(
  c(
    #TUBAFsoilFunctions
    "DescTools",
    "caret",
    "e1071",
    "ggpubr",
    "lifecycle",
    "plotly",
    "pls",
    "progress",
    "prospectr",
    "resemble",
    "simplerspec",
    "tidyverse",
    "useful",
    "utils",
    "devtools",
    "pkgdown",
    "roxygen2",
    #rest
    "caret",
    "corrplot",
    "Cubist",
    "DescTools",
    "devtools",
    "dplyr",
    "e1071",
    "GGally",
    "ggcorrplot",
    "ggExtra",
    "ggh4x",
    "ggplot2",
    "ggpubr",
    "ggsoiltexture",
    "ggstatsplot",
    "ggtern",
    "ggthemes",
    "kohonen",
    "lifecycle",
    "lubridate",
    "mdatools",
    "mlbench",
    "mlr3",
    "mlr3cluster",
    "mlr3data",
    "mlr3extralearners",
    "mlr3filters",
    "mlr3fselect",
    "mlr3hyperband",
    "mlr3inferr",
    "mlr3learners",
    "mlr3mbo",
    "mlr3measures",
    "mlr3misc",
    "mlr3pipelines",
    "mlr3tuning",
    "mlr3tuningspaces",
    "mlr3verse",
    "mlr3viz",
    "opusreader2",
    "plotly",
    "pls",
    "progress",
    "prospectr",
    "randomForest",
    "RColorBrewer",
    "readr",
    "readxl",
    "resemble",
    "pkgdown",
    "roxygen2",
    "rmarkdown",
    "shiny",
    "shinyjs",
    "soiltexture",
    "stringr",
    "tidyverse",
    "useful",
    "usethis",
    "viridis"
  )
)

# Filter out base packages (optional)
base_pkgs <- rownames(installed.packages(priority = "base"))
pkgs <- setdiff(pkgs, base_pkgs)

# Create BibTeX entries for each package
bib_entries <- lapply(pkgs, function(pkg) {
  tryCatch(toBibtex(citation(pkg)), error = function(e) NULL)
})

# Remove NULLs (packages without citation info)
bib_entries <- Filter(Negate(is.null), bib_entries)

# Flatten the list in case of multiple citations per package
bib_text <- paste(unlist(lapply(bib_entries, as.character)), collapse = "\n\n")



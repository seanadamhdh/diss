

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
    
  # OSSL Model local
    
  "mlr3measures",#mlr3measures_0.5.0
    
  "mlr3misc",#mlr3misc_0.11.0
    
  "paradox",#paradox_0.11.1
    
  "mlr3",#mlr3_0.16.0
    
  "mlr3data",#mlr3data_0.6.1
    
  "mlr3filters",#mlr3filters_0.7.1
    
  "bbotk",#bbotk_0.7.2
    
  "mlr3fselect",#mlr3fselect_0.11.0
    
   "mlr3hyperband",#mlr3hyperband_0.4.4
   
   
  "mlr3learners",#mlr3learners_0.5.6
  "mlr3mbo",#mlr3mbo_0.1.2
    
  "mlr3pipelines",#mlr3pipelines_0.5.0
    
  "mlr3tuning",#mlr3tuning_0.18.0
    
  "mlr3tuningspaces",#mlr3tuningspaces_0.4.0
    
  "mlr3viz",#mlr3viz_0.6.1
    
  "matrixStats",#matrixStats_0.63.0
    
    
  "mlr3verse",#",#mlr3verse_0.2.8
    
    
   "mlr3extralearners",#mlr3extralearners_v0.6.0
    
  "mlr3cluster",#mlr3cluster_0.1.8
 
    
  "mlbench",
  "mlr3",
  #"mlr3cluster",
  #"mlr3data",
  #"mlr3extralearners",
  #"mlr3filters",
  #"mlr3fselect",
  #"mlr3hyperband",
  "mlr3inferr",
  #"mlr3learners",
  #"mlr3mbo",
  #"mlr3measures",
  #"mlr3misc",
  #"mlr3pipelines",
  #"mlr3tuning",
  #"mlr3tuningspaces",
  #"mlr3verse",
  #"mlr3viz",
    
    
    
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
    "ChemometricsWithR",
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
    "remotes",
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
    "viridis",
    "renv"
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
bib_text <- paste(unlist(lapply(bib_entries, as.character)), collapse = "\n")




write.table(bib_text,"C:/Users/adam/Desktop/UNI/PhD/DISS/tables/Rpackage_citations.txt",
            row.names = F,col.names = F)

# tidyverse::tidyverse_packages()%>%dput, excl. tidyverse itself
pkgs[which(!pkgs%in%c("broom", "conflicted", "cli", "dbplyr", "dplyr", "dtplyr", 
                      "forcats", "ggplot2", "googledrive", "googlesheets4", "haven", 
                      "hms", "httr", "jsonlite", "lubridate", "magrittr", "modelr", 
                      "pillar", "purrr", "ragg", "readr", "readxl", "reprex", "rlang", 
                      "rstudioapi", "rvest", "stringr", "tibble", "tidyr", "xml2" 
                      ))]%>%sort%>%dput

pkgs[which(pkgs%in%c("broom", "conflicted", "cli", "dbplyr", "dplyr", "dtplyr", 
                      "forcats", "ggplot2", "googledrive", "googlesheets4", "haven", 
                      "hms", "httr", "jsonlite", "lubridate", "magrittr", "modelr", 
                      "pillar", "purrr", "ragg", "readr", "readxl", "reprex", "rlang", 
                      "rstudioapi", "rvest", "stringr", "tibble", "tidyr", "xml2" 
))]%>%sort%>%dput


data.frame(
  c("bbotk", "caret", "corrplot", "Cubist", "DescTools", "devtools", 
    "e1071", "GGally", "ggcorrplot", "ggExtra", "ggh4x", "ggpubr", 
    "ggsoiltexture", "ggstatsplot", "ggtern", "ggthemes", "kohonen", 
    "lifecycle", "matrixStats", "mdatools", "mlbench", "mlr3", "mlr3cluster", 
    "mlr3data", "mlr3extralearners", "mlr3filters", "mlr3fselect", 
    "mlr3hyperband", "mlr3inferr", "mlr3learners", "mlr3mbo", "mlr3measures", 
    "mlr3misc", "mlr3pipelines", "mlr3tuning", "mlr3tuningspaces", 
    "mlr3verse", "mlr3viz", "opusreader2", "paradox", "pkgdown", 
    "plotly", "pls", "progress", "prospectr", "randomForest", "RColorBrewer", 
    "remotes", "renv", "resemble", "rmarkdown", "roxygen2", "shiny", 
    "shinyjs", "simplerspec", "soiltexture", "tidyverse", "useful", 
    "usethis", "viridis"))%>%write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/Rpackages.csv",col_names = F)

if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")

# The data can be downloaded from the INEC's url at https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/ENSANUT/ENSANUT_2018/BDD_ENSANUT_2018_STATA_.zip
# We chose to upload the zip file containing the datasets to github in case the INEC takes 
# down the url to the datasets

options(timeout=600) # we change the download timeout time to 600

# location of the data on github
url <- "https://github.com/aquijanoruiz/PMB/raw/main/data/ENSANUT_2012_2018.zip"
# creates a temporary directory
td <- tempdir()
# creates a placeholder file
tf <- tempfile(tmpdir=td, fileext = ".zip")
# downloads the data into the placeholder file
download.file(url,tf)

# shows the files contained inside the zip file: unzip(tf, list=TRUE)$Name
people_2012_f_name <- unzip(tf, list=TRUE)$Name[1]
people_2012_f_path <- file.path(td, people_2012_f_name)

people_2018_f_name <- unzip(tf, list=TRUE)$Name[2] # Ensanut 2018
people_2018_f_path <- file.path(td, people_2018_f_name)

unzip(tf, exdir=td, overwrite=TRUE)

# loads the dta files
ensanut_2012 <- read.dta(people_2012_f_path, convert.factors = FALSE)
key_ensanut_2012 <- data.frame(variable = names(ensanut_2012),label = attr(ensanut_2012,"var.labels"))

ensanut_2018 <- read.dta13(people_2018_f_path, convert.factors = FALSE)
key_ensanut_2018 <- data.frame(variable = names(ensanut_2018), label = attr(ensanut_2018,"var.labels"))

# cantons data
pmb_cantons_date <- read.csv("https://raw.githubusercontent.com/aquijanoruiz/PMB/main/data/PMB_cantons.csv")


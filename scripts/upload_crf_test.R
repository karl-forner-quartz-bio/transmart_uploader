library(devtools)
devtools::load_all('TransmartUploader')
library("car")
library("brew")
 

path <- "test_data/diagnos_1.tsv"
upload_diagnos(path,transmart_path = 'Sep/Test_Upload_Diagnos_IC', study_type= "Inception", tissue_type = "Blood")

path <- "test_data/diagnos.tsv"
upload_diagnos(path,transmart_path = 'Sep/Test_Upload_Diagnos_SC', study_type= "Cross_Sectional", tissue_type = "Blood")
 
path <- "test_data/demog.txt"
upload_demog(path,transmart_path = 'Sep/Test_Upload_Demog_SC', study_type= "Cross_Sectional", tissue_type = "Blood")
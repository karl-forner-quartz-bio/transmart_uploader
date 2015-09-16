library(devtools)
devtools::load_all('TransmartUploader')


path <- "test_data/demog_IC.txt"
upload_demog(path,transmart_path = 'Inception/CRF/Demog', study_type= "Inception", tissue_type = "Blood")

path <- "test_data/diagnos_IC.tsv"
upload_diagnos(path,transmart_path = 'Inception/CRF/Diagnos', study_type= "Inception", tissue_type = "Blood")

path <- "test_data/diagnos_CS.tsv"
upload_diagnos(path,transmart_path = 'Cross_Sectional/CRF/Diagnos', study_type= "Cross_Sectional", tissue_type = "Blood")
 
path <- "test_data/demog_CS.txt"
upload_demog(path,transmart_path = 'Cross_Sectional/CRF/Demog', study_type= "Cross_Sectional", tissue_type = "Blood")

devtools::load_all('TransmartUploader')



study_type = "cross_sectional"
DB ='db/db_clinical.db'
upload_ecrf(DB, transmart_path = 'Cross_Sectional/Clinical_Data', study_type = "cross_sectional")

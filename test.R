library(devtools)
document('TransmartUploader')
check_man('TransmartUploader')

# Sys.setenv(TRANSMART_DB = 'pyro@5432')

test('TransmartUploader', 'etl_internals')
test('TransmartUploader', 'fam')
test('TransmartUploader', 'input_data')
test('TransmartUploader', 'mapping')
test('TransmartUploader', 'run_tm_etl')
test('TransmartUploader', 'sample')
test('TransmartUploader', 'upload_clinical_data')
test('TransmartUploader', 'utils')
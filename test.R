library(devtools)
document('TransmartUploader')
check_man('TransmartUploader')


test('TransmartUploader', 'etl_internals')
test('TransmartUploader', 'fam')
test('TransmartUploader', 'mapping')
test('TransmartUploader', 'run_tm_etl')
test('TransmartUploader', 'sample')
test('TransmartUploader', 'utils')
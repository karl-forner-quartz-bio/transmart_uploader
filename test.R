library(devtools)
document('TransmartUploader')
check_doc('TransmartUploader')

test('TransmartUploader', 'run_tm_etl')
test('TransmartUploader', 'etl_internals')
test('TransmartUploader', 'sample')
test('TransmartUploader', 'utils')
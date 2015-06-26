library(devtools)
document('TransmartUploader')
load_all('TransmartUploader')

test('TransmartUploader')
test('TransmartUploader', 'fam')
test('TransmartUploader', 'map')
test('TransmartUploader', 'etl')
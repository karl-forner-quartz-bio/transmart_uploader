

devtools::load_all('TransmartUploader')
data <- fetch_test4_sample_data()
dfs <- data[c("Test4_1.txt", "Test4_2.txt")]
res <- run_tm_etl(data[['']])

# 99_download_file_error.R
# download.file not working, tried different method and mode options for download.file
# no help here https://stackoverflow.com/questions/72224687/unzipping-gz-file-in-r
# will need to manually download zipped files
# May 2022

library(R.utils) # for gunzip

# unzip from pubmed
filez = "pubmed22n1148.xml.gz"
url = paste('ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/', filez, sep='')
zipped.file = download.file(url = url, method='auto', destfile=filez) 
(filez) 


# This writes and reads encrypted data frames using the digest function
#Code taken from https://stackoverflow.com/questions/25318800/how-do-i-read-an-encrypted-file-from-disk-with-r

makekey32.aes <- function(seedVAL){

  set.seed(seedVAL)
  key <- as.raw(sample(1:32,32))
  return(key)
}


write.aes <- function(df,filename, key) {
  require(digest)
  zz <- textConnection("out","w")
  write.csv(df,zz, row.names=F)
  close(zz)
  out <- paste(out,collapse="\n")
  raw <- charToRaw(out)
  raw <- c(raw,as.raw(rep(0,16-length(raw)%%16)))
  aes <- AES(key,mode="ECB")
  aes$encrypt(raw)
  writeBin(aes$encrypt(raw),filename)
}
# read encypted data frame from file
read.aes <- function(filename,key) {
  require(digest)
  dat <- readBin(filename,"raw",n=1000)
  aes <- AES(key,mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw>0])
  read.csv(text=txt)
}


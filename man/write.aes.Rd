\name{write.aes}
\alias{write.aes}
\title{Write AES Encrypted df}
\description{
Writes an encrypted data frame to file using AES encryption.
}
\usage{
write.aes(df,filename, key)
}
\arguments{
  \item{df}{A data frame to be encrypted.}
  \item{filename}{The name of the data file to be written out.}
  \item{key}{The 32 bit encryption key to use in encryption.}

}
\examples{
write.aes(df, "filename.dat", key)

}


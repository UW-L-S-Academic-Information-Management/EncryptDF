\name{read.aes}
\alias{read.aes}
\title{Read AES Encrypted df}
\description{
Reads in an AES encrypted data frame from file.
}
\usage{
read.aes(filename, key)
}
\arguments{
  \item{filename}{The name of the data file to be read in.}
  \item{key}{The 32 bit encryption key used to encrypt the file.}

}
\examples{
df <- read.aes("filename.dat", key)

}



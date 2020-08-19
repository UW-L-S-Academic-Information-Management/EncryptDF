\name{makekey32.aes}
\alias{makekey32.aes}
\title{Make 32 bit key}
\description{
Creates a 32 bit raw key for encryption.
}
\usage{
makekey32.aes(seedVal)
}
\arguments{
  \item{seedVal}{A integer value used to seed the random creation of an encryption key.}

\details{This function creates a 32 bit key that can be used to encrypt a data frame.

}
\examples{
key <- make32.aes(12)

}


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



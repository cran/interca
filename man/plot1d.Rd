\name{plot1d}
\alias{plot1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates interpretive axis
}
\description{
The function takes the result of the interca() function and the number of a factorial axis and returns the corresponding interpretive axis with the most significant points.
}
\usage{
plot1d(res, dim = 1)
}
\arguments{
  \item{res}{
The result of the execution of the interca() function.
}
  \item{dim}{
The number of the interpretive axis to be printed.
}
}
\details{
The interca() function must be executed first in order to use the function.
}
\value{
The function returns a \code{ggplot2} scatterplot that corresponds to a single interpretive dimension obtained via \code{interca()}.
}
\seealso{
interca
}
\examples{
# Set the seed to ensure reproducibility
set.seed(123)
# Create three categorical variables
X1 <- sample(c("X1_1", "X1_2"), size = 200, replace = TRUE)
X2 <- sample(c("X2_1", "X2_2", "X2_3"), size = 200, replace = TRUE)
X3 <- sample(c("X3_1", "X3_2", "X3_3", "X3_4"), size = 200, replace = TRUE)
# the resulting data frame
df <- data.frame(cbind(X1,X2,X3))
# convert to factors
df$X1 <- factor(df$X1)
df$X2 <- factor(df$X2)
df$X3 <- factor(df$X3)

res<-interca(df,5)
plot1d(res,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

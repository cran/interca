\name{plot2d}
\alias{plot2d}
\title{
Interpretive 2D plane
}
\description{
The function takes as parameters the result of the interca() function, the interpretive axis that will form the x axis, and the interpretive axis that will form the y axis. The function returns the interpretive plane formed by the above.
}
\usage{
plot2d(res, dim1 = 1, dim2 = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
The result of the interca() function.
}
  \item{dim1}{
The number of the interpretive axis that will form the x axis of the interpretive plane.
}
  \item{dim2}{
The number of the interpretive axis that will form the y axis of the interpretive plane.
}
}
\value{
The function returns a \code{ggplot2} scatterplot that corresponds to a 2D interpretive plane obtained via \code{interca()}.
}
\seealso{
interca
}
\examples{
#For the example, we will use the wg93 data frame from the ca package.
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

res <- interca(df,5)
plot2d(res,1,2)
}

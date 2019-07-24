df.all1 <- combine.two.df(data.from.mocap1$df.all, data.from.mocap2$df.all)
df.all1 <- combine.two.df(df.all1, data.from.mocap3$df.all)


class.names.list1 <- c(data.from.mocap1$class.names.list, data.from.mocap2$class.names.list)
class.names.list3 <- c(class.names.list1, data.from.mocap3$class.names.list)


ge <- generate.eigen(df.all1, 3, FALSE)
eigenvalues <- ge$eigenvalues
eigenvectors <- ge$eigenvectors
mf <- ge$mf
lc <- generate.features.from.eigen(df.all1, eigenvectors, mf)


liczba.wierszy <- length(lc)
liczba.kolumn <- length(eigenvalues)
df <- data.frame(v1 = rep(0, liczba.wierszy))
for (a in 2:(liczba.kolumn))
{
  df[a] <- rep(0, liczba.wierszy)
}
for (a in 1:liczba.wierszy)
{
  df[a,] <-  1/sqrt(eigenvalues) * lc[[a]]
}
variance.exp <- eigenvalues^2/sum(eigenvalues^2)
plot(1-eigenvalues^2/sum(eigenvalues^2))

col <- as.numeric(as.factor(unlist(class.names.list3)))
library("scatterplot3d")
scatterplot3d(df, angle = 55, 
              xlab = 'Dim1 (52.0%)',
              ylab = 'Dim2 (19.1%)',
              zlab = 'Dim3 (5.7%)', 
              color = col,
              pch = col)

legend(5.5,9, legend = levels(as.factor(unlist(class.names.list3))),
       col =  1:12, 
       pch = 1:12,
       cex = 0.8)
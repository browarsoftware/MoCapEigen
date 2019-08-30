#RUN THIS SCRIPT
source("e:\\Publikacje\\MocapEigen\\R\\HelperFunctions.R ")
main.path <- 'e:\\Publikacje\\MocapEigen\\R\\'
#Two-Stage PCA Extracts Spatiotemporal Features for Gait Recognition


list.to.df <- function(my.list)
{
  big.df <- my.list[[1]]
  for (a in 2:length(my.list))
  {
    big.df <- rbind(big.df, my.list[[a]])
  }
  return (big.df)
}

df.to.features.df <- function(df.helper)
{
  how.many.columns <- nrow(df.helper) / samples.count
  rr <- data.frame(c(df.helper[1:samples.count,1], df.helper[1:samples.count,2]))
  
  for (a in 2:how.many.columns)
  {
    rr[,a] <- data.frame(c(df.helper[(((a-1) * samples.count) + 1):(a * samples.count),1], 
                           df.helper[(((a-1) * samples.count) + 1):(a * samples.count),2]))
  }
  return(rr)
}

generate.data.inne <- function(dir_names, path.in, samples.count, reference.data.path, Mean.Face = NULL)
{
  #path.in = path
  #reference.data.path = ref.path
  list.all <- list()
  #path <- paste(path.in, dir_names[1],'\\segmented_bvh\\', sep = '')
  #path1 <- paste(path, 'sample0.bvh', sep = '')
  #print(reference.data.path)
  mocap.ref <- read.mocap(reference.data.path)
  #print('aaaa')
  aa.idx <- 1
  #path1 <- paste(path, 'sample0.bvh', sep = '')
  #mocap.ref <- read.mocap(path1)
  class.names.list <- list()
  all.data.list <- list()
  vector.to.sample <- seq(from=-1, to=1, by=0.001)
  
  #all.mocap.data <- list()
  for (bb in 1:length(dir_names))
  {
    #path <- paste('e:\\mocap_data\\karate\\2016-12-22 ShoriunRiu MP\\evaluation\\hiza_geri_left\\segmented_bvh\\'
    path <- paste(path.in, dir_names[bb],'\\', sep = '')
    
    #path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-12-22 ShoriunRiu MP\\evaluation\\', dir_names[bb],'\\segmented_bvh\\', sep = '')
    #path1 <- paste(path, 'sample0.bvh', sep = '')
    
    
    for (a in 0:9)
    {
      #print(a)
      path2 <- paste(path, 'sample',a, '.bvh', sep = '')
      #print(path2)
      mocap.input <- read.mocap(path2)
      if (FALSE)
      {
        refdata <- mocap.ref$data.frame
        inputdata <- mocap.input$data.frame
        
        inputdataalignment <- rotatedata(inputdata, refdata, "LeftThigh","RightThigh")
        #after following function inputdata and refdata are alignined towards vector LeftThigh - RightThigh
        inputdataalignmentkinematic <- calculate.kinematic(inputdataalignment, bodypartname = "RightFoot")
        refdatakinematic <- calculate.kinematic(refdata, bodypartname = "RightFoot")
        inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, refdatakinematic, limbname = "RightFoot")
        
        mocap.input$data.frame <- inputdataalignmentkinematic
      }
      #plot(mocap.input)
      
      
      helper.data.frame.sampled <- resample.data(mocap.input$data.frame, samples.count)
      #all.mocap.data[[length(all.mocap.data) + 1]] <- helper.data.frame.sampled
      df.all <- generate.features.from.df(helper.data.frame.sampled, mirror)
      
      #plot(mocap.input)
      class.names.list[[length(class.names.list) + 1]] <- dir_names[bb]
      
      all.data.list[[length(all.data.list) + 1]] <- df.all
      
      vv <- df.to.vector(df.all)
      list.all[[aa.idx]] <- vv
      aa.idx <- aa.idx + 1
    }
  }
  df.all <- data.frame('1' = rep(0, length(list.all[[1]])))
  for (a in 1:length(list.all))
  {
    df.all[,a] <- list.all[[a]]
  }
  
  #df.all <- generate.features.from.df(df.all)
  
  if (is.null(Mean.Face))
  {
    #odejmujemy mean face
    Mean.Face <- rowSums(df.all) / ncol(df.all)
  }
  for (a in 1:length(list.all))
  {
    #  df.all[,a] <- df.all[,a] - Mean.Face
  }
  
  return (list(df.all = df.all, class.names.list = class.names.list, mocap.input = mocap.input, Mean.Face = Mean.Face, all.data.list = all.data.list))
}


prepare.classfier.and.data.svm.inne <- function(df1, df2, df3, class.names.vector1, class.names.vector2, class.names.vector3,
                                           number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = 'radial')
{
  my.df1 <- list.to.df(df1)
  my.df2 <- list.to.df(df2)
  my.df3 <- list.to.df(df3)
  df.all12 <- rbind(my.df1, my.df2)
  
  require(FactoMineR)
  pca.res <- PCA(df.all12, scale.unit = FALSE, ncp = 2)
  col.menas.help <- colMeans(df.all12)
  xxx <- pca.res$svd$V
  df.all12 <- pca.res$ind$coord
  #pca.res$ind$coord[1,]
  #(as.matrix(big.df[1,], 28, 1) - colMeans(big.df))%*% xxx
  
  
  df.test <- (as.matrix(my.df3[1,], 1, 28) - col.menas.help)%*% xxx
  for (a in 2:nrow(my.df3))
  {
    df.test <- rbind(df.test, (as.matrix(my.df3[a,], 1, 28) - col.menas.help)%*% xxx)
  }
  #df3 <- df.test
  
  #df.helper <- df.test
  #how.many.columns <- nrow(df.helper) / samples.count
  #rr <- data.frame(c(df.helper[1:samples.count,1], df.helper[1:samples.count,2]))
  #for (a in 2:how.many.columns)
  #{
  #  rr[,a] <- data.frame(c(df.helper[(((a-1) * samples.count) + 1):(a * samples.count),1], 
  #                         df.helper[(((a-1) * samples.count) + 1):(a * samples.count),2]))
  #}
  df.all12 <- df.to.features.df(df.all12)
  df3 <- df.to.features.df(df.test)
  
  
  class.names.list12 <- c(class.names.vector1, class.names.vector2)
  unique.classes <- unique(class.names.list12)
  
  #df.all3 <- data.from.mocap3$df.all
  df3 <- df3[,seq(from=1, to=ncol(df3), by = number.of.add.sub.samples + 1)]
  classifiers <- list()
  for (a in 1:number.of.classfiers)
  {
    #print(a)
    classes.sample <- unique.classes[sample(1:length(unique.classes), number.of.classes.per.features.set, replace = FALSE)]
    columns.to.select <- class.names.list12 %in% classes.sample
    data.to.generate.features <- df.all12[,columns.to.select]
    classifiers[[a]] <- prepare.classifier(data.to.generate.features, number.of.eigen, debug = FALSE)
    classifiers[[a]]$class.names.list12 <- class.names.list12[columns.to.select]
  }
  
  class.names.vector3 <- class.names.vector3[seq(from=1, to=length(class.names.vector3), by = number.of.add.sub.samples + 1)]
  
  for (aa in 1:number.of.classfiers)
  {
    #print(a)
    class1 <- classifiers[[aa]]
    #print(length(class1))
    liczba.wierszy <- length(class1$lc)
    liczba.kolumn <- length(class1$eigenvalues)
    df <- data.frame(v1 = rep(0, liczba.wierszy))
    for (a in 2:(liczba.kolumn))
    {
      df[a] <- rep(0, liczba.wierszy)
    }
    for (a in 1:liczba.wierszy)
    {
      df[a,] <-  1/sqrt(class1$eigenvalues) * class1$lc[[a]]
    }
    library(e1071)
    df$class <- factor(class1$class.names.list12)
    svm.res <- svm(factor(class) ~ ., data = df, scale = FALSE, kernel = kernel)
    classifiers[[aa]]$svm.res <- svm.res
    #class1$svm.res <- svm.res
    #print(length(class1))
    #classifiers[[a]] <- class1
    #classifiers[[a]] <- NULL
  }
  return (list(classifiers = classifiers,
               class.names.vector3 = class.names.vector3,
               df3 = df3))
}

########################################################################


samples.count <- 100
set.seed(1)
mirror = FALSE

        
dir_names <- c('age_uke_left', 'age_uke_right', 'empi_left', 'empi_right', 
               'mae_geri_left', 'mae_geri_right', 
               'gedan_barai_left', 'gedan_barai_right',
               'hiza_geri_left', 'hiza_geri_right',
               'yoko_geri_left', 'yoko_geri_right')

path <- paste(paste(main.path, 'data\\ShoriunRiu 1\\', sep = ''))
#path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 1\\evaluation\\')
ref.path <- paste(path, dir_names[1],'\\','sample0.bvh', sep = '')
data.from.mocap1 <- generate.data.inne(dir_names = dir_names, 
  path.in = path, 
  samples.count = samples.count, 
  reference.data.path = ref.path)

#length(data.from.mocap1$mocap.input.list)
#nrow(data.from.mocap1$mocap.input.list[[2]])
#data.from.mocap1$mocap.input$data.frame
#data.from.mocap1$all.data.list[[1]]


big.df <- data.from.mocap1$all.data.list[[1]]
for (a in 2:length(big.df))
{
  big.df <- rbind(big.df, data.from.mocap1$all.data.list[[a]])
}

if (FALSE)
{
  require(FactoMineR)
  pca.res <- PCA(big.df, scale.unit = FALSE, ncp = 2)
  xxx <- pca.res$svd$V
  pca.res$ind
  pca.res$ind$coord[1,]
}

#df.test <- (as.matrix(big.df[1,], 1, 28) - colMeans(big.df))%*% xxx
#for (a in 2:nrow(big.df))
#{
#  df.test <- rbind(df.test, (as.matrix(big.df[a,], 1, 28) - colMeans(big.df))%*% xxx)
#}

#df.test[1:4,]
#pca.res$ind$coord[1:4,]

#(as.matrix(big.df[1,], 1, 28) - colMeans(big.df))%*% xxx


dir_names <- c('age_uke_left', 'age_uke_right', 
               'empi_left', 'empi_right', 
               'mae_geri_left', 'mae_geri_right', 
               'gedan_barai_left', 'gedan_barai_right',
               'hiza_geri_left', 'hiza_geri_right',
               'yoko_geri_left', 'yoko_geri_right')

path <- paste(paste(main.path, 'data\\ShoriunRiu 2\\', sep = ''))
#path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 2\\evaluation\\')
#ref.path <- paste(path, dir_names[1],'\\segmented_bvh\\','sample0.bvh', sep = '')
data.from.mocap2 <- generate.data.inne(
  dir_names = dir_names, 
  path.in = path, 
  samples.count = samples.count, 
  reference.data.path = ref.path)


dir_names <- c('age_uke_left', 'age_uke_right', 
               'empi_left', 'empi_right',  
               'mae_geri_left', 'mae_geri_right',
               'gedan_barai_left', 'gedan_barai_right',
               'hiza_geri_left', 'hiza_geri_right',
               'yoko_geri_left', 'yoko_geri_right')

path <- paste(paste(main.path, 'data\\ShoriunRiu 3\\', sep = ''))
#path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 3\\evaluation\\')
#ref.path <- paste(path, dir_names[1],'\\segmented_bvh\\','sample0.bvh', sep = '')
data.from.mocap3 <- generate.data.inne(
  dir_names = dir_names, 
  path.in = path, 
  samples.count = samples.count, 
  reference.data.path = ref.path)



#losujemy podzbiór klas, na nim robimy cechy, budujemy dla ka¿dego zestawu cech klasyfikator
prepare.classifier <- function(dftf, number.of.eigen, debug = FALSE)
{
  #25 jest OK
  ge <- generate.eigen(dftf, number.of.eigen, debug)
  eigenvalues <- ge$eigenvalues
  eigenvectors <- ge$eigenvectors
  mf <- ge$mf
  lc <- generate.features.from.eigen(dftf, eigenvectors, mf)
  return (list(eigenvalues = eigenvalues, eigenvectors = eigenvectors, lc = lc, mf = mf))
}


df.all1 <- data.from.mocap1$all.data.list
df.all2 <- data.from.mocap2$all.data.list
df.all3 <- data.from.mocap3$all.data.list

class.names.list1 <- data.from.mocap1$class.names.list
class.names.list2 <- data.from.mocap2$class.names.list
class.names.list3 <- data.from.mocap3$class.names.list


number.of.add.sub.samples = 0 
number.of.classfiers = 1
kernel = 'radial'
number.of.classes.per.features.set <- 10
number.of.eigen <- 40
#number.of.classes.per.features.set
#if (TRUE)

  classfier.and.data <- prepare.classfier.and.data.svm.inne(df1 = df.all1, df2 = df.all2, df3 = df.all3, 
                                                            class.names.vector1 = unlist(class.names.list1),
                                                            class.names.vector2 = unlist(class.names.list2), 
                                                            class.names.vector3 = unlist(class.names.list3), 
                                                      number.of.add.sub.samples = number.of.add.sub.samples, 
                                                      number.of.classfiers = number.of.classfiers, debug = FALSE, kernel = kernel)
  tables.res1 <- classify.and.return.results.svm(classfier.and.data)

#if (FALSE)

  classfier.and.data <- prepare.classfier.and.data.svm(data.from.mocap1$df.all, data.from.mocap3$df.all, data.from.mocap2$df.all, 
                                                       unlist(data.from.mocap1$class.names.list),
                                                       unlist(data.from.mocap3$class.names.list), 
                                                       unlist(data.from.mocap2$class.names.list), 
                                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = kernel)
  tables.res2 <- classify.and.return.results.svm(classfier.and.data)

#if (FALSE)

  classfier.and.data <- prepare.classfier.and.data.svm(data.from.mocap2$df.all, data.from.mocap3$df.all, data.from.mocap1$df.all, 
                                                       unlist(data.from.mocap2$class.names.list),
                                                       unlist(data.from.mocap3$class.names.list), 
                                                       unlist(data.from.mocap1$class.names.list), 
                                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = kernel)
  
  tables.res3 <- classify.and.return.results.svm(classfier.and.data)
  
  #tables.res3 <- classify.and.return.results(classfier.and.data)
  #tables.res3



tables.res <- (tables.res1 + tables.res2 + tables.res3) / 30

name <- paste(main.path, 'results\\','\\Two-Stage PCA svm.kernel=',kernel,
              'samples=', samples.count, 
              'classes.per.features.set=', number.of.classes.per.features.set,
              'number.of.classfiers=', number.of.classfiers,
              'number.of.add.sub.samples=', number.of.add.sub.samples,
              'number.of.eigen=', number.of.eigen,
              'mirror=', mirror, '.txt', sep = '')
write.table(tables.res, name, sep = '\t',
            row.names = TRUE, col.names = TRUE)


ttt <- read.csv("e:\\Publikacje\\MocapEigen\\R\\results\\Multi-segmental svm.kernel=radialsamples=100classes.per.features.set=10number.of.classfiers=1number.of.add.sub.samples=0number.of.eigen=40mirror=FALSE.txt", sep = '\t')
ttt <- read.csv("e:\\Publikacje\\MocapEigen\\R\\results\\Two-Stage PCA svm.kernel=radialsamples=100classes.per.features.set=10number.of.classfiers=1number.of.add.sub.samples=0number.of.eigen=40mirror=FALSE.txt", sep = '\t')
ttt1 <- ttt[1,1]
for (a in 2:nrow(ttt))
{
  ttt1 <- ttt1 + ttt[a,a]
}
ttt1 / nrow(ttt)

data.to.bar <- data.frame(values = c(0.647, 0.628, 0.847, 0.900, 0.911, 0.939),
                          names = c('[11]', '[14]', 'NNg (12; 1; 2; 30f)', 'NNg (10; 150; 0; 25f)', 'Ngg (10; 150; mirroring; 25)', 'SVM (10, 100, no mirroring, 25f)'))

barplot(height = data.to.bar$values, names.arg = data.to.bar$names, legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3))

vv <- c(0.647, 0.628, 0.847, 0.900, 0.911, 0.939)
names(vv) <- c('Algorithm [11]', 'Algorithm [14]', 'NNg (12; 1; 2; 30f)', 'NNg (10; 150; 0; 25f)', 'Ngg (10; 150; mirroring; 25)', 'SVM (10, 100, no mirroring, 25f)')
mycols = c("tan", "orange1", "magenta", "cyan", "red", "sandybrown")
barplot(vv, beside = TRUE, col = mycols, legend = TRUE, ylim = c(0,1), args.legend = list(bty = "n", x = "top", y = "left", ncol = 2),
        names.arg = FALSE)
title(xlab = "Sample Year", ylab = "Abundance")


op <- par(cex = 1)
vv <- c(0.647, 0.628, 0.847, 0.900, 0.911, 0.939)
names(vv) <- c('Algorithm [11]', 'Algorithm [14]', 'NNg (1; 12; 2; 30f)', 'NNg (150; 10; 0; 25f)', 'Ngg (150; 10; mirroring; 25f)', 'SVM (100, 10, no mirroring, 25f)')
mycols = c("tan", "orange1", "magenta", "cyan", "red", "sandybrown")

barplot(vv, beside = TRUE, col = mycols, legend = TRUE, ylim = c(0,1), 
        args.legend = list(x = 7, y = 0.7, ncol = 1, cex = 0.75),
        names.arg = FALSE)
title(xlab = "Classifiers", ylab = "Recogniton rate")
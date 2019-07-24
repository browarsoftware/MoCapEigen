if (!require("RMoCap"))
{
  if (!require("devtools"))
  {
    install.packages("devtools") # if you have not installed "devtools" package
  }
  devtools::install_github("browarsoftware/RMoCap")
}

library("RMoCap")

normalizeVector <- function(vector)
{
  num1 = max(abs(vector))
  vector = vector / num1
  return (vector / ( sqrt(sum(vector^2))))
}

norm_vec <- function(x) sqrt(sum(x^2))

radiansToDegrees <- function (radians)
{
  return (radians * 57.2957795130823)
}

angleBetween <- function(v1, v2)
{
  vector1 <- normalizeVector(v1)
  vector2 <- normalizeVector(v2)
  #print(vector1)
  #print(vector2)
  angle <- 0
  #print(sum(vector1*vector2))
  if (sum(vector1*vector2) >= 0)
  {
    angle = 2 * asin(norm_vec(vector1 - vector2) / 2)
  }
  else
  {
    angle = pi - 2 * asin(norm_vec(-vector1 - vector2) / 2)
  }
  return (radiansToDegrees(angle))
}

calculate.angle <- function(f1, f2, f3, df)
{
  vangl <- list()
  for (v.id in 1:nrow(df))
  {
    v1 <- c(df[v.id,paste(f1, '.Dx', sep ='')], df[v.id,paste(f1, '.Dy', sep ='')], df[v.id,paste(f1, '.Dz', sep ='')])
    v2 <- c(df[v.id,paste(f2, '.Dx', sep ='')], df[v.id,paste(f2, '.Dy', sep ='')], df[v.id,paste(f2, '.Dz', sep ='')])
    v3 <- c(df[v.id,paste(f3, '.Dx', sep ='')], df[v.id,paste(f3, '.Dy', sep ='')], df[v.id,paste(f3, '.Dz', sep ='')])
    vangl[[v.id]] <- angleBetween(v1 - v2, v3 - v2)
  }
  return (unlist(vangl))
}

calculate.angle.xyz <- function(f1, f2, x, y, z, df)
{
  vangl.x <- list()
  vangl.y <- list()
  vangl.z <- list()
  for (v.id in 1:nrow(df))
  {
    v1 <- c(df[v.id,paste(f1, '.Dx', sep ='')], df[v.id,paste(f1, '.Dy', sep ='')], df[v.id,paste(f1, '.Dz', sep ='')])
    v2 <- c(df[v.id,paste(f2, '.Dx', sep ='')], df[v.id,paste(f2, '.Dy', sep ='')], df[v.id,paste(f2, '.Dz', sep ='')])
    vangl.x[[v.id]] <- angleBetween(v1 - v2, x)
    vangl.y[[v.id]] <- angleBetween(v1 - v2, y)
    vangl.z[[v.id]] <- angleBetween(v1 - v2, z)
  }
  return (list(x = unlist(vangl.x), 
               y = unlist(vangl.y),
               z = unlist(vangl.z)))
}

cross.product <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)
  
  # Indices should be treated cyclically (i.e., index 4 is "really" index 1, and
  # so on).  Index3D() lets us do that using R's convention of 1-based (rather
  # than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1
  
  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}


generate.features.from.df <- function(df, mirror = FALSE)
{
  #df <- dddfff$data.frame
  f1 <- 'LeftShoulder'
  f2 <- 'LeftArm'
  f3 <- 'LeftForearm'
  left.ankle <- calculate.angle(f1, f2, f3, df)
  
  features.df <- data.frame(left.ankle = left.ankle)
  
  f1 <- 'RightShoulder'
  f2 <- 'RightArm'
  f3 <- 'RightForearm'
  right.ankle <- calculate.angle(f1, f2, f3, df)
  
  features.df$right.ankle <- right.ankle
  
  f1 <- 'LeftThigh'
  f2 <- 'LeftLeg'
  f3 <- 'LeftFoot'
  left.knee <- calculate.angle(f1, f2, f3, df)
  
  features.df$left.knee <- left.knee
  
  f1 <- 'RightThigh'
  f2 <- 'RightLeg'
  f3 <- 'RightFoot'
  right.knee <- calculate.angle(f1, f2, f3, df)
  
  features.df$right.knee <- right.knee
  
  y.help <- c(0,1,0)
  f1 <- 'RightThigh'
  v1 <- c(df[1,paste(f1, '.Dx', sep ='')], df[1,paste(f1, '.Dy', sep ='')], df[1,paste(f1, '.Dz', sep ='')])
  f1 <- 'LeftThigh'
  v2 <- c(df[1,paste(f2, '.Dx', sep ='')], df[1,paste(f2, '.Dy', sep ='')], df[1,paste(f2, '.Dz', sep ='')])
  
  
  x <- normalizeVector(v1 - v2)
  z <- normalizeVector(cross.product(y.help, x))
  y <- normalizeVector(cross.product(x,z))
  
  f1 <- 'RightThigh'
  f2 <- 'RightLeg'
  RightThigh <- calculate.angle.xyz(f1, f2, x, y, z, df)
  RightThigh.X <- RightThigh$x
  RightThigh.Y <- RightThigh$y
  RightThigh.Z <- RightThigh$z
  
  features.df$RightThigh.X <- RightThigh.X
  features.df$RightThigh.Y <- RightThigh.Y
  features.df$RightThigh.Z <- RightThigh.Z
  
  f1 <- 'LeftThigh'
  f2 <- 'LeftLeg'
  LeftThigh <- calculate.angle.xyz(f1, f2, x, y, z, df)
  LeftThigh.X <- LeftThigh$x
  LeftThigh.Y <- LeftThigh$y
  LeftThigh.Z <- LeftThigh$z
  
  features.df$LeftThigh.X <- LeftThigh.X
  features.df$LeftThigh.Y <- LeftThigh.Y
  features.df$LeftThigh.Z <- LeftThigh.Z
  
  f1 <- 'RightShoulder'
  f2 <- 'RightArm'
  RightShoulder <- calculate.angle.xyz(f1, f2, x, y, z, df)
  RightShoulder.X <- RightShoulder$x
  RightShoulder.Y <- RightShoulder$y
  RightShoulder.Z <- RightShoulder$z
  
  features.df$RightShoulder.X <- RightShoulder.X
  features.df$RightShoulder.Y <- RightShoulder.Y
  features.df$RightShoulder.Z <- RightShoulder.Z
  
  f1 <- 'LeftShoulder'
  f2 <- 'LeftArm'
  LeftShoulder <- calculate.angle.xyz(f1, f2, x, y, z, df)
  LeftShoulder.X <- LeftShoulder$x
  LeftShoulder.Y <- LeftShoulder$y
  LeftShoulder.Z <- LeftShoulder$z
  
  features.df$LeftShoulder.X <- LeftShoulder.X
  features.df$LeftShoulder.Y <- LeftShoulder.Y
  features.df$LeftShoulder.Z <- LeftShoulder.Z
  
  ##################################
  
  f1 <- 'RightArm'
  f2 <- 'RightForearm'
  RightArm <- calculate.angle.xyz(f1, f2, x, y, z, df)
  RightArm.X <- RightArm$x
  RightArm.Y <- RightArm$y
  RightArm.Z <- RightArm$z
  
  features.df$RightArm.X <- RightArm.X
  features.df$RightArm.Y <- RightArm.Y
  features.df$RightArm.Z <- RightArm.Z
  
  f1 <- 'LeftArm'
  f2 <- 'LeftForearm'
  LeftArm <- calculate.angle.xyz(f1, f2, x, y, z, df)
  LeftArm.X <- LeftArm$x
  LeftArm.Y <- LeftArm$y
  LeftArm.Z <- LeftArm$z
  
  features.df$LeftArm.X <- LeftArm.X
  features.df$LeftArm.Y <- LeftArm.Y
  features.df$LeftArm.Z <- LeftArm.Z
  
  
  f1 <- 'RightLeg'
  f2 <- 'RightFoot'
  RightLeg <- calculate.angle.xyz(f1, f2, x, y, z, df)
  RightLeg.X <- RightLeg$x
  RightLeg.Y <- RightLeg$y
  RightLeg.Z <- RightLeg$z
  
  features.df$RightLeg.X <- RightLeg.X
  features.df$RightLeg.Y <- RightLeg.Y
  features.df$RightLeg.Z <- RightLeg.Z
  
  f1 <- 'LeftLeg'
  f2 <- 'LeftFoot'
  LeftLeg <- calculate.angle.xyz(f1, f2, x, y, z, df)
  LeftLeg.X <- LeftLeg$x
  LeftLeg.Y <- LeftLeg$y
  LeftLeg.Z <- LeftLeg$z
  
  features.df$LeftLeg.X <- LeftLeg.X
  features.df$LeftLeg.Y <- LeftLeg.Y
  features.df$LeftLeg.Z <- LeftLeg.Z
  
  if (mirror)
  {
    v.h <- features.df$left.ankle
    features.df$left.ankle <- features.df$right.ankle 
    features.df$right.ankle <- v.h
    
    v.h <- features.df$left.knee
    features.df$left.knee <- features.df$right.knee 
    features.df$right.knee <- v.h
    
    v.h <- features.df$LeftThigh.X
    features.df$LeftThigh.X <- features.df$RightThigh.X 
    features.df$RightThigh.X <- v.h
    
    v.h <- features.df$LeftThigh.Y
    features.df$LeftThigh.Y <- features.df$RightThigh.Y 
    features.df$RightThigh.Y <- v.h
    
    v.h <- features.df$LeftThigh.Z
    features.df$LeftThigh.Z <- features.df$RightThigh.Z 
    features.df$RightThigh.Z <- v.h
    
    v.h <- features.df$LeftShoulder.X
    features.df$LeftShoulder.X <- features.df$RightShoulder.X 
    features.df$RightShoulder.X <- v.h
    
    v.h <- features.df$LeftShoulder.Y
    features.df$LeftShoulder.Y <- features.df$RightShoulder.Y 
    features.df$RightShoulder.Y <- v.h
    
    v.h <- features.df$LeftShoulder.Z
    features.df$LeftShoulder.Z <- features.df$RightShoulder.Z 
    features.df$RightShoulder.Z <- v.h
    
    v.h <- features.df$LeftArm.X
    features.df$LeftArm.X <- features.df$RightArm.X 
    features.df$RightArm.X <- v.h
    
    v.h <- features.df$LeftArm.Y
    features.df$LeftArm.Y <- features.df$RightArm.Y 
    features.df$RightArm.Y <- v.h
    
    v.h <- features.df$LeftArm.Z
    features.df$LeftArm.Z <- features.df$RightArm.Z 
    features.df$RightArm.Z <- v.h
    
    v.h <- features.df$LeftLeg.X
    features.df$LeftLeg.X <- features.df$RightLeg.X 
    features.df$RightLeg.X <- v.h
    
    v.h <- features.df$LeftLeg.Y
    features.df$LeftLeg.Y <- features.df$RightLeg.Y 
    features.df$RightLeg.Y <- v.h
    
    v.h <- features.df$LeftLeg.Z
    features.df$LeftLeg.Z <- features.df$RightLeg.Z 
    features.df$RightLeg.Z <- v.h
    #tu zamieniæ kolumny
  }
  
  return (features.df)
}

#switch.columns <- function

#############################################################################################################
#############################################################################################################

resample.data <- function(mocap.data, samples.count)
{
  df.help <- data.frame(xxx = rep(0, samples.count))
  for (a in 1:ncol(mocap.data))
  {
    #spline
    population <- mocap.data[,a]
    x <- seq(1:length(population))
    xx <- seq(from = 1, to = length(population), length.out = samples.count)
    func = splinefun(x=x, y=population, method="fmm",  ties = mean)
    df.help[,colnames(mocap.data)[a]] <- func(x = xx)
    #df.help[,colnames(mocap.data)[a]] <- mocap.data[,a]
  }
  df.help$xxx <- NULL
  #mocap.input$data.frame <- df.help
  #plot(mocap.input)
  colnames.help <- colnames(mocap.data)
  colnames.help.id <- grep('.Dx', colnames.help, ignore.case = TRUE)
  colnames.help.id <- c(colnames.help.id, grep('.Dy', colnames.help, ignore.case = TRUE))
  colnames.help.id <- c(colnames.help.id, grep('.Dz', colnames.help, ignore.case = TRUE))
  all.col.name.helper <- colnames.help[colnames.help.id]
  df.out <- df.help[,all.col.name.helper]
  return (df.out)
  #return (vec.out)
}


df.to.vector <- function(df)
{
  vec.out <- c()
  for (a in 1:ncol(df))
  {
    vec.out <- c(vec.out, df[,a])
  }
  return(vec.out)
}

#test package
library("RMoCap")
#path <- 'c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-12-22 ShoriunRiu MP\\evaluation\\hiza_geri_left\\segmented_bvh\\'

generate.data <- function(dir_names, path.in, samples.count, reference.data.path, Mean.Face = NULL, number.of.add.sub.samples = 3, mirror = FALSE)
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
  vector.to.sample <- seq(from=-1, to=1, by=0.001)
  
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
      df.all <- generate.features.from.df(helper.data.frame.sampled, mirror)
      
      #plot(mocap.input)
      class.names.list[[length(class.names.list) + 1]] <- dir_names[bb]
      vv <- df.to.vector(df.all)
      list.all[[aa.idx]] <- vv
      aa.idx <- aa.idx + 1
      #######################
      if (number.of.add.sub.samples > 0)
      {
        for (c in 1:number.of.add.sub.samples)
        {
          helper.data.frame <- helper.data.frame.sampled
          for (d in 1:ncol(helper.data.frame))
          {
            noise.vector <- sample(vector.to.sample, samples.count, replace = TRUE)
            helper.data.frame[d,] <- helper.data.frame[d,] + noise.vector
          }
          
          df.all <- generate.features.from.df(helper.data.frame)
          #plot(mocap.input)
          class.names.list[[length(class.names.list) + 1]] <- dir_names[bb]
          vv <- df.to.vector(df.all)
          list.all[[aa.idx]] <- vv
          aa.idx <- aa.idx + 1
        }
      }
      #######################
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
  
  return (list(df.all = df.all, class.names.list = class.names.list, mocap.input = mocap.input, Mean.Face = Mean.Face))
}

df.to.vector <- function(df)
{
  vec.out <- c()
  for (a in 1:ncol(df))
  {
    vec.out <- c(vec.out, df[,a])
  }
  return(vec.out)
}


classiffy.motion <- function(vector.to.classify, list.of.variables, eigenvalues)
{
  id <- 1
  min <- Inf
  for (a in 1:length(list.of.variables))
  {
    v1 <- list.of.variables[[a]]
    distance <- sqrt(sum(1/eigenvalues * (v1 - vector.to.classify)^2))
    if (distance < min)
    {
      id <- a
      min <- distance
      #print(a)
    }
  }
  return (id)
}

generate.eigen <- function(df.all, number.of.eigen, debug = FALSE)
{
  D <- df.all
  D <- t(D)
  mf <- colMeans(D)
  #mf2 <- rowSums(df.all) / ncol(df.all)
  
  nrow(D) # 399 images
  ncol(D) # 64*64 = 4096 pixels
  
  D <- D - mf
  #DDD <- scale(D)
  A <- cov(D)
  # Calculate the largest 20 eigenvalues and corresponding eigenvectors
  library(rARPACK)
  eigs <- rARPACK::eigs(A, number.of.eigen, which = "LM")
  # Eigenvalues
  eigenvalues <- eigs$values
  # Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
  eigenvectors <- eigs$vectors
  if (debug)
  {
    plot(1-eigenvalues^2/sum(eigenvalues^2))
    sum(eigs$vectors[,2])
  }
  return(list(eigenvalues = eigenvalues, eigenvectors = eigenvectors, mf = mf))
}


generate.features.from.eigen.old <- function(D, eigenvectors, mf)
{
  D<- t(D)
  D <- D - mf
  lista.cech <- list()
  for (a in 1:nrow(D))
  {
    #my.scaled.data22 <- scale(data.to.pca[,a])
    my.data <- D[a,]
    lista.cech[[a]] <- as.vector(t(eigenvectors) %*% (my.data))
  }
  return (lista.cech)
}

generate.features.from.eigen <- function(D, eigenvectors, mf)
{
  D<- (D)
  D <- D - mf
  lista.cech <- list()
  for (a in 1:ncol(D))
  {
    #my.scaled.data22 <- scale(data.to.pca[,a])
    my.data <- D[,a]
    lista.cech[[a]] <- as.vector(t(eigenvectors) %*% (my.data))
  }
  return (lista.cech)
}

combine.two.df <- function(df1, df2)
{
  df.all12 <- df1
  col.cont <- ncol(df1)
  for (a in 1:ncol(df2))
  {
    df.all12[,col.cont + a] <- df2[,a]
  }
  return (df.all12)
}



classify.and.return.results <- function(classfier.and.data)
{
  vector.all.classes <- sort(unique(classfier.and.data$class.names.vector3))
  matrix.class.resutls <- matrix(rep(0, length(vector.all.classes) * length(vector.all.classes)),
                                 nrow = length(vector.all.classes),
                                 ncol = length(vector.all.classes))
  colnames(matrix.class.resutls) <- vector.all.classes
  rownames(matrix.class.resutls) <- vector.all.classes
  
  for (a in 1:ncol(classfier.and.data$df3))
  {
    classify.results <- list()
    for (bbb in 1:length(classfier.and.data$classifiers))
    {
      class1 <- classfier.and.data$classifiers[[bbb]]
      vv1 <- as.vector(t(class1$eigenvectors) %*% (classfier.and.data$df3[[a]] - class1$mf))
      class.id <- classiffy.motion(vv1, class1$lc, class1$eigenvalues)
      classify.results[[bbb]] <- class1$class.names.list12[class.id]
    }
    classify.results <- unlist(classify.results)
    classify.result <- names(sort(table(classify.results),decreasing=TRUE)[1])
    matrix.class.resutls[classify.result, classfier.and.data$class.names.vector3[a]] <-
      matrix.class.resutls[classify.result, classfier.and.data$class.names.vector3[a]] + 1
  }
  return (matrix.class.resutls)
}


classify.and.return.results.svm <- function(classfier.and.data)
{
  vector.all.classes <- sort(unique(classfier.and.data$class.names.vector3))
  matrix.class.resutls <- matrix(rep(0, length(vector.all.classes) * length(vector.all.classes)),
                                 nrow = length(vector.all.classes),
                                 ncol = length(vector.all.classes))
  colnames(matrix.class.resutls) <- vector.all.classes
  rownames(matrix.class.resutls) <- vector.all.classes
  
  for (a in 1:ncol(classfier.and.data$df3))
  {
    classify.results <- list()
    for (bbb in 1:length(classfier.and.data$classifiers))
    {
      class1 <- classfier.and.data$classifiers[[bbb]]
      vv1 <- as.vector(t(class1$eigenvectors) %*% (classfier.and.data$df3[[a]] - class1$mf))
      vv1 <- 1/sqrt(class1$eigenvalues) * vv1
      class.name.svd <- as.character(predict(class1$svm.res,t(vv1)))
      #class.id <- classiffy.motion(vv1, class1$lc, class1$eigenvalues)
      classify.results[[bbb]] <- class.name.svd
    }
    classify.results <- unlist(classify.results)
    classify.result <- names(sort(table(classify.results),decreasing=TRUE)[1])
    matrix.class.resutls[classify.result, classfier.and.data$class.names.vector3[a]] <-
      matrix.class.resutls[classify.result, classfier.and.data$class.names.vector3[a]] + 1
  }
  return (matrix.class.resutls)
}

prepare.classfier.and.data <- function(df1, df2, df3, class.names.vector1, class.names.vector2, class.names.vector3,
                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE)
{
  df.all12 <- combine.two.df(df1, df2)
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
  return (list(classifiers = classifiers,
               class.names.vector3 = class.names.vector3,
               df3 = df3))
}

prepare.classfier.and.data.svm <- function(df1, df2, df3, class.names.vector1, class.names.vector2, class.names.vector3,
                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = 'radial')
{
  df.all12 <- combine.two.df(df1, df2)
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


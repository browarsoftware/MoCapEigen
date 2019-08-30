#RUN THIS SCRIPT
source("e:\\Publikacje\\MocapEigen\\R\\HelperFunctions.R ")
main.path <- 'e:\\Publikacje\\MocapEigen\\R\\'
#Multi-segmental movement patterns reflect juggling complexity and skill level

generate.features.from.df.inne <- function(df, mirror = FALSE)
{
  col.names <- colnames(df)
  c.id <- grepl(".Dx", col.names, fixed=TRUE)
  c.id <- c.id | grepl(".Dy", col.names, fixed=TRUE)
  c.id <- c.id | grepl(".Dz", col.names, fixed=TRUE)
  col.names <- col.names[c.id]
  features.df <- df[,col.names]
  return (features.df)
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
      if (TRUE)
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
      df.all <- generate.features.from.df.inne(helper.data.frame.sampled, mirror)
      
      #plot(mocap.input)
      class.names.list[[length(class.names.list) + 1]] <- dir_names[bb]
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
  
  return (list(df.all = df.all, class.names.list = class.names.list, mocap.input = mocap.input, Mean.Face = Mean.Face))
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

df.all1 <- data.from.mocap1$df.all
df.all2 <- data.from.mocap2$df.all
df.all3 <- data.from.mocap3$df.all

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
{
  classfier.and.data <- prepare.classfier.and.data.svm(df.all1, df.all2, data.from.mocap3$df.all, 
                                                       unlist(class.names.list1),
                                                       unlist(class.names.list2), 
                                                       unlist(data.from.mocap3$class.names.list), 
                                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = kernel)
  tables.res1 <- classify.and.return.results.svm(classfier.and.data)
}
#if (FALSE)
{
  classfier.and.data <- prepare.classfier.and.data.svm(data.from.mocap1$df.all, data.from.mocap3$df.all, data.from.mocap2$df.all, 
                                                       unlist(data.from.mocap1$class.names.list),
                                                       unlist(data.from.mocap3$class.names.list), 
                                                       unlist(data.from.mocap2$class.names.list), 
                                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = kernel)
  tables.res2 <- classify.and.return.results.svm(classfier.and.data)
}
#if (FALSE)
{
  classfier.and.data <- prepare.classfier.and.data.svm(data.from.mocap2$df.all, data.from.mocap3$df.all, data.from.mocap1$df.all, 
                                                       unlist(data.from.mocap2$class.names.list),
                                                       unlist(data.from.mocap3$class.names.list), 
                                                       unlist(data.from.mocap1$class.names.list), 
                                                       number.of.add.sub.samples, number.of.classfiers, debug = FALSE, kernel = kernel)
  
  tables.res3 <- classify.and.return.results.svm(classfier.and.data)
  
  #tables.res3 <- classify.and.return.results(classfier.and.data)
  #tables.res3
}


tables.res <- (tables.res1 + tables.res2 + tables.res3) / 30

name <- paste(main.path, 'results\\','\\Multi-segmental svm.kernel=',kernel,
              'samples=', samples.count, 
              'classes.per.features.set=', number.of.classes.per.features.set,
              'number.of.classfiers=', number.of.classfiers,
              'number.of.add.sub.samples=', number.of.add.sub.samples,
              'number.of.eigen=', number.of.eigen,
              'mirror=', mirror, '.txt', sep = '')
write.table(tables.res, name, sep = '\t',
            row.names = TRUE, col.names = TRUE)
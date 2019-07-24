#RUN THIS SCRIPT
source("e:\\Publikacje\\MocapEigen\\R\\HelperFunctions.R ")
main.path <- 'e:\\Publikacje\\MocapEigen\\R\\'

for (number.of.classes.per.features.set in c(4,6,8,10))
{
  for (number.of.classfiers in c(50,100,150,200))
  {
    for (number.of.add.sub.samples in c(0,1,2))
    {
      for (number.of.eigen in c(5,10,15,20))
      {
        samples.count <- 100
        set.seed(1)
        mirror = FALSE

        name <- paste(main.path, 'results\\', 
                      'samples=', samples.count, 
                      'classes.per.features.set=', number.of.classes.per.features.set,
                      'number.of.classfiers=', number.of.classfiers,
                      'number.of.add.sub.samples=', number.of.add.sub.samples,
                      'number.of.eigen=', number.of.eigen,
                      'mirror=', mirror, '.txt', sep = '')
                
        dir_names <- c('age_uke_left', 'age_uke_right', 'empi_left', 'empi_right', 
                       'mae_geri_left', 'mae_geri_right', 
                       'gedan_barai_left', 'gedan_barai_right',
                       'hiza_geri_left', 'hiza_geri_right',
                       'yoko_geri_left', 'yoko_geri_right')
        
        path <- paste(paste(main.path, 'data\\ShoriunRiu 1\\', sep = ''))
        #path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 1\\evaluation\\')
        ref.path <- paste(path, dir_names[1],'\\','sample0.bvh', sep = '')
        data.from.mocap1 <- generate.data(dir_names = dir_names, 
          path.in = path, 
          samples.count = samples.count, 
          reference.data.path = ref.path, 
          number.of.add.sub.samples = number.of.add.sub.samples)
        
        
        if (mirror)
        {
          #mirror 1
          dir_names <- c('age_uke_right', 'age_uke_left', 'empi_right', 'empi_left',
                         'mae_geri_right', 'mae_geri_left',
                         'gedan_barai_right', 'gedan_barai_left',
                         'hiza_geri_right', 'hiza_geri_left',
                         'yoko_geri_right', 'yoko_geri_left')
          
          data.from.mocap1.mirror <- generate.data(dir_names = dir_names, 
                                            path.in = path, 
                                            samples.count = samples.count, 
                                            reference.data.path = ref.path, 
                                            number.of.add.sub.samples = number.of.add.sub.samples, mirror = TRUE)
          
          #class names from previous dataset
          data.from.mocap1.mirror$class.names.list <- data.from.mocap1$class.names.list
        }
        
        dir_names <- c('age_uke_left', 'age_uke_right', 
                       'empi_left', 'empi_right', 
                       'mae_geri_left', 'mae_geri_right', 
                       'gedan_barai_left', 'gedan_barai_right',
                       'hiza_geri_left', 'hiza_geri_right',
                       'yoko_geri_left', 'yoko_geri_right')
        
        path <- paste(paste(main.path, 'data\\ShoriunRiu 2\\', sep = ''))
        #path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 2\\evaluation\\')
        #ref.path <- paste(path, dir_names[1],'\\segmented_bvh\\','sample0.bvh', sep = '')
        data.from.mocap2 <- generate.data(
          dir_names = dir_names, 
          path.in = path, 
          samples.count = samples.count, 
          reference.data.path = ref.path, 
          number.of.add.sub.samples = number.of.add.sub.samples)
        
        
        if (mirror)
        {
          dir_names <- c('age_uke_right', 'age_uke_left', 
                         'empi_right', 'empi_left',
                         'mae_geri_right', 'mae_geri_left', 
                         'gedan_barai_right', 'gedan_barai_left',
                         'hiza_geri_right', 'hiza_geri_left',
                         'yoko_geri_right', 'yoko_geri_left')
          
          data.from.mocap2.mirror <- generate.data(
            dir_names = dir_names, 
            path.in = path, 
            samples.count = samples.count, 
            reference.data.path = ref.path, 
            number.of.add.sub.samples = number.of.add.sub.samples, mirror = TRUE)
          
          #class names from previous dataset
          data.from.mocap2.mirror$class.names.list <- data.from.mocap2$class.names.list
        }
        
        dir_names <- c('age_uke_left', 'age_uke_right', 
                       'empi_left', 'empi_right',  
                       'mae_geri_left', 'mae_geri_right',
                       'gedan_barai_left', 'gedan_barai_right',
                       'hiza_geri_left', 'hiza_geri_right',
                       'yoko_geri_left', 'yoko_geri_right')
        
        path <- paste(paste(main.path, 'data\\ShoriunRiu 3\\', sep = ''))
        #path <- paste('c:\\projects\\mocap eigen\\mocap_data\\karate\\2016-04-01 ShoriunRiu 3\\evaluation\\')
        #ref.path <- paste(path, dir_names[1],'\\segmented_bvh\\','sample0.bvh', sep = '')
        data.from.mocap3 <- generate.data(
          dir_names = dir_names, 
          path.in = path, 
          samples.count = samples.count, 
          reference.data.path = ref.path, 
          number.of.add.sub.samples = number.of.add.sub.samples)
        
        if (mirror)
        {
          dir_names <- c('age_uke_right', 'age_uke_left',
                         'empi_right', 'empi_left',
                         'mae_geri_right', 'mae_geri_left',
                         'gedan_barai_right', 'gedan_barai_left',
                         'hiza_geri_right', 'hiza_geri_left',
                         'yoko_geri_right', 'yoko_geri_left')
          
          data.from.mocap3.mirror <- generate.data(
            dir_names = dir_names, 
            path.in = path, 
            samples.count = samples.count, 
            reference.data.path = ref.path, 
            number.of.add.sub.samples = number.of.add.sub.samples)
          
          #class names from previous dataset
          data.from.mocap3.mirror$class.names.list <- data.from.mocap3$class.names.list
        }
        
        
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
        
        if (mirror)
        {
          df.all1 <- combine.two.df(data.from.mocap1$df.all, data.from.mocap1.mirror$df.all)
          df.all2 <- combine.two.df(data.from.mocap2$df.all, data.from.mocap2.mirror$df.all)
          df.all3 <- combine.two.df(data.from.mocap3$df.all, data.from.mocap3.mirror$df.all)
          
          class.names.list1 <- c(data.from.mocap1$class.names.list, data.from.mocap1.mirror$class.names.list)
          class.names.list2 <- c(data.from.mocap2$class.names.list, data.from.mocap2.mirror$class.names.list)
          class.names.list3 <- c(data.from.mocap3$class.names.list, data.from.mocap3.mirror$class.names.list)
        } else
        {
          df.all1 <- data.from.mocap1$df.all
          df.all2 <- data.from.mocap2$df.all
          df.all3 <- data.from.mocap3$df.all
          
          class.names.list1 <- data.from.mocap1$class.names.list
          class.names.list2 <- data.from.mocap2$class.names.list
          class.names.list3 <- data.from.mocap3$class.names.list
        }
        
        
        #if (TRUE)
        {
          classfier.and.data <- prepare.classfier.and.data(df.all1, df.all2, data.from.mocap3$df.all, 
                                                           unlist(class.names.list1),
                                                           unlist(class.names.list2), 
                                                           unlist(data.from.mocap3$class.names.list), 
                                                           number.of.add.sub.samples, number.of.classfiers, debug = FALSE)
          tables.res1 <- classify.and.return.results(classfier.and.data)
        }
        #if (FALSE)
        {
          classfier.and.data <- prepare.classfier.and.data(data.from.mocap1$df.all, data.from.mocap3$df.all, data.from.mocap2$df.all, 
                                                           unlist(data.from.mocap1$class.names.list),
                                                           unlist(data.from.mocap3$class.names.list), 
                                                           unlist(data.from.mocap2$class.names.list), 
                                                           number.of.add.sub.samples, number.of.classfiers, debug = FALSE)
          tables.res2 <- classify.and.return.results(classfier.and.data)
        }
        #if (FALSE)
        {
          classfier.and.data <- prepare.classfier.and.data(data.from.mocap2$df.all, data.from.mocap3$df.all, data.from.mocap1$df.all, 
                                                           unlist(data.from.mocap2$class.names.list),
                                                           unlist(data.from.mocap3$class.names.list), 
                                                           unlist(data.from.mocap1$class.names.list), 
                                                           number.of.add.sub.samples, number.of.classfiers, debug = FALSE)
          tables.res3 <- classify.and.return.results(classfier.and.data)
        }
        
        
        tables.res <- (tables.res1 + tables.res2 + tables.res3) / 30
        write.table(tables.res, name, sep = '\t',
                    row.names = TRUE, col.names = TRUE)
        print("**********************************************************")
        print(name)
        print("**********************************************************")
      }
    }
  }
}



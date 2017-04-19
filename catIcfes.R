#setwd('/Users/williamcruz/Desktop/1 ICFES/H CAT')
# cambiar el directorio de trabajo a conveniencia

# 1 version: incluye un tiempo maximo de aplicacion de 'maxt' y numero maximo de items 'maxq'.
#            Carga una base de datos, la randomiza y presenta el problema y las opciones de respuesta
#            en esta version el test no es adaptativo
catIcfes <- function(){
  # get user's input from console
  {
    maxq    <- 10 # numero maximo de preguntas
    maxt    <- 20 # tiempo maximo de la aplicacion en segundos
    the_err <- 1  # valor inicial del error de estimacion de theta 
    
    args <- commandArgs(TRUE)
    answers <- as.data.frame(matrix(nrow = maxq, ncol = 3)) ; colnames(answers) <- c('answers','keyItem','resTime')
    
    st1 <- as.data.frame(as.table(proc.time()))
    st2 <- as.data.frame(as.table(proc.time()-st1$Freq[3]))
    x   <- 1
    
    # open data set and randomize it
    db   <- read.xlsx('Bases de datos CAT v1.xlsx',sheet = 1)
    rand <- sample(nrow(db))
    db   <- db[rand,]
    
    while(x <= maxq && st2$Freq[3] <= maxt && the_err >= 0.21){ 
        st0  <- as.data.frame(as.table(proc.time())) 
        preg <- paste('Responda la siguiente operacion matematica ',db$problema[x], sep = '...')
        
        cat(preg,"\n")
        
        v1 <- c('A','B','C','D')
        v2 <- c(db$opcion.A[x],db$opcion.B[x],db$opcion.C[x],db$opcion.D[x])
        
        msj0 <- paste0(v1,sep='. ',v2)
        msj0 <- paste0(msj0,collapse = "\n")
        cat(msj0)
        
        word <- readline(prompt="Escriba la opcion correcta y presione Enter para continuar: "); cat(word)
        answers$answers[x] <- word
        answers$keyItem[x] <- db$keyItem[x]
        
        st2 <- as.data.frame(as.table(proc.time()-st1$Freq[3]))
        st3 <- as.data.frame(as.table(proc.time()-st0$Freq[3]))
        answers$resTime[x] <- st3$Freq[3]
        
        x   <- x+1
        Sys.sleep(1) # esperar un 1 antes de la presentacion del siguiente item. 
        cat("\014") 
    }
  }
  answers$index <- as.numeric(answers$answers == answers$keyItem)
  answers<<-answers
  return(answers)
} 



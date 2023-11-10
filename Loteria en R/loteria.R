rm(list = ls())


library(beepr)
i <- 1
n <- 1
precio = 20
contador = 1
intento <- c()
gasto <- c()

#####

loteria <- function(numero) {

run = TRUE

while (run == TRUE) {
  
  sample <- sample(1:99999, 1)
  cat("Iter:", i, "intento:", n, "buscado:", numero, "lotería:", sample, "gasto:", precio*n, "\n")
  
    if (numero == sample) {
    cat("\nEl número",sample,"resulta premiado. \nIteración:", i, "Intento:", n)
    cat("\nEl gasto total en lotería ha sido de:", precio*n, "euros")
    run = FALSE
    }  

  n = n + 1
  
  }  

  intento <<- c(intento, n - 1) # va creando la lista de elementos y saca la lista fuera de la función con <<-
  gasto <<- c(gasto, precio*(n - 1))
}

numero = 29570

while (i < 100) {
  loteria(numero)
  beep(sound = 1)
  Sys.sleep(2)
  
  if (i == 99) {
    print("FIN")
    beep(sound = 2)
    Sys.sleep(2)
  }
  i <- i + 1
}

intento
gasto

mean(intento)
median(intento)
mean(gasto)

options(scipen = 999)

hist(intento, main = "Distribución de intentos | lotería",
     sub = "frecuencia de aciertos del número en n intentos",
     xlab = "Intentos", ylab = "Frecuencia",
     col = "skyblue", border = "black")
abline(v=median(intento), col="purple")
abline(v=mean(intento), col="blue")
legend("topright", legend = "Media", col = "purple", 
       lty = 1, lwd = 2, bty = "n", cex = 0.9, inset = c(0, 0.1), xpd = TRUE)
legend("topright", legend = "Mediana", col = "blue", 
       lty = 1, lwd = 2, bty = "n", cex = 0.9, inset = c(0, 0.2), xpd = TRUE)



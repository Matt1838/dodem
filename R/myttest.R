myt.test <- function(x, y,alternative = "two.sided", paired = FALSE, var.equal = FALSE, brief=FALSE){
  # Gestion d'erreurs
  if (is.null(x)==TRUE){
    stop("Erreur : Le vecteur x ne peut pas etre NULL")
  }
  if (is.null(y)==TRUE){
    stop("Erreur : Le vecteur y ne peut pas etre NULL")
  }
  if (is.numeric(x)==FALSE){
    stop("Erreur : Le vecteur x doit etre numerique")
  }
  if (is.numeric(y)==FALSE){
    stop("Erreur : Le vecteur x doit etre numerique")
  }
  # Cas d'echantillons apparies
  if (paired == TRUE) {
    if (length(x)!=length(y)){
      stop("Erreur : Les vecteurs x et y doivent etre de meme longueur")
    }
    n <- length(x)
    statistic <- (mean(x)-mean(y))/(sqrt(var(x-y)/n))
    df = n-1
    method <- 'Student two sample t-test'
  }
  # Cas d'echantillons non apparies
  if (paired ==FALSE) {
    n1 <- length(x)
    n2 <- length(y)
    # Cas d'egalite des variances
    if (var.equal == TRUE){
      statistic <- (mean(x)-mean(y))/(sqrt(((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2))*sqrt(1/n1+1/n2))
      df <- n1+n2-2
      method <- 'Student two sample t-test'
    }
    # Cas de non egalite des variances
    if (var.equal == FALSE) {
      statistic <- (mean(x)-mean(y))/sqrt(var(x)/n1+var(y)/n2)
      df <- (var(x)/n1+var(y)/n2)^2/((var(x)/n1)^2/(n1-1)+(var(y)/n2)^2/(n2-1))
      method <- 'Welch two sample t-test'
    }
  }
  # Calcul de la p-value : Cas bivarie
  if (alternative == "two.sided"){
    if (statistic < 0){
      p.value <- 2*pt(statistic, df)
    }
    else{
      p.value <- 2*pt(statistic, df, lower.tail = FALSE)
    }
  }
  # Cas univaries
  if (alternative == "less"){
    p.value <- pt(statistic, df)
  }
  if (alternative == "greater"){
    p.value <- pt(statistic, df, lower.tail = FALSE)
  }
  return(list(method, alternative, p.value, df,statistic))
}

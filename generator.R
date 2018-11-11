#funkcja pomocnicza
modular_inverse <- function(k, m) {

  if (k == 0) {
    return(0)
  }

  u <- 1
  w <- k
  x <- 0
  z <- m

  while(w) {

    if (w < z) {
      q <- u
      u <- x
      x <- q
      q <- w
      w <- z
      z <- q
    }

    q <- w %/% z
    u <- u - q * x
    w <- w - q * z

  }

  if (z == 1) {
    if (x < 0) {
      x <- x + m
    }
    return(x)
  } else {
    return(-1)
  }

}


mod_inv_generator <- function(x0, a, b, m, n) {

  if (x0<0 | floor(x0)!=x0) {
    stop('Zla wartosc poczatkowa!')
  }
  if (floor(a)!=a) {
    stop('Zly parametr a!')
  }
  if (floor(b)!=b) {
    stop('Zly parametr b!')
  }
  if (m<2 | floor(m)!=m) {
    stop('Zly parametr m!')
  }
  if (n<1 | floor(n)!=n) {
    stop('Zly parametr n!')
  }


  results <- rep(0, n)

  ind <- 1
  x <- x0

  while(ind <= n) {

    x_mod_inv <- modular_inverse(x, m)
    if (x_mod_inv == -1) {
      print('Brak odwrotnosci modulo - upewnij sie, ze m jest liczba pierwsza!')
      return(-1)
    }
    x <- (a * x_mod_inv + b) %% m
    results[ind] <- x
    ind <- ind + 1
  }

  return(results)

}

formatSeries <- function(vec){
  vec[vec == vec[1]] <- paste("<font color = 'blue' size = '5'>" , vec[vec == vec[1]], "</font>")
  vec
}


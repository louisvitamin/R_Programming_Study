blackscholes <- function(S,X,rx,T,sigma){
  
  d1 <- (log(S/X)+(rx+sigma^2/2)*T)/sigma*sqrt(T)
  d2 <- d1 - sigma*sqrt(T)
  
  values <- S*pnorm(d1) - X*exp(-rx*T)*pnorm(d2)
  
  return(values)
}
# theoretical price of call option 

imp.vol <-
  function(S, X, rx, T, op){
    sig = 0.2
    sig.up = 2
    sig.down = 0.001
    count = 0 

    df = blackscholes(S, X, rx, T, sig) - op
    
    while(abs(df) > 0.001 && count<2000 ){
      if(df < 0){
        sig.down = sig
        sig = (sig.up + sig)/2
      }else{
        sig.up = sig
        sig = (sig.down + sig)/2
      }
      df = blackscholes(S, X, rx, T, sig) - op
      count = count + 1
    }
    if(count==2000){
      return(NA)
    }else{
      return(sig)
    }
  }


blackscholes(100, 102, 0.03, 6/252, .75)

imp.vol(122.44, 146, 0.0002, 18/252, .08)

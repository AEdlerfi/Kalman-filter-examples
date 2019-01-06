#----------------------------------------------------------------------------
# Kalman Filter with DLM
#----------------------------------------------------------------------------

# Example 1 - Terms of Trade Local level model using DLM

Data <- tibble(Date = ToT$date,
               Value = ToT$value )


Tot <-  ts(ToT$value, start = c(1959,3), f = 4)

# tot  = FFOt +vt
# Ot = GGOt-1 +wt

# Local level model F = 1, G = 1, only parameters of the model to be estimated are the observation, Vt, and transition Wt, variances. 

# in DLM FF, GG is a constant model (not time varying)
 
mod1 <- function(params){
  
  dlm(
    FF = 1, 
    V = exp(params[1]),
    GG = 1,
    W = exp(params[2]),
    m0 = 1,
    C0 = 100
    
  ) 
  
  }

mod1.est <-  dlmMLE(Tot, parm = c(0,0), build = mod1)

dlmTot <- mod1(mod1.est$par)

KF.tot <- data.frame(
  Data <- Tot,
  Filtered.level = dlmFilter(Tot, dlmTot)$f,
  Smoothed.level = dlmSmooth(Tot, dlmTot)$s[-1] 
  )
                      

KF.tot %>% 
  mutate(Date = ToT$date) %>% 
  gather(Type, Value, - Date) %>% 
  filter(Date >= '1960-06-01') %>% 
  ggplot()+ 
  geom_line(aes(Date, Value, colour = Type))


      
      
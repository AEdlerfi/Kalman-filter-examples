#----------------------------------------------------------------------------
# Kalman Filter with DLM
#----------------------------------------------------------------------------

# Example 1 - Terms of Trade Local level model using DLM

Data <- tibble(Date = ToT$date,
               Value = ToT$value )


Tot <-  ts(ToT$value, start = c(1959,3), f = 4)

# tot  = FF0t +vt
# 0t = GG0t-1 +wt

# Local level model F = 1, G = 1, only parameters of the model to be estimated are the observation, Vt, and transition Wt, variances. 

# in DLM FF, GG is a constant model (not time varying)
 
mod1 <- function(params){
  
  dlm(
    FF = 1, # is a 1x1 dimenstional matrix, which pulls from the State vector 0t there is no unknown parameters here as the model is a random walk
    V = exp(params[1]), # the system variance, needs estimating 
    GG = 1, # transition matrix, showing how the state evolves, no parameters needed to estimated here as the model is a random walk.
    W = exp(params[2]), # transition equation variance, variance of the state vector (how the state evolves)
    m0 = 1, # prior estimates (mean of the state)
    C0 = 100 # prior estimtes (variance of the state)
    
  ) 
  
  }

mod1.est <-  dlmMLE(Tot, parm = c(0,0), build = mod1)

dlmTot <- mod1(mod1.est$par)

#-------------------------------------------------------------------------
# Filtered and smoothed states 
#-------------------------------------------------------------------------

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

#-------------------------------------------------------------------------
# Residual analysis 
#-------------------------------------------------------------------------


      
      
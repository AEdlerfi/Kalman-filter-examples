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
# Local linear trend 
#-------------------------------------------------------------------------

# tot  = FF0t +vt
# 0t = GG0t-1 +wt

# FF = [1,0]
# GG = [1,1  , 0t = [a,b]' # trend 2x2.x.1
#      0,1]
# Wt = [0,0
#       0,1]  # state variance comes from trend

# Construct DLM (need to set out above like this!)

mod2 <- dlm(
  
  FF =  matrix(c(1,0), ncol = 2, byrow = TRUE), 
  
  V =  matrix(0), 
  
  GG = matrix(c(1,1,
                0,1), ncol = 2, byrow = TRUE) , 
  
  W = matrix(c(0,0,
               0,0), ncol = 2, byrow = TRUE),
  
  m0 =  c(1,1), 
  
  C0 =  matrix(c(1e+07,0,
                 0,1e+07), ncol =2,  byrow = TRUE) 
  
) 

# Est params

buildFun <- function(params){
  
  V(mod2) <- exp(params[1])
  
  W(mod2)[2,2] <- exp(params[2])
  
  return(mod2)
  
}

mod2.est <-  dlmMLE(Tot, parm = c(0,0), build = buildFun)

dlmTotlt <- buildFun(mod2.est$par)

#-------------------------------------------------------------------------
# Local linear trend 
#-------------------------------------------------------------------------

KF.tot <- data.frame(
  Data <- Tot,
  Filtered.level = dlmFilter(Tot, dlmTotlt)$f,
  Smoothed.level = dlmSmooth(Tot, dlmTotlt)$s[-1,1] 
)


KF.tot %>% 
  mutate(Date = ToT$date) %>% 
  gather(Type, Value, - Date) %>% 
  filter(Date >= '1960-06-01') %>% 
  ggplot()+ 
  geom_line(aes(Date, Value, colour = Type))

#-------------------------------------------------------------------------
# AR model with local linear trend GDP 
#-------------------------------------------------------------------------


# Construct DLM (need to set out above like this!)

mod3 <- dlm(
  
  FF =  matrix(c(1,0,1,0), ncol = 4, byrow = TRUE), 
  
  V =  matrix(0), 
  
  GG = matrix(c(1,1,0,0,
                0,1,0,0,
                0,0,0,1,
                0,0,0,0), ncol = 4, byrow = TRUE) , # W[3,3] and W[4,3] AR parameters to be estimated 
  
  W = matrix(c(0,0,0,0,
               0,0,0,0,
               0,0,0,0,
               0,0,0,0), ncol = 4, byrow = TRUE),
  
  m0 =  c(0,0,0,0), 
  
  C0 =  matrix(c(1e+07,0,0,0,
                 0,1e+07,0,0,
                 0,0,1e+07,0,
                 0,0,0,1e+07), ncol =4,  byrow = TRUE) 
  
) 

# Set priors for level and slope

Level <- log(GDP$value[1])
Slope <- mean(diff(log(GDP$value)))

# Est params

buildFun <- function(params){
  
  V(mod3) <- 1e-7
  
  GG(mod3)[3,3] <- ARtransPars(params[4])
  
  GG(mod3)[4,3] <- ARtransPars(params[5])
  
  W(mod3)[1,1] <- exp(params[1])
  
  W(mod3)[2,2] <- exp(params[2])
  
  W(mod3)[3,3] <- exp(params[3])
  
  m0(mod3)[1:2] <- c(Level,Slope)
  
  C0(mod3)[1,1] <- 2
  
  C0(mod3)[2,2] <- 2
  
  return(mod3)
  
}

mod3.est <-  dlmMLE(log(GDP$value), parm = c(-2.75,-1,-3,0.4,0.4), build = buildFun)

dlmGDPgap <- buildFun(mod3.est$par)

#-------------------------------------------------------------------------
# AR1 Local level 
#-------------------------------------------------------------------------

Pot.out <- data.frame(
  log.GDP =log(GDP$value),
  
  Potential.Output = dlmSmooth(log(GDP$value), dlmGDPgap)$s[-1,1], # 1 element of the state vector : Pot(t) = Pot(t-1)+trend 
  
  Output.gap = dlmSmooth(log(GDP$value), dlmGDPgap)$s[-1,3] # 3 element of the state vector : gap(t) = AR1*gap(t-1)+ AR2*gap(t-2)
  
)


Pot.out %>% 
  mutate(Date = GDP$date) %>% 
  gather(Type, Value, - Date) %>% 
  filter(Date >= '1960-06-01' & Type != "Output.gap") %>% 
  ggplot()+ 
  geom_line(aes(Date, Value, colour = Type))+
  theme_bw()
  

Pot.out %>% 
  mutate(Date = GDP$date) %>% 
  gather(Type, Value, - Date) %>% 
  filter(Date >= '1960-06-01' & Type == "Output.gap") %>% 
  ggplot()+ 
  geom_hline(yintercept = 0)+
  geom_line(aes(Date, Value, colour = Type))+
  theme_bw()


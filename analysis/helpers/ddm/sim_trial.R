sim_trial <- function(vL = 1, vR = 1, d = 2, s = 2, b = 0, ndt = .1, a = 1, dt = .1, maxTime=25, warning=T) {

  # Convert to dt space.
  d = d*dt
  s = s*dt

  # Initiate process.
  E = b

  # Non-decision time.
  time = ceiling(ndt/dt)

  # Diffusion.
  timeout = F
  while (abs(E) < a) {
    time = time + 1
    epsilon = rnorm(1, mean=0, sd=s)
    E = E + d*(vL - vR) + epsilon
    if (time>(maxTime*(1/dt))) {
      timeout = T
      break
    }
  }

  # Record choice and response time. Don't forget to convert back to seconds.
  time = time*dt
  if (timeout) {
    if (warning==T) {print("Simulation timed out.")}
    return(list(choice=NA, rt=NA))
  }
  if (E>a) { return(list(choice=1, rt=time)) }
  if (E<-a) { return(list(choice=0, rt=time)) }
}
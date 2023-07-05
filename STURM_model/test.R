    # Find the constant that minimizes the objective function
    print("Starting calibration (can take some t)")

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "BFGS", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "CG", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "DE", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "SANN", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    # Retrieve the constant solution
    # write.csv(left_join(ms_i,  mutate(target, constant = result$par)), "test.csv")

    constant <- result$par

    objective_function(rep(0, times = nrow(target)))
    constant <- rep(0, times = nrow(target))

    # Find the constant that minimizes the objective function
    result <- uniroot(objective_function, interval = c(-1000, 0))
    constant_solution <- result$root

    # Print the constant solution
    cat("Constant solution:", constant_solution)

    

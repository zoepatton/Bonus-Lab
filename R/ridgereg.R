#' Ridge Regression
#' 
#' Sets reference class for ridge regression model
#' 
#' @field formula as a formula
#' @field data as a data frame
#' @field norm_matrix as a matrix
#' @field lambda as numeric
#' @field y_hat as a matrix
#' @field beta_hat as a vector
#' 
#' @return Returns an object of the class 'ridgereg'
#' 
#' @export ridgereg
#' @exportClass ridgereg
#' 

ridgereg <- setRefClass("ridgereg",
                        # Include fields ------          
                        fields= list(formula="formula",
                                     call = "vector",
                                     data="data.frame",
                                     lambda="numeric",
                                     y_hat = "matrix",
                                     beta_hat = "matrix",
                                     x_values = "matrix",
                                     y_values = "matrix"
                        ), 
                        
                        
                        methods=list(
                          #Initialize the the function ------
                          initialize = function(formula, data, lambda){
                            call <<- c("ridgereg(formula = ",
                                       Reduce(paste,deparse(formula)),
                                       ", data = ",
                                       deparse(substitute(data)),
                                       ", lambda = ",
                                       lambda,
                                       ")")
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            #Independent X-values
                            x_values <<- scale(model.matrix(formula, data=data)[,-1])
                            
                            #Dependent y-values
                            y_label <- all.vars(formula)[1]
                            y_values <<- as.matrix(data[[y_label]])
                            
                            #Normalize covariates
                            
                            x_values_new <- rbind(x_values, diag(x = sqrt(lambda), nrow = ncol(x_values), ncol = ncol(x_values)))
                            y_values_new <- rbind(y_values, matrix(0, nrow = ncol(x_values), ncol = 1))
                            
                            #QR
                            
                            xQR = qr(x_values_new)
                            Q = qr.Q(xQR)
                            R = qr.R(xQR)
                            
                            #Coefficient estimates
                          
                            beta_hat <<- solve(R) %*% t(Q) %*% y_values_new

                            
                            #Fitted values
                            
                            y_hat <<- x_values %*% beta_hat
                            
                          },
                          
                          #Print function
                          
                          print = function(){
                            #print call
                            cat("\nCall:\n")
                            lapply(call, cat)
                            #Names of the coefficients
                            c_names <- row.names(beta_hat)
                            #Vals of coeff.
                            c_vals <- as.vector(beta_hat)
                            #print Coefficients:
                            cat("\n\nCoefficients:\n")
                            cat(paste(c_names,collapse = "  "),collapse="\n")
                            cat(paste(c_vals,collapse = "  "),collapse="\n")
                          },
                          
                          #Predict function
                          
                          predict = function(){
                            
                            return(y_hat)
                            
                          },
                          
                          #Coefficients function
                          
                          coef = function(){
                            
                            coef_vector <- as.vector(t(beta_hat))
                            names(coef_vector) <- row.names(beta_hat)
                            
                            return(coef_vector)
                          }
                        )
)

#print call: mod_object$print()


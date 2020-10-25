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
                                     data="data.frame",
                                     norm_matrix="matrix",
                                     lambda="numeric",
                                     y_hat = "matrix",
                                     beta_hat = "matrix",
                                     X = "matrix",
                                     Y = "matrix"
                        ), 
                        
                        
                        
                        methods=list(
                          #Initialize the the function ------
                          initialize = function(formula, data, lambda){
                            
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            #Independent X-values
                            X <<- scale(model.matrix(formula, data=data)[,-1])
                            
                            #Dependent y-values
                            y_label <- all.vars(formula)[1]
                            Y <<- as.matrix(data[[y_label]])
                            
                            #Normalize covariates
                            
                            L = diag(x = sqrt(lambda), nrow = ncol(X), ncol = ncol(X))
                            
                            X_new = rbind(X, L)
                            Y_new = rbind(Y, matrix(0, nrow = ncol(X), ncol = 1))
                            
                            #QR
                            
                            xQR=qr(X_new)
                            Q = qr.Q(xQR)
                            R = qr.R(xQR)
                            
                            #Coefficient estimates
                          
                            beta_hat <<- solve(R) %*% t(Q) %*% Y_new

                            #beta_hat <<- solve(t(norm_matrix) %*% norm_matrix + diag(lamda, nrow=ncol(norm_matrix))) %*% t(norm_matrix) %*% y
                            
                            #Fitted values
                            
                            #y_hat <<- norm_matrix %*% beta_hat
                            y_hat <<- X %*% beta_hat
                            
                          },
                          
                          #Print function
                          
                          print = function(){
                            #print call
                            cat("\nCall:\n")
                            lapply(call, cat)
                            #Names of the coefficients
                            c_names <- row.names(beta_hat)
                            #Vals of coeff.
                            c_vals <- as.numeric(beta_hat)
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


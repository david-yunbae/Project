#' Assumption
#'
#' @description This function outputs the 3 graphs of assumption for the linear regression model.
#' @param x text string; This should be the data file name of the test sample.
#'
#' @return 3 graphs of linearity, constant variance and normal distribution of the residuals
#' @export
#'
#' @examples assumption(x=project-csv)
assumption <- function(x){
  dat <- readr::read_csv(x,show_col_types = FALSE)
  D <- lm(height ~ weight,dat)
  a <- ggplot(dat,aes(x=height,y=weight))+ geom_point() + stat_smooth(method="lm", col="red") +ggtitle("I) Y vs X")

  b <- ggplot(dat)+geom_point(mapping=aes(x=D$fitted.values ,y=D$residuals)) + geom_hline(yintercept=0,lwd=2)+ggtitle("II) Residual plot")+ylab("Residuals")+xlab("Fitted values")

  c <- ggplot(dat)+geom_histogram(mapping=aes(x=D$residuals),bins=40) +ggtitle("III) Distribution is normal")+xlab("Residuals")

  print((a+b)/c)
}


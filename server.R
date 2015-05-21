library(forecast)
library(fpp)
data(sunspotarea)

shinyServer(
        function(input, output) {
                
                output$newPlot <- renderPlot({
                                plot(sunspotarea, xlab='year', main='Sunspots area Forecasts')
                                lasttv <- input$lasttv
                                trainingSet<- window(sunspotarea, 1875, c(1875, lasttv-1875))
                                fit <- nnetar(trainingSet,lambda=0)
                                fitSet<-window(fit$fitted,1875, c(1875, lasttv-1875))
                                fcast <- forecast(fit,h=30)$mean
                                measured <- sunspotarea
                                lines(fcast,col="blue",lwd=3)
                                lines(fitSet,col="red")
                                lines(c(lasttv, lasttv), c(0, 2500),col="red",lwd=3)
                                rmseTest <- sqrt(mean((fcast - measured)^2))
                                rmseTrain <- accuracy(fit)[2]
                                text(1900, 2700, paste("LAST TRAINING YEAR = ", lasttv))
                                text(1900, 2500, paste("RMSE TRAIN = ", round(rmseTrain, 0)))
                                text(1900, 2300, paste("RMSE TEST = ", round(rmseTest, 0)))
                        })
        }
)
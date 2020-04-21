single_lift<-function(actual,predicted,nbin,exposure)
{
  
  plot_data<-data.table(actual=actual,pred=predicted
                        ,expo=exposure)
  
  
  plot_data1<-plot_data[order(pred),]
  
  expo_bin<-cumsum(plot_data1$expo)/sum(plot_data1$expo)
  
  bins<- cut(expo_bin,nbin)
  
  plot_data2<-data.frame(actual=plot_data1$actual,predicted=plot_data1$pred,bins)
  
  rm(plot_data1)
  
  rm(plot_data)
  
  actual_mean<-setDT(plot_data2)[, .(actual = mean(actual)), by = .(bins)]
  
  predicted_mean<-setDT(plot_data2)[, .(predicted = mean(predicted)), by = .(bins)]
  
  mean_frame<-data.frame(bins=1:nbin,actual=actual_mean$actual,predicted=predicted_mean$predicted)
  
  ggplot() +
    geom_point(data = mean_frame, aes(x = bins, y = actual), color = "blue") +
    geom_point(data = mean_frame, aes(x = bins, y = predicted), color = "red")
  
}

single_lift(pre.test_freq$claimcst0,pure_prem_pred*pre.test_freq$exposure,20,pre.test_freq$exposure)


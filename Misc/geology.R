tempdata = F993700408373824102023104258PM
tempdata2 = tempdata[,c(39:47)]
tempdata2$total = rowSums(tempdata2)
#tempdata3 = tempdata[which(tempdata2$total > 90),]
tempdata3$total = rowSums(tempdata3[,c(39:47)])
tempdata3 = 100*tempdata[,c(39:47)]/tempdata3$total
tempdata3$total = rowSums(tempdata3)
tempdata3$talc = tempdata3$`K2O(WT%)` + tempdata3$`NA2O(WT%): altern. values or methods`


ggplot(data3 = tempdata3)+
  geom_smooth(aes(x = tempdata3$`SIO2(WT%)`, y = tempdata3$talc))+
  geom_point(aes(x = tempdata3$`SIO2(WT%)`, y = tempdata3$talc))
###
#task4
tempdata4 = tempdata[which(tempdata$`SIO2(WT%)`>69),]
tempdata4 = round(tempdata4[,37:47], )
tempdata4$`AL2O3(WT%)` = tempdata4$`AL2O3(WT%)`/101.9612
tempdata4$`CAO(WT%)` = tempdata4$`CAO(WT%)`/56.0774
tempdata4$`NA2O(WT%): altern. values or methods` = tempdata4$`NA2O(WT%): altern. values or methods`/61.9789
tempdata4$`K2O(WT%)` = tempdata4$`K2O(WT%)`/94.196
tempdata4$ANK = tempdata4$`AL2O3(WT%)`/(tempdata4$`NA2O(WT%): altern. values or methods` + tempdata4$`K2O(WT%)`)
tempdata4$ACNK = tempdata4$`AL2O3(WT%)`/(tempdata4$`NA2O(WT%): altern. values or methods` + tempdata4$`K2O(WT%)` + tempdata4$`CAO(WT%)`)

ggplot(data3 = tempdata4)+
  geom_count(aes(x = tempdata4$ACNK, y = tempdata4$ANK))+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 1)+
  xlab('A/CNK')+
  ylab('A/NK')


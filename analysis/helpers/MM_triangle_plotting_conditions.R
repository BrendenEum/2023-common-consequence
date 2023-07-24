library(dplyr)
library(ggplot2)

p_list = c(.3, .5, .7)
r_list = c(.3, .7)

x_a_list = c()
x_b_list = c()
y_a_list = c()
y_b_list = c()
x_ap_list = c()
x_bp_list = c()
y_ap_list = c()
y_bp_list = c()

ind = 1
for (p in p_list) {
  for (r in r_list){

    x_a = 0
    x_a_list[ind] = x_a
    y_a = 0
    y_a_list[ind] = y_a

    x_b = r*(1-p)
    x_b_list[ind] = x_b
    y_b = p*r
    y_b_list[ind] = y_b

    x_ap = 1-r
    x_ap_list[ind] = x_ap
    y_ap = 0
    y_ap_list[ind] = y_ap

    x_bp = 1-p*r
    x_bp_list[ind] = x_bp
    y_bp = p*r
    y_bp_list[ind] = y_bp

    ind = ind + 1

  }
}

pdata.a = data.frame(x = x_a_list, y = y_a_list, lottery = "a")
pdata.a$choice = seq(1, 6, 1)
pdata.b = data.frame(x = x_b_list, y = y_b_list, lottery = "b")
pdata.b$choice = seq(1, 6, 1)
pdata.ap = data.frame(x = x_ap_list, y = y_ap_list, lottery = "ap")
pdata.ap$choice = seq(7, 12, 1)
pdata.bp = data.frame(x = x_bp_list, y = y_bp_list, lottery = "bp")
pdata.bp$choice = seq(7, 12, 1)
pdata = rbind(pdata.a, pdata.b)
pdata = rbind(pdata, pdata.ap)
pdata = rbind(pdata, pdata.bp)
pdata$choice = factor(pdata$choice)

plt.MM = ggplot(data=pdata, aes(x=x, y=y)) +
  geom_abline(intercept=1, slope=-1)+
  geom_line(aes(group=choice), linewidth=1.5, color='lightgrey')+
  geom_point(aes(color=lottery), size=7)+
  theme_classic()+
  coord_cartesian(expand=F) +
  xlim(c(0,1))+
  ylim(c(0,1)) +
  labs(y = "Pr(Outcome 1)", x = "Pr(Outcome 3)", color = "Lottery")

plot(plt.MM)

figdir = "D:/OneDrive - California Institute of Technology/PhD/Rangel Lab/2023-common-consequence/analysis/outputs/figures"
ggsave(file.path(figdir, "visualizing_lottery_choices_MM.png"), plot=plt.MM)

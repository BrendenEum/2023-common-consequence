difference_calc <- function(p,r,h,m) {



  amts = c(h, m, 0)

  probs = c(1:3)
  probs[1] = p*r
  probs[2] = 1-r
  probs[3] = r*(1-p)

  return(sum(amts*probs) - m)
}

p_list = c(.3, .5, .7)
r_list = c(.3,.7)


m = 20
m_bot_mult = .5
m_top_mult = 5



xs = c()
ys= c()
rs = c()
ps = c()

ind = 1
for (p in p_list) {
  for (r in r_list){
    h_list = seq(m*m_bot_mult,m*m_top_mult,2)
    for (h in h_list) {
      x = h
      y = difference_calc(p, r, h, m)
      xs[ind] = x
      ys[ind] = y
      ps[ind] = p
      rs[ind] = r
      ind = ind+1
    }
  }
}
pdata.h = data.frame(x = xs, y=ys, p=ps, r=rs)

plt.h <- ggplot(data=pdata.h, aes(x=x, y=y)) +
  geom_hline(yintercept=0, color='darkgrey') +
  geom_line(linewidth=2) +
  theme_classic() +
  ylim(c(-10,10)) +
  labs(y = "Expected Value Difference", x = "Outcome 1 in Lotteries (H)") +
  facet_grid(cols = vars(p), rows=vars(r), labeller=label_both)



figdir = "D:/OneDrive - California Institute of Technology/PhD/Rangel Lab/2023-common-consequence/analysis/outputs/figures"
ggsave(file.path(figdir, "choosing_h_range.png"), plot=plt.h)
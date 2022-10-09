install.packages("piecewiseSEM")
library(piecewiseSEM)

dat <- read.csv('https://raw.githubusercontent.com/ivanhykwong/RoadsideTrees-ShauKeiWan/main/TreeData_SEMvariable.csv')
dat <- dat[,-c(1:6)]

colnames(dat) <- c('Z2','Z3','PedWidth','Lane','DBuilding','Insol','NewP',
                   'Native','Palm','C_b','X_c','B_b','F_b',
                   'Dist_C','Tree_Den','Close_same','DBH_C','DBH','S_DBH')
col <- c('PedWidth', 'Lane','DBuilding','Insol',
         'Dist_C','DBH_C','Tree_Den','DBH','S_DBH')
dat[col] <- scale(dat[col])

model <- psem(
  glm(NewP~Z2+Z3+PedWidth+Lane+DBuilding+Insol,'binomial',dat),
  
  glm(Native~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  glm(Palm~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  glm(C_b~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  glm(X_c~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  glm(B_b~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  glm(F_b~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP,'binomial',dat),
  
  lm(Dist_C~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b,dat),
  glm(Close_same~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b,'binomial',dat),
  lm(DBH_C~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b,dat),
  lm(Tree_Den~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b,dat),
  
  lm(DBH~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b+Dist_C+Close_same+DBH_C+Tree_Den,dat),
  lm(S_DBH~Z2+Z3+PedWidth+Lane+DBuilding+Insol+NewP+Native+Palm+C_b+X_c+B_b+F_b+Dist_C+Close_same+DBH_C+Tree_Den,dat),
  
  Close_same %~~% Dist_C,
  DBH_C %~~% Dist_C,
  Tree_Den %~~% Dist_C,
  DBH_C %~~% Close_same,
  Tree_Den %~~% DBH_C,
  S_DBH %~~% DBH
)

s <- summary(model, conserve = TRUE)

options(max.print=10000)
sink(file = "psem_output.txt")
s
sink(file = NULL)
options(max.print=1000)


# Sensitivity Analysis

A sensitivity analysis is a technique used to understand how changes in the inputs of a mathematical model or system affect the output. In the context of a model, such as a simulation or a statistical model, sensitivity analysis helps to identify which input parameters have the most significant influence on the results.

To begin load the model and dataset we will use for this workshop:
```{r, include=T}
load( 'data/SensitivityProducts.RDATA')
```
We will use the following packages:
```{r, include=T}
library(randomForest)
library(tidyverse)
library(gtools)
library(ggplot2)
```

In this workshop, we will prepare a sensitivity analysis for the model FCH4_F_gC.rf. Take a look at the model:

```{r, include=T}
FCH4_F_gC.rf 
```
The model includes monthly precipitation (P_f), mean temperature (TA_F), and a binary indicator for upland (1= upland ecosystem and 0 = aquatic ecosystem).

Explore the conditions present within the dataset (fluxnet.new) used to build the model. First subset only the variables used in the final model:

```{r, include=T}
model.vars <- fluxnet.new %>% as.data.frame %>%  select( P_F, TA_F,Upland)
model.vars %>% head
```
Next, we will summarize the conditions within the dataset by quantiles (0.25, 0.5, 0.75): note that your categorical varibles need to be listed in .by=c().

```{r, include=T}
model.vars.lower <- model.vars %>% reframe(.by =Upland, P_F = quantile(P_F, 0.25),TA_F = quantile(TA_F, 0.25 ), Quantile = as.factor(0.25)) %>% as.data.frame()

model.vars.lower %>% head

model.vars.median <- model.vars %>% reframe(.by =Upland, P_F = quantile(P_F, 0.5), TA_F = quantile(TA_F, 0.5 ), Quantile = as.factor(0.5)) %>% as.data.frame()

model.vars.median %>% head

model.vars.upper <- model.vars %>% reframe(.by =Upland, P_F = quantile(P_F, 0.75), TA_F = quantile(TA_F, 0.75 ), Quantile = as.factor(0.75)) %>% as.data.frame()

model.vars.upper %>% head

````

Combine the individual Summaries into one dataframe:

```{r, include=T}
summary <- smartbind( model.vars.lower, model.vars.median, model.vars.upper)
summary
```
Now, choose the variable you want to explore: TA_F

Look at the conditions present witing the dataset for TA_F
```{r, include=T}
summary(fluxnet.new$TA_F)

```

Look at the range in values for TA_F:
```{r, include=T}
range(fluxnet.new$TA_F)
```

Access each individual range value: 
```{r, include=T}
range(fluxnet.new$TA_F)[1]

range(fluxnet.new$TA_F)[2]
```
Use the range to generate a sequence of values going from the highest to lowest:
```{r, include=T}
TA_F.seq <- seq(range(fluxnet.new$TA_F)[1], range(fluxnet.new$TA_F)[2], by=5 )
TA_F.seq
```
Create a dataframe:
```{r, include=T}
TA_F.seq.df <- data.frame(TA_F = TA_F.seq )
TA_F.seq.df
```
Next we will take the summary, remove the values for TA_F and replace is with the generated range:

```{r, include=T}
TA_F_1AT <- summary %>% select(-TA_F) %>% cross_join(TA_F.seq.df)
TA_F_1AT
```
In this dataframe, all other variables in the model are held at their quantile values and only temperature varies across the range of values observed. This approach is "one-at-a-time". We choose one variable to vary and hold all others at a specific value. Often the mean is chosen but using the 0.25 , 0.5, and 0.75 quantile helps to visualize what the model would predict across conditions. 

Next, use the predict() function to predict values into the dataframe (TA_F_1AT) with the simulated conditions:

```{r, include=T}
TA_F_1AT$PRED <-predict(FCH4_F_gC.rf , newdata =  TA_F_1AT)
TA_F_1AT$PRED
```
Lets look at the predictions.

```{r, include=T}
ggplot() + geom_point(data =TA_F_1AT , aes( x=TA_F, y=PRED) )
```
Use geom_smooth to visualize the general relationship:

```{r, include=T}
ggplot() + geom_smooth(data =TA_F_1AT , aes( x=TA_F, y=PRED) )
```
Look at the predictions by Quantile and by the Upland indicator:

```{r, include=T}
ggplot() + geom_line(data =TA_F_1AT , aes( x=TA_F, y=PRED, color=Quantile, group=interaction(Quantile, Upland), linetype =Upland)  )
```
Now explore the next variable: PA_f
```{r, include=T}
summary(fluxnet.new$P_F)

range(fluxnet.new$P_F)

range(fluxnet.new$P_F)[1]

range(fluxnet.new$P_F)[2]

P_F.seq <- seq(range(fluxnet.new$P_F)[1], range(fluxnet.new$P_F)[2], by=100 )

P_F.seq.df <- data.frame(P_F = P_F.seq )

P_F_1AT <- summary %>% select(-P_F) %>% cross_join(P_F.seq.df)

P_F_1AT$PRED <- predict(FCH4_F_gC.rf , newdata =  P_F_1AT)

ggplot() + geom_point(data =P_F_1AT , aes( x=P_F, y=PRED) )

ggplot() + geom_smooth(data =P_F_1AT , aes( x=P_F, y=PRED) )

ggplot() + geom_line(data =P_F_1AT , aes( x=P_F, y=PRED, color=Quantile, group=interaction(Quantile, Upland), linetype =Upland)  )

```
Use this same workflow to explore the sensitivity of your groups model. Please provide a presentation explaining how you fit your model (mtry?, ntree?) and how variables where selected (forward versus backward selection). Describe your final model results (variables in the final model, their importance, the %Var explained, observed versus predicted for testing and training data), including a correlation plot, variance importance plots, and sensitivity analyses. This report will develop into the methods and results portion of your final project. 
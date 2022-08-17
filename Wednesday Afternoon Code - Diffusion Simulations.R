# Diffusion simulation models in R
# Author: Jake Fisher

# This lab will focus on how to simulate a diffusion process in R.  We will
# cover:
# - Setting up a diffusion simulation
# - Changing different parameter settings
# - Presenting the results

# Following best practices, we begin by setting up the workspace
rm(list = ls())
setwd("C:/Users/fishe/Box Sync/Home Folder jcf26/SNH2017/Instructor dropbox")
library(statnet)
library(tidyverse)
library(magrittr)
library(ggnetwork)

# Dedicated diffusion packages
library(EpiModel)
library(netdiffuseR)

# Typically, the purpose of a diffusion simulation is to illustrate the effects
# of a new diffusion process, or a new type of network structure, on the
# ultimate results of a diffusion process.  As such diffusion simulations are 
# highly variable -- after all, the point for each one is to show something new!

# However, despite the variability, most diffusion simulations build on a
# common set of building blocks:
# - an underlying network structure,
# - a transmission process, and
# - a running timer.
# In essence, we are defining an algorithm, or a set of rules, for how people
# get "infected" with a social contagion, and looking at how those rules perform
# under differnt conditions.

# To illustrate this, I will show a few examples of diffusion models.  These are
# just an introduction to what you can do; the sky is the limit!

##### Example 1: Disease epidemics ####
# The most straightforward and common use for a diffusion simulation is to
# simulate an epidemic spreading through a population.  Some examples of this
# include:
# Moody, James. 2002. "The Importance of Relationship Timing for Diffusion." 
#      Social Forces 81(1):25-56.
# Merli, M.Giovanna, James Moody, Joshua Mendelsohn, and Robin Gauthier. 2015.
#      "Sexual Mixing in Shanghai: Are Heterosexual Contact Patterns Compatible 
#      With an HIV/AIDS Epidemic?" Demography 52(3):919-42.

# In the example here, we will simulate an epidemic spreading through a random
# network.  This roughly approximates a "random mixing" scenario, where people
# interact randomly.

# To simulate random mixing, we construct the following rules:
# 1. Set up a world with a fixed number of people and a small fraction of 
#    initial infections, arranged randomly as a network
# 2. Pick an edge at random
# 3. If the edge is discordant (a susceptible-infected connection), flip a coin
#    to determine whether the non-infected person gets infected
# 4. Repeat steps 1 and 2 until everyone is infected, or until a certain number
#    of steps has passed

# First, in our simple example, let's set our parameters ahead of time
n.people <- 100
p.infection <- 0.5
pct.starting.infected <- 0.01
max.time <- 5000
contact.rate <- 0.05 / 2  # prob. of contact between any 2 people in the network

### Step 1: Set up world ###
# Create a random graph, where edges are placed randomly.  This is called a
# Bernoulli or an Erdos-Renyi random graph
set.seed(919)
net <- rgraph(n = n.people, tprob = contact.rate) %>%
  symmetrize %>%  # Make symmetric -- doesn't matter who is connected to who
  network(matrix.type = "adjacency")  # convert to statnet

# Chose a subset of people who are infected initially
infected <- sample(
  x = c(T, F),      # people can be infected (T) or susceptible (F)
  size = n.people,  # create a vector that is n.people long
  replace = T,      # with replacement, so that >1 person can be infected
  prob = c(pct.starting.infected, 1 - pct.starting.infected)
  )

### Step 2: Choose an edge ###
# For each step, we're going to choose an edge at random, and then, if the edge
# is discordant, flip a coin to determine whether the susceptible person gets
# infected.

# First, create an edgelist...
el <- as.edgelist(net) %>%
  as.data.frame %>%
  set_names(value = c("from", "to")) %>%
  
  # ... attach the values of infected...
  mutate(from.infected = infected[from], to.infected = infected[to],
         
         # ... and create a discordant variable
         discordant = (from.infected != to.infected))

# Next, choose an edge at random
random.edge <- sample(nrow(el), size = 1)

# Check if the edge is discordant
el[random.edge, "discordant"]  # it's not, so we do nothing.

# For the example, I'm going to speed this up by choosing a discordant edge
discordant.edge <- sample(which(el$discordant), size = 1)

### Step 3: Flip a coin to see if the person gets infected ###

# Now, flip a coin to see if the uninfected person gets infected
el[discordant.edge, ]  # Person 62 is the suceptible, but we will want to be
                       # able to determine that without looking manually

# A little tricky indexing to pull out the ID of the susceptible person...
who.susceptible <- with(
  el[discordant.edge, ],
  c(from, to)[!c(from.infected, to.infected)]
  )

# Flip the coin to determine if infection spreads (it does)
(infected[who.susceptible] <- sample(c(T, F), size = 1, 
                                    prob = c(p.infection, 1 - p.infection)))

### Step 4: Repeat ###
# To repeat this process, we actually embed steps 1 and 2 in a loop.

# Set up a list with the output
infections <- vector(length = max.time, mode = "list")

# Save what we already did as the first iteration
infections[[1]] <- infected

# Quick aside -- what did we create?
head(infections)

# Drop the "from.infected", "to.infected", and "discordant" columns from el,
# because they'll actually change with every iteration
el %<>% select(-from.infected, -to.infected, -discordant)

# Next, run the loop
set.seed(27708)
for (t in 2:max.time) {
  infections[[t]] <- infections[[t - 1]]
  
  # Pick an edge at random
  random.edge <- sample(nrow(el), size = 1)
  
  # If the edge is discordant, flip a coin to decide if the infection spreads
  if (with(el[random.edge, ], 
           infections[[t]][from] != infections[[t]][to])) {
    
    who.susceptible <- with(
      el[random.edge, ],
      c(from, to)[!c(infections[[t]][from], infections[[t]][to])]
      )
    
    infections[[t]][who.susceptible] <- sample(
      c(T, F), 
      size = 1, 
      prob = c(p.infection, 1 - p.infection)
    )
  }
}


# Now we have a list of who was infected at what time point.  Let's combine
# that into a data.frame, so we can work with it more easily
(results <- infections %>%
  lapply(FUN = as.data.frame) %>%
  lapply(FUN = set_names, value = "infected") %>%
  bind_rows(.id = "t") %>%
  mutate(id = rep(1:network.size(net), times = max.time),
         t = as.integer(t)) %>%
  tbl_df)

# This dataset is the raw results of our simulation, but it's usually easier to
# look at a summary.  Let's look at the number of people infected over time
infections.by.time <- results %>%
  group_by(t) %>%
  summarize(n.infections = sum(infected)) %>%
  mutate_each(funs(as.numeric), t, n.infections)

# Aside: in R, there are many ways to solve the same problem.  We could have
# gone directly from the raw data to the summaries using the apply functions:
# infections.by.time <- data.frame(
#   t = 1:max.time,
#   n.infected = sapply(infections, sum)
# ) 

# Plotting this relationship shows the 
qplot(data = infections.by.time, x = t, y = n.infections, geom = "line")

# Or, alternatively, we could look at whether people who are more central
# are infected sooner
time.to.infection <- results %>%
  group_by(id) %>%
  summarize(time.infected = min(t[infected])) %>%
  arrange(id) %>%
  mutate(indegc = degree(net))

ggplot(time.to.infection, aes(x = indegc, y = time.infected)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# For fun, let's plot a few slices of the network in time
set.seed(330)
net.layout <- ggnetwork(net) %>% 
  rename(id = vertex.names)

net.layout.by.time <- split(results, f = results$t) %>%
  lapply(FUN = right_join, y = net.layout, by = "id") %>% 
  bind_rows

net.layout.by.time %>% 
  filter(t %in% c(1, 100, 250, 500, 750, 1000)) %>%
  ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(aes(color = infected)) + 
  facet_wrap(~ t) + 
  theme_blank()

# We could animate this with gganimate... but it doesn't work on R v. 3.4.  See
# https://raw.githubusercontent.com/jalapic/rmeetup_examples/master/ggnetwork_gganimate.R
# for example code.

# The statnet team developed a new package, called EpiModel, to simplify running
# epidemic diffusion models.  Tutorials are online here:
# http://www.epimodel.org/, and we'll walk through one to illustrate.

# Set the parameters ahead of time
param <- param.icm(
  inf.prob = 0.2,  # probability of contracting an infection in a step
  act.rate = 0.25  # speed with which people interact
  )
init <- init.icm(
  s.num = 500,  # number of people who are infected initially
  i.num = 1     # number of people who are susceptible initially
  )
control <- control.icm(
  type = "SI",  # type of model, vs., e.g., SIR or SIS
  nsims = 10,   # number of simulations to run
  nsteps = 300  # number of steps per simulation
  )

# Run the model.  ICM means individual contact model (vs. a deterministic model,
# where the results are determined by a set of differential equations)
set.seed(919)
mod <- icm(param, init, control)

# Now we can look at some summaries of the model
# Look at the state of the model at a particular time slice
summary(mod, at = 125)  

# Look at a dataset with the state of the model at a given time
mod %>%
  as.data.frame(mod = "mean") %>%
  tbl_df

# Get a plot of the number of infections by time
plot(mod)

# We can customize the plot, showing the individual simulations, for example
plot(mod, y = "i.num", sim.lines = TRUE, mean.smooth = FALSE, 
     qnts.smooth = FALSE)

# The package is quite flexible.  It can incorporate demographic changes,
# different network structures, and even custom functions for each of the 
# components of the model (like different mortality schedules).  Check the 
# website for more examples of how to do this.

##### Example 2: Diffusion of innovations #####
# The second example is the diffusion of innovations.  The process by which
# innovations spread is less well-understood.  We usually simulate it with a 
# process that's very similar to diseases diffusing.

# In this case, we will focus on a "complex contagion", meaning something you
# have to hear about from multiple people before you will adopt it.

# Setup
# 1. Construct a small world network
# 2. Choose one person at random, and set him/her and his/her neighbors as 
#    initial adopters
# 3. Infect all the people who have more than tau neighbors infected
# 4. Set the people who were infected at the previous round as "recovered"
# 5. Repeat steps 

### Set parameters in advance ###
n <- 50
tau <- 2 / 6
max.time <- 50

### Step 1: Construct network ###
# Construct a small world network
set.seed(919)
sw.net <- igraph::sample_smallworld(1, n, 2, p = .2) %>%
  intergraph::asNetwork(.) 

### Step 2: Choose a person & neighbors at random as an initial adopter ###
# Set up vector to indicate adoption
adopters <- rep(F, n)

# Choose a person at random
initial.adopter <- sample(seq_len(n), size = 1)

# Get the list of people they're attached to
initial.neighbors <- get.neighborhood(sw.net, initial.adopter)

# Set them all as "adopters"
adopters[c(initial.adopter, initial.neighbors)] <- T

### Step 3: infect all people who have more than tau neighbors infected ###

# Let's look at one person, person 18, to see how this works
ego.extract(sw.net, ego = 20, neighborhood = "out")

# Person 18 hasn't adopted
adopters[20]

# About half of their neighbors have adopted
adopters[c(18, 19, 22, 25)]
mean(adopters[c(18, 19, 22, 25)])

# To decide whether the person adopts, we test whether the fraction of
# adopters is greater than tau
mean(adopters[c(18, 19, 22, 25)]) >= tau  # adoption!

# We can update everyone simultaneously using matrix multiplication
adj.mat <- sw.net[, ]
diag(adj.mat) <- 0  # set the diagonal to 0, b/c people don't weight themselves

# We could take the sum...
adj.mat %*% adopters

#... or the percentage
(adj.mat.rn <- adj.mat / rowSums(adj.mat))
adj.mat.rn %*% adopters

# And then we calculate the people who are above our threshold
(adj.mat.rn %*% adopters) >= tau

# Note that this actually allows people to abandon the innovation if enough of
# of their neighbors are not adopters.  For now we don't want that to happen, 
# so we'll only test people who are not yet adopters
ifelse(adopters, TRUE, ((adj.mat.rn %*% adopters) >= tau))

### Step 4: Repeat ###
# Again, we can take care of this by wrapping it in a loop
adopt <- vector(mode = "list", length = max.time)
adopt[[1]] <- adopters

for (t in 2:max.time) {
  adopt[[t]] <- ifelse(adopters, TRUE, ((adj.mat.rn %*% adopt[[t - 1]]) >= tau))
}

# Note that again we get the characteristic S-shaped curve:
data.frame(
  t = 1:max.time,
  n.adopt = sapply(adopt, sum)
) %>%
  ggplot(aes(x = t, y = n.adopt)) + 
  geom_line()

# Let's plot a few frames

set.seed(330)
sw.net.layout <- ggnetwork(sw.net) %>% 
  rename(id = vertex.names)

sw.net.layout.by.time <- adopt %>%
  lapply(FUN = as.data.frame) %>% 
  lapply(FUN = set_names, value = "adopter") %>% 
  lapply(FUN = mutate, id = 1:n) %>%
  lapply(FUN = right_join, y = sw.net.layout, by = "id") %>% 
  bind_rows(.id = "t") %>% 
  mutate(t = as.integer(t))

sw.net.layout.by.time %>% 
  filter(t < 10) %>% 
  ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(aes(color = adopter)) + 
  facet_wrap(~ t) + 
  theme_blank()

# There's a new package for analyzing the diffusion of innovations.  It focuses
# mainly on creating survival models for diffusion data (and has a different
# data structure) so we won't cover it here.  But it's well worth checking out
# the vignette:
vignette("analyzing-medical-innovation-data")


##### Example 3: Averaging ideas #####
# A third common approach use for diffusion simulation is to suggest how
# people's beliefs change through interaction with others.  A longstanding
# assumption is that people take the weighted average of their friends.

# Set the parameters in advance
self.weight <- 0.2
max.time <- 5

# Load the data and save a few values for convenience
load("faux_add_health_with_faux_attitude.Rdata")

n.people <- network.size(add.health)  
attitude <- (add.health %v% "faux.attitude")
adj.mat <- add.health[, ]  # save the adjacency matrix to its own object

# Plotting the network shows that attitude values are randomly distributed in
# this network
set.seed(27708)
net.layout <- ggnetwork(add.health)

p <- ggplot(net.layout, aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "lightgray", 
             arrow = arrow(length = unit(6, "pt"), type = "closed")) + 
  theme_blank() + 
  scale_color_distiller(palette = "Spectral")

p + geom_nodes(aes(color = faux.attitude), size = 4)

# In general, we assume that people update their attitude using the average of
# their neighbors' opinions, and their own opinion.  For example, person 3 names
# 4 friends: people 6, 29, 34, and 40
ego.extract(add.health, ego = 3, neighborhood = "out")

# Those people have 4, 2, 3, and 2 as their attitudes
attitude[c(6, 29, 34, 40)]

# Their mean value is 2.75
mean(attitude[c(6, 29, 34, 40)])

# Then we assume that people take the weighted average of their own attitudes
# and their friends' attitudes
self.weight * attitude[3] + (1 - self.weight) * mean(attitude[c(6, 29, 34, 40)])

# We could repeat this for everyone individually, but this can actually be done
# by matrix multiplication, which makes it much easier to program.

# First, we have to row-normalize (set rows so that they sum to 1 by dividing
# each row by its sum) the adjacency matrix
diag(adj.mat) <- 0  # set the diagonal to 0

(adj.mat.rn <- adj.mat / rowSums(adj.mat))  # note the missing values!
                                            # Isolates => dividing by 0

# NaN does not play nicely with other values, so swap it for 0 in the row-
# normalized matrix.
adj.mat.rn[which(is.nan(adj.mat.rn), arr.ind = T)] <- 0

# In general, we assume that isolates do not change under this model.
# Mechanically, we can either remove them from the analysis or we can set their
# self-weights to 1.

# Identify outdegree isolates
isos <- (degree(add.health, cmode == "outdegree") == 0)

# Set self-weights to 1:
self.weight.vec <- rep(self.weight, times = n.people)
self.weight.vec[isos] <- 1

# Remove isolates
self.weight.no.iso <- self.weight.vec[!isos]
adj.mat.no.iso <- adj.mat.rn[!isos, !isos]
attitude.no.iso <- attitude[!isos]

# Now run the simulation...

# ... with self-weights of isolates = 1
I <- diag(n.people)  # identity matrix
SW <- diag(self.weight.vec)  # diagonal matrix with self-weights on diagonal

(adj.mat.rn %*% (I - SW) + SW) %*% attitude

# ... removing the isolates
I.no.iso <- diag(sum(!isos))
SW.no.iso <- diag(self.weight.vec[!isos])

(adj.mat.no.iso %*% (I.no.iso - SW.no.iso) + SW.no.iso) %*% attitude.no.iso

# Similarly to the other simulations, we can repeatedly perform this procedure
# using a for loop:
sim.attitude <- vector(mode = "list", length = max.time)
sim.attitude[[1]] <- attitude

for (t in 2:max.time) {
  sim.attitude[[t]] <- (adj.mat.rn %*% (I - SW) + SW) %*% sim.attitude[[t - 1]]
}

# Now let's plot the values by time, to show how the diffusion process unfolds

# Convert each list element to a data frame that ggplot can use, & combine them
layout.with.attitudes <- sim.attitude %>%
  lapply(FUN = as.data.frame) %>%
  lapply(FUN = set_names, value = "attitude.value") %>%
  lapply(FUN = mutate, vertex.names = seq_len(n.people)) %>%
  lapply(FUN = right_join, y = net.layout, by = "vertex.names") %>%
  bind_rows(.id = "iteration")

# Plot it, with facets to show how this changes over each iteration
p %+% 
  layout.with.attitudes +
  geom_nodes(aes(color = attitude.value), size = 4) +
  facet_wrap(~ iteration)
  
# A package that's under development, latentnetDiffusion, takes care of the
# matrix multiplication and reshaping of the data for you
devtools::install_github("jcfisher/latentnetDiffusion")
library(latentnetDiffusion)
library(latentnet)

# For example, to run the same simulation as above, use:
(sim.attitudes.2 <- degroot(W = add.health, Y = as.matrix(attitude), 
                            all.iter = T, self.weight = 0.8) %>%
  tidyDegroot(id = seq_len(n.people)) )

# Or, we can fit a latent space model, and simulate the diffusion process over
# the latent space:
# fit <- latentnet::ergmm(add.health ~ euclidean(d = 2))  # takes a minute!
# save(fit, file = "ergmm_2d_fit.Rdata")
load("ergmm_2d_fit.Rdata")

(ls.attitudes <- ergmmDegroot(fit, Y = as.matrix(attitude), simulate = F, 
                             all.iter = T) %>%
  tidyDegrootList(id = seq_len(n.people)))


# Plot it, to show the difference
layout.with.ls.attitudes <- ls.attitudes %>%
  rename(vertex.names = id) %>% 
  {split(., f = .$iteration)} %>% 
  lapply(FUN = right_join, y = net.layout, by = "vertex.names") %>%
  bind_rows(.id = "iteration")

p %+% 
  layout.with.ls.attitudes +
  geom_nodes(aes(color = pred.value), size = 4) +
  facet_wrap(~ iteration)
  
# The same model, simulated:
set.seed(919)
(ls.attitudes.sim <- ergmmDegroot(fit, Y = as.matrix(attitude), simulate = T, 
                                  all.iter = T) %>%
    tidyDegrootList(id = seq_len(n.people)))
  
# Plotted again
layout.with.ls.attitudes.sim <- ls.attitudes.sim %>%
  rename(vertex.names = id) %>% 
  {split(., f = .$iteration)} %>% 
  lapply(FUN = right_join, y = net.layout, by = "vertex.names") %>%
  bind_rows(.id = "iteration")

p %+% 
  layout.with.ls.attitudes.sim +
  geom_nodes(aes(color = pred.value), size = 4) +
  facet_wrap(~ iteration)

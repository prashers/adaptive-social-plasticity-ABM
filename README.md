# Agent-Based Decision-Making Simulation
## TL;DR

This project uses an agent-based simulation and analytics workflow to test how different decision-making strategies affect social interactions with influential agents in a resource-constrained environment. I ran large-scale parameter experiments (1,000 scenarios; 5,000+ observations) and analyzed the results using SQL, R, and statistical modeling to uncover nonlinear patterns and trade-offs between strategies. The project demonstrates my ability to design experiments, build scalable data pipelines, and translate complex system behavior into insights that are relevant to real-world problems.



## Overview

This project uses an agent-based simulation to explore how simple decision-making rules influence behavior and interaction patterns in environments with limited resources. By varying how agents observe, remember, and respond to others, the model shows how individual choices can scale up to complex group-level outcomes.

The project pairs simulation modeling with a structured analytics workflow to test behavior across a wide range of scenarios.



## Problem Addressed

When individuals make decisions based on incomplete information and limited memory, it’s unclear which strategies lead to better individual and group outcomes under constrained conditions. This project uses simulation to systematically test how different capacities or tendencies to observe, remember, and respond to the actions of others shape interaction patterns and performance outcomes.



## What I Built

- An agent-based simulation where agents search for and access a shared resource (a food patch)

- A parameter testing framework evaluating 1,000 combinations of agent/group characteristics (group size and the ability of agents to perceive, remember, and act on the actions of other agents) across repeated runs

- An end-to-end analytics pipeline using SQL and R to store, query, and analyze simulation output

- A visualization and modeling workflow to compare outcomes across scenarios

The simulations produced datasets with 5,000+ observations, supporting systematic comparison across conditions.



## Analysis Approach

The analysis focused on two main outcomes:
(1) changes in interaction connectivity (who becomes more connected in the network of agents - based on spatial proximity), and
(2) food intake.

These measures were tracked across simulation phases to evaluate how different decision rules performed over time.

Simulation results were analyzed in R using visualization and statistical modeling to identify patterns and nonlinear relationships between parameters and outcomes. Generalized additive models (GAMs) were used to explore interaction effects, with model checks used to assess fit and reliability.



## Key Results

The analysis showed that agent characteristics interact to influence the capacity of agents to respond to, and benefit from, the actions of others. Intermediate parameter values, reflecting flexible use of information from others, often produced better outcomes for agents than extreme strategies. These results highlight how flexible decision-making can outperform rigid strategies in constrained environments. In particular, strategies that balanced responsiveness to others with flexibility over time often outperformed both rigid following (i.e., always responding to the actions of others) and purely independent behavior (i.e., never responding to the actions of others).

Findings from this work were published in a peer-reviewed journal, validating the modeling and analysis approach. 

You can find the paper here: https://doi.org/10.1093/icb/icaf126 



## Relevance to Data & Product Analytics

This project demonstrates analytical skills that are directly transferable to data analyst, product analytics, and product data science roles, including:

- **Experimental design under constraints:** Designing controlled experiments to evaluate strategy trade-offs when resources, information, or capacity are limited.

- **Large-scale scenario testing:** Running and managing parameter sweeps across hundreds of scenarios to understand sensitivity, interactions, and nonlinear effects.

- **Outcome definition and measurement:** Translating abstract system behavior into concrete, measurable outcomes (e.g., connectivity, performance) that can be tracked over time.

- **Statistical modeling of complex systems:** Using flexible models (e.g., GAMs) to capture nonlinear relationships and interactions that are common in real-world data.

- **End-to-end analytics workflows:** Building reproducible pipelines to store, query, analyze, and visualize data using SQL and R.

- **Insight communication:** Interpreting technical results to explain why certain strategies outperform others and what trade-offs decision-makers should consider.



## Technical Stack

- Agent-based modeling

- R (data analysis, visualization, statistical modeling)

- SQL (querying simulation output)

- Simulation experimentation


## Skills Demonstrated

- Simulation-based analysis

- Experimental design and parameter sweeps

- Statistical modeling of nonlinear systems

- Data pipeline design

- Reproducible analytics workflows


## Repo Contents
"quail_centrality_4_50_setup.Rmd" contains the code used for data cleaning. The data_ord_50.csv data file used in this script is available in FigShare: https://doi.org/10.6084/m9.figshare.29474114.v1

"quail_centrality_4_50_analysis.Rmd" contains the code used for all analyses

"manuscript plots.R" contains the code used to produce all plots

"quail_centrality_4.nlogo" is the netlogo file containing the code for the agent-based model


### Data Schema (Simulation Output)
The data_ord_50.csv file on FigShare contains the following columns:
  
 "run" is the model run ID - a different number for each run of the model. 
  
 "group.size" is one of the five parameters that can vary between run IDs. It sets the number of agents for each model run
  
 "mem" is one of the five parameters that can vary between run IDs. It sets the maximum number of time steps that a successful forager can be remembered by scroungers
 
 "attention" is one of the five parameters that can vary between run IDs. It sets the probability that agents will enter a successful forager into memory if their memory slot is empty.
 
 "preference" is one of the five parameters that can vary between run IDs. It sets the probability that agents will follow the successful forager in their memory.
 
 "approachfood" is one of the five parameters that can vary between run IDs. It is referred to as the asocial-information parameter in the ICB paper. It determines whether foragers other than the producer approach food when their energy level gets low.
 
 "ticks" is the time step within the current model run - as appears in Netlogo
 
 "proximIDs", list of Netlogo WHO numbers (ID numbers) of the agents within proximity of each individual. Lists are ordered by WHO numbers of each agent (i.e. the first list indicates which agents are in proximity to agent 0, the second list indicates which agents are in proximity to agent 1, and so on)
 
 "currentsuccforagers", the list containing IDs of agents that successfully foraged in each time step
 
 "energylist" list containing each agent's energy level in the current time step
 
 "combo.num" number indicating each unique combination of the five parameters that are allowed to vary between runIDs
 
 "phase" character string indicating which phase of the model the row corresponds to

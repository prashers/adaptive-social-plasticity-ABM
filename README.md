# adaptive-social-plasticity-ABM
This repo contains the Netlogo file for the final version of my 'quail_centrality' model and R code for analysis of the model output.

This is the model and code corresponding to my paper in ICB: https://doi.org/10.1093/icb/icaf126

**Overview**

This project uses an agent-based simulation to explore how simple decision-making rules influence behavior and interaction patterns in systems with limited resources. By varying how agents observe, remember, and respond to others, the model shows how individual choices can scale up to complex group-level outcomes.

The project pairs simulation modeling with a structured analytics workflow to test behavior across a wide range of conditions.


**Problem Addressed**

When systems involve many interacting actors, small changes in decision rules can produce large and sometimes unexpected outcomes. This project demonstrates how simulation-based analysis can be used to explore those dynamics in a controlled, repeatable way.


**What I Built**

An agent-based simulation where agents compete for access to a shared resource

A parameter testing framework evaluating 1,000 combinations of decision rules across repeated runs

An end-to-end analytics pipeline using R and SQL to store, query, and analyze simulation output

A visualization and modeling workflow to compare outcomes across scenarios

The simulations produced datasets with 5,000+ observations, supporting systematic comparison across conditions.


**Analysis Approach**

Simulation results were analyzed in R using visualization and statistical modeling to identify patterns and nonlinear relationships between parameters and outcomes. Generalized additive models (GAMs) were used to explore interaction effects, with model checks used to assess fit and reliability.


**Key Outcomes**

The analysis showed that decision-making rules interact in important ways, and that intermediate parameter values often produced better outcomes than extreme strategies. These results highlight how flexible decision-making can outperform rigid strategies in constrained environments.

Findings from this work were published in a peer-reviewed journal, validating the modeling and analysis approach. You can find the paper here: https://doi.org/10.1093/icb/icaf126 


**Business Relevance**

While this project is based on a simulated environment, the methods and insights are directly applicable to business and product analytics problems involving interacting agents and limited resources, such as:

- User behavior modeling: Understanding how visibility, memory, and preference influence engagement or adoption

- Marketplace dynamics: Exploring how information access and following behavior affect outcomes for buyers and sellers

- Operations and logistics: Testing decision rules under capacity constraints before implementing real-world changes

- Product experimentation: Evaluating strategy trade-offs in complex systems where A/B testing may be costly or slow

This project demonstrates my ability to:

- frame complex problems as testable experiments

- design scalable simulation workflows

- analyze large parameter spaces

- translate technical results into actionable insights


**Technical Stack**

- Agent-based modeling

- R (data analysis, visualization, statistical modeling)

- SQL (querying simulation output)

- Simulation experimentation and workflow automation


**Skills Demonstrated**

- Simulation-based analysis

- Experimental design and parameter sweeps

- Statistical modeling of nonlinear systems

- Data pipeline design

- Reproducible analytics workflows


**Repo Contents**
"quail_centrality_4_50_setup.Rmd" contains the code used for data cleaning. The data_ord_50.csv data file used in this script is available in FigShare: https://doi.org/10.6084/m9.figshare.29474114.v1

"quail_centrality_4_50_analysis.Rmd" contains the code used for all analyses

"manuscript plots.R" contains the code used to produce all plots

"quail_centrality_4.nlogo" is the netlogo file containing the code for the agent-based model


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

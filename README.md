# adaptive-social-plasticity-ABM
This repo contains the Netlogo file for the final version of my 'quail_centrality' model and R code for analysis of the model output.

This is the model and code corresponding to my paper in ICB: TKTKlink

"quail_centrality_4_50_setup.Rmd" contains the code used for data cleaning. The data_ord_50.csv data file is available in FigShare: TKTKlink

"quail_centrality_4_50_analysis.Rmd" contains the code used for all analyses

"manuscript plots.R" contains the code used to produce all plots



In quail_centrality_4, I made the following changes (as of Aug 7, 2024):

1. the producer no longer follows the same rules as scroungers because I am modeling a scenario in which the producer knows where the one food source is and how to access it

2. the food patch is now present and accessible to all agents in the first and last phase of the model
        - This way I will be seeing if changing the value of a single individual influences its centrality rather than seeing if centrality changes compared to random interactions
        - This is important bc we already know that complex grouping patterns can result from individual responses to resource distribution (e.g., Ramos-Fernandez et al. 2006) - so in the previous version I could just be seeing effects of a concentrated resource on changing centrality in proximity network
        - NOTE: NOW ACTIVITY IN THE FORAGING PHASE WOULD NOT IMPACT AGENT MOVEMENT WHEN MEMORY IS LONGEST BC WHO THEY REMEMBER (a successful forager from the first phase) IS FIXED
        
3. the food patch is depleted and disappears for 5 time steps in the first and third phases. It reappears when any of the agents steps on one of the food patches by chance after those 5 time steps (if I don't have a few time steps in between, I think it would reset immediately because there would probably be an agent on the food patch already)
        - if I did not add this, food would be resetting and different group sizes would no longer have different levels of competition in the pre- and post-foraging phases
        - this also incorporates higher chance of larger groups resetting the food patch in the pre- or post-foraging phase since there are more agents that can potentially step on a food patch
        - this change, in addition to #2, is meant to make the comparison of producer strength between the foraging phase and other phases more valid since the only difference between phases in the current model is that the producer is the only one who can reset the food patch in the foraging phase
		
4. Adjusted movement options for foragers when the approach-food? switch is on -- they now move toward food patch if their energy is very low and the food is accessible (food patch is green)
        - I think this better reflects scrounging behavior - agent moves straight to an available food source without regard for how the food became accessible
        - This means agents always know where accessible food is
        - attention/preference/memory still only control whether agents follow the successful forager they saw when they are not hungry or there is no accessible food

5. Added a switch to control whether reset threshold is consistent or changes across group sizes

6. Rounded coordinates to 5 decimal places to reduce final csv file size (using the 'precision' command)

7. The arena will resize to keep group density constant across group sizes if the 'resize-arena?' switch is on
        - the foraging patch remains the same size for all group sizes
        - The thing I want to isolate with different group sizes is level of competition (fewer opportunities to feed with increasing group size)

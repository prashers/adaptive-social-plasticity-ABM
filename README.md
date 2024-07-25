# quail-centrality-ABM
This repo contains the Netlogo file for (hopefully) the final version of my 'quail_centrality' model and R code for analysis of the output

In quail_centrality_4, I am planning to make the following changes (as of July 23, 2024):

1. the producer no longer follows the same rules as scroungers because I am modeling a scenario in which the producer knows where the one food source is and how to access it

2. the food patch is now present and accessible to all agents in the first and last phase of the model
        - This way I will be seeing if changing the value of a single individual influences its centrality rather than seeing if centrality changes compared to random interactions
        - This is important bc we already know that complex grouping patterns can result from individual responses to resource distribution (e.g., Ramos-Fernandez et al. 2006) - so in the previous version I could just be seeing effects of a concentrated resource on changing centrality in proximity network
        - NOTE: NOW ACTIVITY IN THE FORAGING PHASE WOULD NOT IMPACT AGENT MOVEMENT WHEN MEMORY IS LONGEST BC WHO THEY REMEMBER (a successful forager from the first phase) IS FIXED
        
3. the food patch is depleted and disappears for x time steps in the first and third phases - otherwise it would be resetting and different group sizes would no longer have different levels of competition
		
4. Added separate scrounging movement (scroungers move straight to food if 1. they are hungry, 2. they perceived a successful forager, and 3. food is accessible)
        - This separates scrounging from following more (this is not meant to be a producer-scrounger model, but a model looking at how non-foraging interactions may change in response to an individual's foraging activity), but if they can always go straight to food when available, there may be no influence of following on foraging success

5. Added a switch to control whether reset threshold is consistent or changes across group sizes

6. Rounded coordinates to 5 decimal places to reduce final csv file size (using the 'precision' command)

7. The arena will resize to keep group density constant across group sizes if the 'resize-arena?' switch is on
        - the foraging patch remains the same size for all group sizes
        - The thing I want to isolate with different group sizes is level of competition (fewer opportunities to feed with increasing group size)
        - MAYBE CHOOSE A FEW PARAMETER COMBINATIONS TO TEST WHETHER CHANGING ARENA SIZE WOULD HAVE AN EFFECT (IF NOT THEN NOT NECESSARY TO DO A FULL PARAMETER SWEEP)


TO BE DETERMINED:

8. How can I replace arbitrary values in code with values that make more sense (e.g., resource-level and reset-thresholds)
        - I think I just used values that seemed to give agents time to feed
        - THESE WOULD VARY A LOT BASED ON THE BIOLOGICAL SYSTEM - YOU COULD DO A TEST PARAMETER SWEEP WITH DIFFERENT VALUES FOR THIS TOO - IF IT WOULD BE HELPFUL
# quail-centrality-ABM
This repo contains the Netlogo file for (hopefully) the final version of my 'quail_centrality' model and R code for analysis of the output

In quail_centrality_4, I am planning to make the following changes (as of July 23, 2024):

1. the producer no longer follows the same rules as scroungers because I am modeling a scenario in which the producer knows where the one food source is and how to access it

2. the food patch is now present and accessible to all agents in the first and last phase of the model
        - This way I will be seeing if changing the value of a single individual influences its centrality rather than seeing if centrality changes compared to random interactions
        - This is important bc we already know that complex grouping patterns can result from individual responses to resource distribution (e.g., Ramos- Fernandez et al. 2006) - so in the previous version I could just be seeing effects of a concentrated resource on changing centrality in proximity network
        - NOTE: NOW ACTIVITY IN THE FORAGING PHASE WOULD NOT IMPACT AGENT MOVEMENT WHEN MEMORY IS LONGEST BC WHO THEY REMEMBER (a successful forager from the first phase) IS FIXED
		
3. Added separate scrounging movement (scroungers move straight to food if 1. they are hungry, 2. they perceived a successful forager, and 3. food is accessible)
        - This separates scrounging from following more, but if they can always go straight to food when available, there may be no influence of following on foraging success

4. Added a switch to control whether reset threshold is consistent or changes across group sizes

5. CAN YOU MAKE NETLOGO ROUND THE COORDINATES TO A FEW DECIMAL PLACES TO REDUCE CSV FILE SIZE? (use the 'precision' command)

TO BE DETERMINED:
6. Decide whether to resize the arena for different group sizes to keep space per individual constant
        - Would I also need to resize the foraging patch so it takes up the same proportion of space in each arena?
        - The thing I want to isolate with different group sizes is level of competition (fewer opportunities to feed with increasing group size)
        - But other things that change with group size currently are group density, ratio of scroungers to producer
        - MAYBE CHOOSE A FEW PARAMETER COMBINATIONS TO TEST WHETHER CHANGING ARENA SIZE WOULD HAVE AN EFFECT - IF NOT THEN NOT NECESSARY TO DO A FULL PARAMETER SWEEP

7. How can I replace arbitrary values in code with values that make more sense (e.g., resource-level and reset-thresholds)
        - I think I just used values that seemed to give agents time to feed
        - THESE WOULD VARY A LOT BASED ON THE BIOLOGICAL SYSTEM - YOU COULD DO A TEST PARAMETER SWEEP WITH DIFFERENT VALUES FOR THIS TOO - IF IT WOULD BE HELPFUL
	
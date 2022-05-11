breed [producers producer] ;producer is the only individual able to access yellow patches
breed [foragers forager] ;foragers can only access green patches (food made available by producer)

undirected-link-breed [proxims proxim] ;undirected proximity links between agents that are within a certain distance from each other
directed-link-breed [affils affil] ;directed affiliation links created at model set up (pre-existing relationships)
directed-link-breed [folls foll] ;directed following links (directed from follower toward agent that is being followed)


globals [
  history ;history will be a list showing the "public record" of the individuals (who numbers) that have foraged within the past x ticks
  sub-history ;a subet of the history list containing IDs of individuals that have foraged within a number of  time steps equal to memory
  current-succ-foragers ; current-succ-foragers will keep track of successful foragers in each time step, which will be used to iterate history
  memory-succ-foragers ; memory-succ-foragers will hold the who numbers of foragers who have eaten within the past memory time steps ;this is not used to control agent movements in the current iteration of the model
  prox-centrality-list ; list showing number of other agents in proximity (within a certain radius) to each agent - updated at each time step
  foll-centrality-list ; list showing number of incoming following links that each agent has - updated at each time step
  proxim-IDs ; list containing lists of the who numbers of agents in proximity to each turtle (IDs of proxim link neighbors)
  foll-IDs ; list containing lists of the who numbers of agents following each turtle (IDs of in-foll link neighbors)
  affil-IDs ; list containing lists of the who numbers of agents with affiliation links coming from each turtle (IDs of out-affil neighbors)
  xycor-list ; list containing x and y coordinates of each agent at each time step - updated at each time step
  current-xycor ; object holding the current x and y coordinates of each agent - used to update the xycor-list
  end-timer ; a numeric variable to count the number of time steps since food is depleted
  fss-list ; list containing first-sf-seen value of each turtle
]

turtles-own [ ;Values of each of these variables are unique to each agent
  energy ;energy level - used to control agent movement
  stop-timer ;a numeric variable counting the number of time steps since a forager first paused on a yellow patch
  first-sf-seen ;variable to hold the ID of the first successful forager that is seen by each turtle
  first-sf-seen-timer ;a numeric variable counting the number of time steps since the first-sf-seen variable was set. first-sf-seen is updated when the timer reaches memory time steps
]

patches-own [
  resource-level
  reset-id ;to distinguish between food patches that are always accessible from those that become inaccessible again
  reset-counter
]



to setup
  ca
  reset-ticks

  ;; CREATE AGENTS AND MOVE THEM TO EMPTY PATCHES
  create-producers 1 [ ;create 1 producer in a random patch, set its color to red, set its energy to __
    set color red
    set energy round random-normal 150 10
    set first-sf-seen nobody
    set first-sf-seen-timer 0
  ]

  create-foragers 5 [;create 5 foragers in random patches, set their color to blue, set their energy to __
    set color blue
    set energy round random-normal 150 10
    set stop-timer 0
    set first-sf-seen nobody
    set first-sf-seen-timer 0
  ]

  ask turtles [ ;move to any empty black patch (so that no turtles start off on top of another agent)
    let empty-black-patch one-of patches with [pcolor = black and not any? turtles-here]
    move-to empty-black-patch
  ]


  ;; CREATE INITIAL AFFILIATION LINKS between foragers (exclude producer)
  if prior-affils? [

    ifelse unfam-prod? ;if unfam-prod? switch is on, all foragers will be familiar with each other, except the producer. That means, if anyone forages in the same time step as the producer, that forager will become the first-sf-seen for observers

      [ask foragers [
         create-affils-to other foragers [set color black + 2] ;each forager makes affiliation links directed to each other forager
        ]
      ]

      [ask foragers [ ;else
         repeat random 2 + 1 [create-affil-to one-of other foragers [set color black + 2]] ;each forager makes affiliation links directed to 1 - 3 other foragers
        ]
      ]

    set affil-IDs n-values 6 ["[NA]"]
    ask foragers [
      let affil-turtles (sentence [who] of out-affil-neighbors)
      set affil-IDs replace-item who affil-IDs affil-turtles
      ;print affil-IDs
    ]

  ]


  ;;SET UP VARIABLES
  set history n-values 301 [[]] ;history is a list of 301 (or whatever max number of ticks + 1 is) empty lists
  ;print history
  set memory-succ-foragers []
  set prox-centrality-list n-values 6 [0]
  set foll-centrality-list n-values 6 [0]
  iterate-centrality
  iterate-fss-list

  set xycor-list n-values 301 [n-values 6 [0]]
  iterate-xycor-list

  set end-timer 0


  ;; LEAD IN PERIOD WITHOUT FOOD PATCH
  ifelse alt-food? [
    let food-p (patch-set patch 4 4 patch 4 5 patch 5 4 patch 5 5) ;define food patches that will become the initially inaccessible food
    ask food-p [set resource-level 50]

    let alt-food-p (patch-set patch -4 -4 patch -4 -5 patch -5 -4 patch -5 -5) ;define alternative food patches that will be accessible for all
    ask alt-food-p [set resource-level 50]



  ] ;end of if alt-food?
  [ ;start of else alt-food?
    let central-patches (patch-set patch 0 0 patch 0 1 patch 1 0 patch 1 1) ;define central-patches

    ask central-patches [ ; ask central patches to set their resource-level to 100
      set resource-level 100
    ]



  ] ;end of ifelse alt-food?
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  if ticks = 100 [present-food]
  if ticks = 200 [ask patches [set pcolor black set resource-level 0]]
  if not any? patches with [resource-level > 0] [set end-timer end-timer + 1] ;start counting number of ticks since food ran out
  if end-timer >= 101 [stop] ;stop model 100 ticks after the food has been removed (end-timer must be 101 because it starts at 1 in the first time step that food is removed)


  set current-succ-foragers [] ;an empty list that will be used to keep track of successful foragers in each time step


  ifelse eat-delay?
  [for-actions prod-actions] ;foragers will NOT be able to eat in the same time step as the producer first accesses food - matches a scenario where birds are able to tell who first accessed the food even if it is crowded
  [prod-actions for-actions] ;foragers will be able to eat in the same time step as the producer first accesses food if they are already on a food patch - matches a scenario where birds are unable to tell who made the food accessible


  ask patches with [pcolor != black] [ ;ask patches that are not black to set their color to black if their resource-level reaches zero
    if resource-level <= 0 [set pcolor black]
  ]

  if ticks > 100 and end-timer = 0 and reset-food? [reset-food] ;run reset-food procedure only after lead in period, before  and if reset-food? switch is on


  tick


  if not empty? current-succ-foragers [;I want this to be updated after tick, so that the correct items in the history list get replaced
    iterate-history ;adding IDs of successful foragers to the appropriate item in the history list
    turtle-actions ;updating first-sf-seen for each turtle if appropriate
  ]

  iterate-fss-list
  iterate-memory ;memory should be updated at every time step - I was previously updating it only if current-succ-foragers was not empty, but that would allow foragers to follow others even after food is gone and time steps more than memory have passed (memory list would still have last x number of foragers that were able to eat)
  iterate-centrality
  iterate-xycor-list

end


to present-food ;introducing food patch(es) after 100 timesteps
  ifelse alt-food? [
    let food-p (patch-set patch 4 4 patch 4 5 patch 5 4 patch 5 5) ;define food patches that will become the initially inaccessible food

    let alt-food-p (patch-set patch -4 -4 patch -4 -5 patch -5 -4 patch -5 -5) ;define alternative food patches that will be accessible for all

    ask food-p [
      set pcolor yellow
      set reset-id 1
      set reset-counter 0
      if any? turtles-on food-p [ ;if there are turtles on any of the food patches, ask them to move to one of neighboring black patches so that no one is on the food patch when it is introduced
        ask turtles-on food-p [move-to one-of neighbors with [pcolor != yellow]]
      ]
    ]

    ask alt-food-p [ ;same thing for alternative food patch
      set pcolor green
      set reset-id 2
      set reset-counter 0
      if any? turtles-on alt-food-p [ ;if there are turtles on any of the alternative food patches, ask them to move to one of neighboring black patches so that no one is on the alternative food patch when it is introduced
        ask turtles-on alt-food-p [move-to one-of neighbors with [pcolor != green]]
      ]
    ]
  ] ;end first half of ifelse alt-food?

  [ ;if alt-food? switch is off, do this:
    let central-patches (patch-set patch 0 0 patch 0 1 patch 1 0 patch 1 1) ;define central-patches

    ask central-patches [
      set pcolor yellow
      set reset-id 1
      set reset-counter 0
      if any? turtles-on central-patches [ ;if there are turtles on any of the central patches, ask them to move to one of neighboring black patches so that no one is on the food patch when it is introduced
        ask turtles-on central-patches [move-to one-of neighbors with [pcolor != yellow ]]
      ]
    ]
  ]
end



to prod-actions
  ask producers [
    set label energy

    if first-sf-seen != nobody [ ;if there is a who number saved in first-sf-seen, increase the timer by one until
      set first-sf-seen-timer first-sf-seen-timer + 1
      if first-sf-seen-timer > memory [;if timer exceeds memory value, reset first-sf-seen to no-turtles (first-sf-seen is forgotten and a new one can be remembered)
        set first-sf-seen nobody
        set first-sf-seen-timer 0
        ]
    ]


    let previous-energy energy ;save energy level before moving/accessing to keep track of who has foraged at each time step

    (ifelse
      energy < 50 and [pcolor] of patch-here != black [access];if your energy level is less than 50 and resource-level of patch you are on is greater than zero, then access the food
      energy <= 15 and any? patches with [pcolor != black] [move-to-patch] ;if your energy level is 15 or below, move-to-patch
      [;if neither one of the energy conditions are met:
        (ifelse not eat-delay? [movement];If eat-delay? is off producer should movement (follow same movement rules as other foragers)
        alt-food? [movement] ;If alt-food? is on, producer should movement (follow same movement rules as other foragers)
        [ifelse cluster? [move-cluster] [move-random]]) ;If eat-delay? and cluster? are off, producer should move randomly (it doesn't follow others based on their foraging activity - bc why would a bird follow other successful foragers if it knows it can access food itself)
      ]
    )

    if energy > previous-energy [ ;if energy after taking your turn is higher than your previous-energy (i.e. you have foraged this turn), then add your who number to the list of current successful foragers
      set current-succ-foragers lput who current-succ-foragers
    ]

    set energy energy - 1 ;lose 1 energy per tick
  ]
end


to for-actions
  ask foragers [
    set label energy

    if first-sf-seen != nobody [ ;if there is a who number saved in first-sf-seen, increase the timer by one until
      set first-sf-seen-timer first-sf-seen-timer + 1
      if first-sf-seen-timer > memory [;if timer exceeds memory value, reset first-sf-seen to nobody (first-sf-seen is forgotten and a new one can be remembered)
        set first-sf-seen nobody
        set first-sf-seen-timer 0
        ]
    ]


    let previous-energy energy ;save energy level before moving/accessing to keep track of who has foraged at each time step

    (ifelse
      [pcolor] of patch-here = green and energy < 50 [eat] ;if the patch you are on is green and you have less than 50 energy then eat
      [pcolor] of patch-here = yellow [ ;if current patch is not green one of neighboring patches is yellow then:
        set stop-timer stop-timer + 1 ;stop-timer increases by 1 for each tick that the forager has been next to a yellow patch
                ifelse stop-timer >= 3 [ ;if stop-timer reaches 5 then run movement procedure, otherwise remain stopped
                   movement
                   set stop-timer 0]
                   [stop]
        ]
      energy <= 15 and any? patches with [pcolor != black] [move-to-patch] ;if your energy level is 15 or below, move-to-patch
      [movement] ;if patch-here is not green or yellow and the condition in the line directly above is also not met, then run movement procedure
      )

    if energy > previous-energy [ ;if energy after taking your turn is higher than your previous-energy (i.e. you have foraged this turn), then add your who number to the list of current successful foragers
      set current-succ-foragers lput who current-succ-foragers
    ]

    set energy energy - 1 ;lose 1 energy per tick
  ]
end


to turtle-actions ;setting first-sf-seen of each turtle
  ask turtles [
      if first-sf-seen = nobody [ ;if a turtle's first-sf-seen variable is empty, check whether any of current-succ-foragers was in the turtle's field of view and if so, put its who number in the calling turtle's first-sf-seen
        let turtles-in-view other turtles in-cone 30 270 ;turtle-set containing other turtles in field of view (within 30 patches distance and 270 degree viewing angle)

        if any? turtles-in-view [
          let csf-set turtles-in-view with [member? who current-succ-foragers] ;csf-set contains those turtles in view that successfully foraged in the current time step
          ;show csf-set
          (ifelse
            count csf-set > 1 [

              ifelse prior-affils? [

                let csf-set-who (sentence [who] of csf-set) ;make a list of the who numbers for all turtles in csf-set
                let affil-sf out-affil-neighbors with [member? who csf-set-who] ;let affil-sf be foragers that calling agent has outgoing affil links to and that were successful foragers in view in the current time step
                ;show affil-sf
                (ifelse any? affil-sf and count affil-sf > 1 [set first-sf-seen [who] of one-of affil-sf] ;if there is more than one affil-sf, choose one to be first-sf-seen for the calling turtle
                  any? affil-sf and count affil-sf = 1 [set first-sf-seen [who] of affil-sf]
                  [set first-sf-seen [who] of one-of csf-set]) ;if there is no affil-sf, choose one of turtles in csf-set to be first-sf-seen for the calling turtle

              ]
              [set first-sf-seen [who] of one-of csf-set] ;if there are more than 1 turtles in view that successfully foraged in the current time-step and prior-affils? switch is off, choose one to be first-sf-seen for the calling turtle

            ]
            count csf-set = 1 [set first-sf-seen [who] of csf-set] ;if there is only 1 turtle in view that successfully foraged in the current time-step, make it the first-sf-seen for the calling turtle
            [stop])
        ]

      ]
    ]
end


to access ;procedure only run by the producer
  set energy energy + 10 ;increase producer's energy by 10

  ask patch-here [
    set resource-level resource-level - 10 ;decrease patch's resource-level by 10
    if pcolor = yellow [
      set pcolor green ;set the patch's color to green (accessible to other foragers)
      ask neighbors with [pcolor = yellow] [set pcolor green]
    ]
  ]
end


to eat
  set energy energy + 10 ;increase forager's energy by 10

  ask patch-here [
    set resource-level resource-level - 10 ;decrease patch's resource-level by 10
  ]
end


to move-to-patch ;This makes the quail turn to face the closest yellow or green patch and take a step forward (toward the closest food patch)

  let food-patches patches with [resource-level > 0]  ;define food-patches as patches with resource-level greater than zero
  let closest-food min-one-of food-patches [distance myself] ;define closest-food as the food-patch with the smallest distance to the current forager

  if [pcolor] of patch-here = black and closest-food != nobody [
    set heading towards closest-food ;could also replace this line with "face closest-food"
    fd 1
  ]
end


to movement ;Follow first successful forager that you remember

  ifelse random-float 1 < attention and first-sf-seen != nobody [;if a random decimal number is less than the attention value (higher likelihood when attention is greater) AND there were successful foragers in the previous time step

    if is-list? first-sf-seen [set first-sf-seen reduce sentence first-sf-seen] ;if first-sf-seen was saved as a list (which was happening sometimes for some reason) reduce it to just a number

    ifelse unfam-prod? and first-sf-seen = 0
    [unfam-prod-movement]
    [regular-movement]
  ]

  [ ;if random-float 1 is greater than or equal to attention value or there are no successful-foragers-in-memory
    ask my-out-folls [die] ;remove all outgoing following links
    ifelse cluster? [move-cluster] [move-random] ;if cluster? switch is on, then run move-cluster procedure. Otherwise run move-random procedure
  ]


end


to unfam-prod-movement
  let unfam-prod-pref preference - 0.5 ;define preference for unfamiliar producer as preference value minus 0.5

  ifelse random-float 1 < unfam-prod-pref [ ; if a random decimal number is less than preference minus 0.5 (higher likelihood when preference is greater), then:

    ifelse [distance myself] of turtle first-sf-seen > 2.5 [ ;only change heading and move toward the sf if it is more than 2.5 units away (so the follower never ends up on the same patch as the sf)
      set heading towards turtle first-sf-seen ; face the first successful forager you remember seeing and move 1 unit forward
      forward 1
    ]
    [ifelse cluster? [move-cluster] [move-random]] ;if first-sf-seen is <= 2.5 units away, move-cluster or move-random (otherwise agents just stop moving when they are already closer than 2.5 units - like when feeding on patch - and att, pref are both 1)

    ask my-out-folls [die] ;remove any previous following links - I DON'T NEED THIS HERE IF first-sf-seen FOR EACH TURTLE DOESN'T CHANGE
    create-foll-to turtle first-sf-seen [set color red set thickness 0.3] ; create following link to first-sf-seen
  ]

  [ ;if random-float 1 is greater than or equal to unfam-prod-pref value
    ask my-out-folls [die] ;remove all outgoing following links
    ifelse cluster? [move-cluster] [move-random] ;if cluster? switch is on, then run move-cluster procedure. Otherwise run move-random procedure
  ]
end


to regular-movement
  ifelse random-float 1 < preference [ ; if a random decimal number is less than the preference value (higher likelihood when preference is greater), then:

    ifelse [distance myself] of turtle first-sf-seen > 2.5 [ ;only change heading and move toward the sf if it is more than 2.5 units away (so the follower never ends up on the same patch as the sf)
      set heading towards turtle first-sf-seen ; face the first successful forager you remember seeing and move 1 unit forward
      forward 1
    ]
    [ifelse cluster? [move-cluster] [move-random]] ;if first-sf-seen is <= 2.5 units away, move-cluster or move-random (otherwise agents just stop moving when they are already closer than 2.5 units - like when feeding on patch - and att, pref are both 1)

    ask my-out-folls [die] ;remove any previous following links - I DON'T NEED THIS HERE IF first-sf-seen FOR EACH TURTLE DOESN'T CHANGE
    create-foll-to turtle first-sf-seen [set color red set thickness 0.3] ; create following link to first-sf-seen
  ]

  [ ;if random decimal number is NOT less than preference value, do this:
    ask my-out-folls [die] ;remove all outgoing following links
    ifelse cluster? [move-cluster] [move-random] ;if cluster? switch is on, then run move-cluster procedure. Otherwise run move-random procedure
  ]
end


to move-cluster ;movement with general attraction toward conspecifics

  let closest-turtle min-one-of other turtles [distance myself]
  let dist-to-ct [distance myself] of closest-turtle

  ifelse dist-to-ct > 5 and random-float 1 < grouping [ ;move toward average coordinates of other turtles if doing so still keeps you ~4 units or more away from other turtles
    let mean-xcor mean [xcor] of other turtles
    let mean-ycor mean [ycor] of other turtles

    set heading towardsxy mean-xcor mean-ycor
    forward 1
  ]
  [move-random]

end


to move-random
  right random 360 ;rotate a random number of degrees right
  left random 360 ;rotate a random number of degrees left

  ifelse patch-ahead 1 != nobody
    [forward 1] ;if there is a patch ahead, move one step forward
    [right 180 forward 1] ;if there is no patch 1 unit ahead (at edges of display), turn 180 degrees and step forward


; Use this code if you want agents to move to center of patches instead of fixed 1 unit steps in any direction:
;    let empty-patch one-of neighbors with [not any? turtles-here] ;define empty-patch as one of neighboring patches with no turtles on it
;    move-to empty-patch ;only allow turtles to move to empty patches (defined in the previous line as being one of the neighboring patches with no turtles on it)
end


to reset-food

  ifelse alt-food? [

    if sum [resource-level] of patches with [reset-id = 1] <= 150 [ ;if total resource level of food patches gets below 300, reset them to yellow and 100 resource-level each
      ask patches with [reset-id = 1] [
;        if reset-counter >= 3 [stop] ;if patches have been reset twice already, then do not reset again

        set pcolor yellow
        set resource-level 50
        set reset-counter reset-counter + 1
      ]
    ]

    if sum [resource-level] of patches with [reset-id = 2] <= 150 [
      ask patches with [reset-id = 2] [
;        if reset-counter >= 3 [stop]

        set pcolor green ;they are already green, but I have this here to help detect issues
        set resource-level 50
        set reset-counter reset-counter + 1
      ]
    ]
  ] ;end first half of ifelse alt-food?

  [
    if sum [resource-level] of patches with [reset-id = 1] <= 300 [ ;if total resource level of food patches gets below 300, reset them to yellow and 100 resource-level each
      ask patches with [reset-id = 1] [
;        if reset-counter >= 3 [stop] ;if patches have been reset twice already, then do not reset again

        set pcolor yellow
        set resource-level 100
        set reset-counter reset-counter + 1
      ]
    ]
  ]


;  ask patches with [reset-id = 1] [
;
;    if reset-counter >= 3 [stop]
;
;   if resource-level <= 50 [ ;Reset patches if any one of them gets below 50 resource-level
;      set pcolor yellow
;      set resource-level 100
;      set reset-counter reset-counter + 1
;
;      ask neighbors with [pcolor = green] [
;        set pcolor yellow
;        set resource-level 100
;        set reset-counter reset-counter + 1
;      ]
;    ]
;  ]

;  if alt-food? [
;    ask patches with [reset-id = 0] [

;      if reset-counter >= 3 [stop]

;      if resource-level <= 50 [
;        set resource-level 100
;        set reset-counter reset-counter + 1

;        ask neighbors with [pcolor = green] [
;          set resource-level 100
;          set reset-counter reset-counter + 1
;        ]
;      ]
;    ]
;  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to iterate-history ;put temporary list of quail that were successful foragers in the current timestep into the corresponding item in the history list
;  let i ticks - 1 ;ticks minus 1 because the first item in a netlogo list is item 0
  set history replace-item ticks history current-succ-foragers
;      print item i history
end


to iterate-fss-list
  set fss-list n-values 6 ["[NA]"]

  ask turtles [
    if first-sf-seen != nobody [set fss-list replace-item who fss-list (sentence first-sf-seen)]
  ]

end


to iterate-memory
  ifelse ticks <= memory
  [   let i 0
      set sub-history sublist history i (ticks + 1) ;make a sublist of history from item i to item (ticks) i.e., the part of the list showing successful foragers from memory number of steps ago to those from previous time step (I have ticks + 1 because the sublist command excludes the last item in the range you give it)
      let dup-succ-foragers reduce sentence sub-history ;list of foragers that have successfully eaten within memory (with duplicates)
      set memory-succ-foragers remove-duplicates dup-succ-foragers ; list of foragers that have successfully eaten within memory (with duplicates removed)
  ]
    [;else do this
      let i ticks - memory ;this will be oldest item in history that the foragers can access (if memory is 20, this will allow for a sublist of 20 items representing the past events available to foragers for making their movement decision)
      set sub-history sublist history i (ticks + 1) ;make a sublist of history from item i to item (ticks) i.e., the part of the list showing successful foragers from memory number of steps ago to those from previous time step (I have ticks + 1 because the sublist command excludes the last item in the range you give it)
      ;print sub-history

      let dup-succ-foragers reduce sentence sub-history ;list of foragers that have successfully eaten within memory (with duplicates)
      ;print dup-succ-foragers
      set memory-succ-foragers remove-duplicates dup-succ-foragers ; list of foragers that have successfully eaten within memory (with duplicates removed)
      ;print memory-succ-foragers
  ]

end


to iterate-centrality
  set proxim-IDs n-values 6 ["[NA]"] ;proxim-IDs and foll-IDs list will reset to [NA]s every time step
  set foll-IDs n-values 6 ["[NA]"]

  ask turtles [
    let prox-centrality count other turtles in-radius 4
    set prox-centrality-list replace-item (who) prox-centrality-list prox-centrality ;list showing number of other foragers close to each agent

    ask my-proxims [die] ;remove all previous proximity links
    create-proxims-with other turtles in-radius 4 [set color yellow set thickness 0.3] ;create proximity links with any turtles nearby in the current time step

    if any? proxim-neighbors [ ;proxim-IDs list will update if the current turtle has other turtles in proximity, otherwise list items will remain as [NA]
      let proxim-turtles (sentence [who] of proxim-neighbors)
      set proxim-IDs replace-item who proxim-IDs proxim-turtles
    ]



    let foll-centrality count my-in-folls ;count number of incoming following links
    set foll-centrality-list replace-item (who) foll-centrality-list foll-centrality ;list showing number of followers for each turtle

    if any? in-foll-neighbors [ ;foll-IDs list will update if the current turtle has other turtles following it in the current time step, otherwise list items will remain as [NA]
      let foll-turtles (sentence [who] of in-foll-neighbors)
      set foll-IDs replace-item who foll-IDs foll-turtles
    ]

  ]
  ;print prox-centrality-list
  ;print foll-centrality-list
end


to iterate-xycor-list
  set current-xycor n-values 6 ["[NA NA]"] ;make temporary list of current coordinates

  ask turtles [
    let coors sentence xcor ycor
    set current-xycor replace-item who current-xycor coors ;ask each turtle to add its current coordinates to the temporary list in the place matching its who number
  ]
  ;print current-xycor
  ;print reduce sentence current-xycor ;PUT THIS IN A GLOBAL VARIABLE IF COORDINATES WITHOUT SQUARE BRACKET WOULD BE A BETTER OUTPUT
  ;set current-xycor reduce sentence current-xycor ;use this if you want to remove brackets separating coordinates of each turtle

  set xycor-list replace-item ticks xycor-list current-xycor ;add temporary list to the overall list of xy coordinates in the place matching the current tick number
end
@#$#@#$#@
GRAPHICS-WINDOW
357
21
638
303
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-10
10
-10
10
0
0
1
ticks
30.0

BUTTON
29
18
95
51
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
122
19
185
52
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
23
181
195
214
attention
attention
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
23
238
195
271
preference
preference
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
22
292
194
325
memory
memory
0
200
200.0
5
1
NIL
HORIZONTAL

BUTTON
203
20
284
53
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
22
339
309
530
Proximity Degree
Time
Quail in radius
0.0
10.0
0.0
6.0
true
true
"" ""
PENS
"producer" 1.0 0 -2674135 true "" "plot item 0 prox-centrality-list"
"F1" 1.0 0 -7500403 true "" "plot item 1 prox-centrality-list"
"F2" 1.0 0 -16777216 true "" "plot item 2 prox-centrality-list"
"F3" 1.0 0 -13840069 true "" "plot item 3 prox-centrality-list"
"F4" 1.0 0 -13345367 true "" "plot item 4 prox-centrality-list"
"F5" 1.0 0 -1184463 true "" "plot item 5 prox-centrality-list"

SWITCH
204
128
309
161
cluster?
cluster?
1
1
-1000

SWITCH
204
180
324
213
eat-delay?
eat-delay?
0
1
-1000

SLIDER
23
128
195
161
grouping
grouping
0
1
1.0
0.01
1
NIL
HORIZONTAL

SWITCH
24
75
155
108
prior-affils?
prior-affils?
1
1
-1000

PLOT
354
338
644
529
Following In-Degree
Time
Num followers
0.0
10.0
0.0
6.0
true
true
"" ""
PENS
"producer" 1.0 0 -2674135 true "" "plot item 0 foll-centrality-list"
"F1" 1.0 0 -7500403 true "" "plot item 1 foll-centrality-list"
"F2" 1.0 0 -16777216 true "" "plot item 2 foll-centrality-list"
"F3" 1.0 0 -13840069 true "" "plot item 3 foll-centrality-list"
"F4" 1.0 0 -13345367 true "" "plot item 4 foll-centrality-list"
"F5" 1.0 0 -1184463 true "" "plot item 5 foll-centrality-list"

SWITCH
204
238
317
271
alt-food?
alt-food?
1
1
-1000

SWITCH
163
75
302
108
unfam-prod?
unfam-prod?
1
1
-1000

SWITCH
206
295
335
328
reset-food?
reset-food?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="prelim analysis" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>affil-IDs</metric>
    <metric>prox-centrality-list</metric>
    <metric>proxim-IDs</metric>
    <metric>memory-succ-foragers</metric>
    <metric>fss-list</metric>
    <metric>foll-centrality-list</metric>
    <metric>foll-IDs</metric>
    <metric>current-xycor</metric>
    <steppedValueSet variable="memory" first="0" step="50" last="200"/>
    <steppedValueSet variable="attention" first="0" step="0.25" last="1"/>
    <steppedValueSet variable="preference" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="grouping">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-affils?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unfam-prod?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-delay?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-food?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ABSPoster100RunsPerCombo" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>affil-IDs</metric>
    <metric>prox-centrality-list</metric>
    <metric>proxim-IDs</metric>
    <metric>memory-succ-foragers</metric>
    <metric>fss-list</metric>
    <metric>foll-centrality-list</metric>
    <metric>foll-IDs</metric>
    <metric>current-xycor</metric>
    <steppedValueSet variable="memory" first="0" step="50" last="200"/>
    <steppedValueSet variable="attention" first="0" step="0.25" last="1"/>
    <steppedValueSet variable="preference" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="grouping">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-affils?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unfam-prod?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-delay?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-food?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ABSPoster50RunsPerCombo" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>affil-IDs</metric>
    <metric>prox-centrality-list</metric>
    <metric>proxim-IDs</metric>
    <metric>memory-succ-foragers</metric>
    <metric>fss-list</metric>
    <metric>foll-centrality-list</metric>
    <metric>foll-IDs</metric>
    <metric>current-xycor</metric>
    <steppedValueSet variable="memory" first="0" step="50" last="200"/>
    <steppedValueSet variable="attention" first="0" step="0.25" last="1"/>
    <steppedValueSet variable="preference" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="grouping">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-affils?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unfam-prod?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-delay?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-food?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ABSPoster50RunsPerCombo+succ-foragers" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>affil-IDs</metric>
    <metric>prox-centrality-list</metric>
    <metric>proxim-IDs</metric>
    <metric>current-succ-foragers</metric>
    <metric>fss-list</metric>
    <metric>foll-centrality-list</metric>
    <metric>foll-IDs</metric>
    <metric>current-xycor</metric>
    <steppedValueSet variable="memory" first="0" step="50" last="200"/>
    <steppedValueSet variable="attention" first="0" step="0.25" last="1"/>
    <steppedValueSet variable="preference" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="grouping">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-affils?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unfam-prod?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-delay?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-food?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

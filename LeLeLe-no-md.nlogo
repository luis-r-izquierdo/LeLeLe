;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LeLeLe
;; LeLeLe is a model designed to analyse
;; the effect of conditional dissociation
;; in the evolutionary emergence of cooperation.
;; Copyright (C) 2013 Segismundo S. Izquierdo & Luis R. Izquierdo
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es


;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;


globals [

  ;; Variables for the agent-based model

  numCC numCD numDD       ;; these variables store how many times each of the outcomes has been observed in one match
  %-CC %-CD %-DD
  %-players-in-CC %-players-in-CD %-players-in-DD
  num-outcomes

  pool-of-singles ;; players without mate at the beginning of the period
  n-of-singles-at-the-beginning
  n-of-singles-selected-to-be-paired
  paired-players

  strategy-names
  strategy-frequencies
  strategy-frequencies-pool-of-singles
  strategy-frequencies-pool-of-singles-norm

  ;; reporters since tick = track-regimes-from-tick

  cum-num-cooperative-regime    ;; only since tick = track-regimes-from-tick!
  cum-num-defective-regime      ;; only since tick = track-regimes-from-tick!
  last-regime                   ;; only since tick = track-regimes-from-tick!

  num-transitions-cc-to-dd      ;; only since tick = track-regimes-from-tick!
  num-transitions-dd-to-cc      ;; only since tick = track-regimes-from-tick!

  ;; Variables for the mean dynamics

  CC-couples CD-couples DD-couples
  variables-corresponding-to-each-strategy
  %-players-in-CC-md
  %-players-in-CD-md
  %-players-in-DD-md
  %-singles-md

  strategy-numbers
  strategy-payoffs-md
  strategy-in-pool-of-singles-md
  strategy-frequencies-md

  total-payoff-md
  total-in-pool-of-singles-md

]

breed [players player]

players-own [
  action        ;; the action is either 0 (C) or 1 (D)
  break-if-C    ;; 0 if the player does not split up after the other player plays C. 1 otherwise.
  break-if-D    ;; 0 if the player does not split up after the other player plays D. 1 otherwise.

  mate
  new-partnership?

  payoff
  strategy-number
  ;; strategies are numbered from 0 (0 0 0) to 7 (1 1 1)
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; the following procedure is called when the model is first loaded
to startup
  clear-all
  setup-variables
  reset-ticks
  setup-initial-conditions
  no-display
end

to setup-variables
  set strategy-names ["CSS" "CSL" "CLS" "CLL" "DSS" "DSL" "DLS" "DLL"]
  set strategy-numbers n-values 8 [i -> i]

  set cum-num-cooperative-regime 0
  set cum-num-defective-regime   0
  set last-regime                0

  set num-transitions-cc-to-dd   0
  set num-transitions-dd-to-cc   0

end



to setup-initial-conditions
  create-players num-players [
    set mate nobody
    set payoff 0
    set hidden? true
  ]
  set pool-of-singles (players with [mate = nobody])
  set n-of-singles-at-the-beginning (count pool-of-singles)

  if-else initial-strategy = "random"
     [ ;; random distribution of initial strategies for the whole population
       ask players [
         set strategy-number random 8
         update-strategy-variables
       ]
     ]
     [
       let st-number (position initial-strategy strategy-names)
       ask players [
         set strategy-number st-number
         update-strategy-variables
       ]
     ]

  ;; here we plot the initial conditions, before any matching occurs
  gather-data
  update-graphs

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  make-couples
  play

  gather-data

  tick
  if (ticks >= track-regimes-from-tick) [update-regimes]
  update-graphs

  split-up
  ;; new generation
  kill-and-breed
  if (count players != num-players) [adjust-num-players]

end


to make-couples
  ask players [set new-partnership? false]
  set pool-of-singles (players with [mate = nobody])
  set n-of-singles-at-the-beginning (count pool-of-singles)
  let players-to-be-paired (pool-of-singles with [random-float 1.0 < prob-rematch])
  set n-of-singles-selected-to-be-paired (count players-to-be-paired)

  if (n-of-singles-selected-to-be-paired mod 2 = 1) [
    set n-of-singles-selected-to-be-paired (n-of-singles-selected-to-be-paired - 1)
    set players-to-be-paired (n-of n-of-singles-selected-to-be-paired players-to-be-paired)
  ]

  ask players-to-be-paired [set new-partnership? true]
  ask players-to-be-paired [
    if (mate = nobody) [
      set mate one-of (players-to-be-paired with [mate = nobody and self != myself])
      ask mate [set mate myself]
    ]
  ]
end

to play
  ask players [
    set payoff ifelse-value (mate = nobody)
      [out-payoff]
      [payoff-for action ([action] of mate)]
  ]
end

to-report payoff-for [my-action her-action]
  ;; my-action is 0 if C, 1 if D
  report
    (1 - my-action) * (1 - her-action) * CC-payoff +
    (1 - my-action) * her-action * CD-payoff +
    my-action * (1 - her-action) * DC-payoff +
    my-action * her-action * DD-payoff
end

to split-up
  ask players[
    if (mate != nobody) [
      if (([action] of mate = 0) and (break-if-C = 1))
        or (([action] of mate = 1) and (break-if-D = 1))
        ;; if programmed to split up after your mate's selected action
        [ask mate [set mate nobody]    set mate nobody ]
      ]
    ]
end

to kill-and-breed

  let list-fitness n-values 8 [? -> sum [payoff] of players with [strategy-number = ?]]

  if (sum list-fitness = 0) [set list-fitness n-values 8 [1]]
    ;; Applies when all players have zero fitness
  let cum-fitness [0]
    ;; cum-fitness last value is 0 and is 9 items long
  foreach list-fitness [? -> set cum-fitness fput (? + first cum-fitness) cum-fitness]

  ask players [
    if (random-float 1.0 < (1.0 / expected-life))
      [
       if (mate != nobody)  [ ask mate [set mate nobody] ]
       set mate nobody
       if-else (random-float 1.0 < prob-mutation)
        [set strategy-number random 8]
        [set strategy-number 7
         let tmp random-float first cum-fitness
           ;; select the new strategy with probability proportional to fitness
          foreach butfirst cum-fitness [ ? -> if ( tmp < ?) [set strategy-number (strategy-number - 1)] ]
        ]
       update-strategy-variables
      ]
    ]
end

to adjust-num-players
  let adjustment (num-players - (count players))
  if adjustment != 0 [
  ifelse adjustment > 0
    [
      create-players adjustment [
        set mate nobody
        set strategy-number ifelse-value (initial-strategy = "random")
          [random 8]
          [position initial-strategy strategy-names]
        update-strategy-variables
      ]
    ]
    [
      ask n-of (0 - adjustment) players [
        if (mate != nobody)  [ ask mate [set mate nobody] ]
        die
      ]
    ]
  ]
end

to update-strategy-variables
  ;; update strategy variables from strategy-number
  let remain strategy-number
  set break-if-D (remain mod 2)  set remain int (remain / 2)
  set break-if-C (remain mod 2)  set remain int (remain / 2)
  set action     (remain mod 2)
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Statistics    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to gather-data

  set paired-players (players with [mate != nobody])
  set numCC (count paired-players with [action = 0 and [action] of mate = 0]) / 2
  set numDD (count paired-players with [action = 1 and [action] of mate = 1]) / 2
  set numCD (count paired-players with [action = 0 and [action] of mate = 1])
  set num-outcomes (numCC + numDD + numCD)
  set %-CC ifelse-value (num-outcomes = 0) [0] [(numCC / num-outcomes)]
  set %-CD ifelse-value (num-outcomes = 0) [0] [(numCD / num-outcomes)]
  set %-DD ifelse-value (num-outcomes = 0) [0] [(numDD / num-outcomes)]
  let n-players (count players)
  set %-players-in-CC 2 * numCC / n-players
  set %-players-in-CD 2 * numCD / n-players
  set %-players-in-DD 2 * numDD / n-players

  set strategy-frequencies n-values 8 [st-n -> count players with [strategy-number = st-n]]
  set strategy-frequencies-pool-of-singles n-values 8 [st-n -> count pool-of-singles with [strategy-number = st-n]]
  let num-singles-in-pool n-of-singles-at-the-beginning ;; this is just to avoid a potential division by 0
  if num-singles-in-pool = 0 [set num-singles-in-pool 1]
  set strategy-frequencies-pool-of-singles-norm (map [? -> ? / num-singles-in-pool] strategy-frequencies-pool-of-singles)

end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Reporters     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Regime-related reporters ;;

to-report cooperative-regime
  report (%-cc-lower-limit <= %-CC and %-CC <= %-cc-upper-limit)
end

to-report defective-regime
  report (%-dd-lower-limit <= %-DD and %-DD <= %-dd-upper-limit)
end

to update-regimes

  if cooperative-regime [
    set cum-num-cooperative-regime (cum-num-cooperative-regime + 1)

    if last-regime = 2 [
      set num-transitions-dd-to-cc (num-transitions-dd-to-cc + 1)
    ]
    set last-regime 1
  ]

  if defective-regime [
    set cum-num-defective-regime (cum-num-defective-regime + 1)

    if last-regime = 1 [
      set num-transitions-cc-to-dd (num-transitions-cc-to-dd + 1)
    ]
    set last-regime 2
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Plots       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to update-graphs
  ;; all graphs refer to the situation before the new generation comes in.
  let current-num-players (count players)

  set-current-plot "Players"
    set-current-plot-pen "no play"  plotxy ticks current-num-players
    set-current-plot-pen "in DD"    plotxy ticks (2 * num-outcomes)
    set-current-plot-pen "in CD/DC" plotxy ticks (2 * (num-outcomes - numDD))
    set-current-plot-pen "in CC"    plotxy ticks (2 * numCC)
    set-plot-y-range 0 current-num-players

  set-current-plot "Players' pairs"
    set-current-plot-pen "singles before matching"
    plot n-of-singles-at-the-beginning
    set-current-plot-pen "singles after matching"
    plot (n-of-singles-at-the-beginning - n-of-singles-selected-to-be-paired)
    set-current-plot-pen "in pairs after matching"
    plot count paired-players
    set-plot-y-range 0 current-num-players

  set-current-plot "Strategy Distribution"
    let total (sum strategy-frequencies)
    let bar 1
    foreach (n-values 8 [? -> ?]) [ ? ->
      set-current-plot-pen item ? strategy-names
      plotxy ticks bar
      set bar (bar - ((item ? strategy-frequencies) / total))
    ]
    set-plot-y-range 0 1

end
@#$#@#$#@
GRAPHICS-WINDOW
402
303
575
477
-1
-1
55.0
1
2
1
1
1
0
0
0
1
-1
1
-1
1
1
1
1
ticks
30.0

SLIDER
11
120
141
153
num-players
num-players
1
5000
400.0
1
1
NIL
HORIZONTAL

SLIDER
11
10
131
43
CC-payoff
CC-payoff
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
138
11
255
44
CD-payoff
CD-payoff
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
46
131
79
DC-payoff
DC-payoff
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
138
47
255
80
DD-payoff
DD-payoff
0
10
1.0
1
1
NIL
HORIZONTAL

BUTTON
11
273
88
306
Setup
startup
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
176
274
257
307
Go
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

PLOT
278
10
658
237
Players
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"no play" 1.0 1 -10899396 true "" ""
"in DD" 1.0 1 -2674135 true "" ""
"in CD/DC" 1.0 1 -4539718 true "" ""
"in CC" 1.0 1 -13345367 true "" ""

MONITOR
278
239
400
284
% players in CC
%-players-in-CC
3
1
11

MONITOR
537
239
658
284
% players in DD
%-players-in-DD
3
1
11

MONITOR
402
239
534
284
% players in CD
%-players-in-CD
3
1
11

PLOT
278
502
657
653
Players' pairs
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"singles before matching" 1.0 0 -16777216 true "" ""
"in pairs after matching" 1.0 0 -4699768 true "" ""
"singles after matching" 1.0 0 -10899396 true "" ""

SLIDER
11
158
140
191
expected-life
expected-life
1
100
25.0
1
1
NIL
HORIZONTAL

BUTTON
91
273
172
306
Go once
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

SLIDER
11
195
139
228
prob-mutation
prob-mutation
0
1
0.05
0.001
1
NIL
HORIZONTAL

CHOOSER
154
136
262
181
initial-strategy
initial-strategy
"random" "CSS" "CSL" "CLS" "CLL" "DSS" "DSL" "DLS" "DLL"
0

MONITOR
11
496
111
549
cc-regime
cum-num-cooperative-regime
17
1
13

MONITOR
164
497
268
550
dd-regime
cum-num-defective-regime
17
1
13

MONITOR
11
552
132
605
cc to dd ->
num-transitions-cc-to-dd
17
1
13

MONITOR
177
217
258
270
NIL
ticks
17
1
13

SLIDER
11
425
137
458
%-cc-lower-limit
%-cc-lower-limit
0
1
0.62
0.01
1
NIL
HORIZONTAL

SLIDER
11
460
137
493
%-cc-upper-limit
%-cc-upper-limit
0
1
0.82
0.01
1
NIL
HORIZONTAL

SLIDER
142
426
268
459
%-dd-lower-limit
%-dd-lower-limit
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
142
461
268
494
%-dd-upper-limit
%-dd-upper-limit
0
1
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
146
552
268
605
<- dd to cc
num-transitions-dd-to-cc
17
1
13

PLOT
278
285
657
500
Strategy Distribution
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"CSS" 1.0 1 -4528153 true "" ""
"CSL" 1.0 1 -11033397 true "" ""
"CLS" 1.0 1 -14070903 true "" ""
"CLL" 1.0 1 -15390905 true "" ""
"DSS" 1.0 1 -612749 true "" ""
"DSL" 1.0 1 -955883 true "" ""
"DLS" 1.0 1 -2674135 true "" ""
"DLL" 1.0 1 -8053223 true "" ""

TEXTBOX
23
345
89
402
Definition of regimes
13
0.0
1

TEXTBOX
45
404
107
422
cc regime
13
0.0
1

TEXTBOX
180
407
248
425
dd regime
13
0.0
1

INPUTBOX
109
343
269
403
track-regimes-from-tick
1000.0
1
0
Number

SLIDER
12
231
138
264
prob-rematch
prob-rematch
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
11
83
255
116
out-payoff
out-payoff
0
10
0.0
1
1
NIL
HORIZONTAL

MONITOR
11
607
64
652
% CC
%-CC
3
1
11

MONITOR
207
608
268
653
% DD
%-DD
3
1
11

MONITOR
87
608
188
653
% CD
%-CD
3
1
11

@#$#@#$#@
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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


module Dynamics where

import Base
import ProbBase

data MatrixSt m =
  MatrixSt { getParticles :: (Particles m) {- + music state etc. -}}
  deriving (Show, Eq)


{- simulate t milliseconds in matrix.
will be called from the game system -}
simulate :: Double -> MatrixSt m -> MatrixSt m
simulate t mtx = undefined


{- potential balancing function between visualization and stabilization?
  0 < r < 1.

This looks good :)  -}
bla r = weightProb (\x -> r*exp (-abs(x-0)) + (1-r)*exp ( -abs(x-9)) ) p0
p0 = createParticles  [9,9,1,1,1,1,1,1,3,3] [0..9::Double]

renderMatrix :: MatrixSt m -> a
renderMatrix = undefined

{-
Zusatz an den Forderungen an die Modellierung:
* Nicht alles muss iwann spiky werden. Es darf auch
in best. richtungen gleichverteilt sein, wenn es das
schon vorher war.

* Es gibt einen Unschärfemechanismus, welcher
automatisch auftritt, sobald der Schärfe/Spikymechanismus
schwächer wird. Beide können als Dual voneinander gesehen werden.
Die Schärfe ist üblicherweise auf einen hohen Schärfewert by default,
und kann per Spielerinput verringert werden. Es geht aber
automatisch danach wieder hoch.

* p(v) beeinflusst die kombinierte wkt. p(s*w)
in einer solchen Art und Weise, dass die wkt.
dort, wo p(v) hoch ist abhängig von p(s*w) erhöht wird.
(also z.B. proportional, polynomisch, etc.)
Dies hat den Effekt, dass es eine grössere Unschärfe
oder stärkere Visualisierung braucht,
um eine längere Distanz zu überqueren (oder durch Wände zu gehen)

* Im Allgemeinen führt gleichzeitiges visualisieren und schärfen
dazu, dass die schwache Visualisierung bleibt und der starke Default immernoch da ist
=> cool. Multiworlds!
=> evtl. konkret navigierbar und betretbar machen? => manipulation of conditional probabilities

* Im Allgemeinen führt gleichzeitiges Visualisiern und Unschärfe
machen zu einer schwächer werdenen Wahrscheinlichkeit

für die originale Position-(en) und eine weniger
stark schwächer werdende Wkt. für die Visualisierung.

=> Teleportation besteht also aus dem visualisieren(kontinulierlich),
dann unschärfe aktivieren, ... verfolgen, und zum rechten
zeitpunkt unschärfe deaktivieren.

=> entspricht zu einem gewissen Grad dem "aus der Welt treten"
und dann "in die Welt eintreten"

=> cool :)

=> Im Vergleich zu "Zeit ist nicht-linear" aber,
  ist die Kausalität noch nicht modelliert (oder?)
  Man kann gerade in jeden beliebigen Zustand rein.
  Also auch in Zustände die keine konsistente Vergangenheit haben....
  Oder? Was ist mit causal loops?
  Andererseits... wenn man erst einmal mit einer
  konsistenten Welt beginnt, dann ist der cheat-freie
  Spielverlauf ja bereits eine sinnvolle Vergangenheit 
  jedes besuchten Zustands.
  
  
=> Aktuell wird jedoch der Beobachter teleportiert.
Der Spieler selbst nicht wirklich. Deswegen gibt es trotz
möglichen Zeitreisen immer genau eine Version des Spielers
und aller Weltobjekte. Es gibt keine Interaktion mit dem selbst.
Es ist kein Zeitreisen in diesme Sinne. Nur der Beobachter wechselt seine
AUfmerksamkeit.

==> Was tun? Wenn ich versuche den Teil der Kausalität
als auch den Effekt der Zeitreisen + Parallelwelten zu intigrieren,
dann habe ich das Modell noch anzupassen.

Diese "von der physikalischen Welt unabhängige Welt"
darzustellen fehlt auf jedenfall.
Die Welt ist möglicherweise mehr als nur eine Wahrscheinlihckeitsvtl.
über den möglichen zuständen.

Eine Alternative Formulierung würde nicht belibige
Teleportationen erlauben. Dafür braucht es explizit
Attraktoren und "teleporter". Diese Attraktoren
und Teleporter funktionieren selbst unabhängig von der konkreten Raumzeitausprägung.
Zeit ist dann auch nur eine Form der Distanz wie es das auch
der Raum ist.
Ausserdem würden dann Dinge wieder Parallelwelten,
Interaktionen mit selbst und Kausalität (und Loops)
sehr sehr relevant werden.

==> Du brauchst das noch nicht sofort ausformuliert
oder intuitiv verstanden zu haben. Es reicht soweit gekommen zu sein,
und sich darüber bewusst geworden zu sein,
dass es diese Diskrepanz zwischen der aktuellen Implementation
und dieser alternativen (oder vllt. originalen?) Formulierung gibt.

Im sinne des Spiels:
  * Beide könnten interessant werden.
  * Ersteres wäre ein interessantes Spielkonzept
      welches eine begrenzte Komplexität hätte. (Als Spieler und als Entwickler)
      
  * Zweiteres wäre ein stückchen komplexer,
    wäre dann aber "realistischer" und interessanter auch.


-}

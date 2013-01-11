;;;; schulze-Beispiel 2 from http://de.wikipedia.org/wiki/Schulze-Methode
(load "schulze.scm")

(choices '(A B C D))

(vote '(A B C D))
(vote '(A B C D))
(vote '(A B C D))

(vote '(D A B C))
(vote '(D A B C))

(vote '(D B C A))
(vote '(D B C A))

(vote '(C B D A))
(vote '(C B D A))

(result)

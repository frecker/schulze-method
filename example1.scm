;;;; schulze example 1 from http://de.wikipedia.org/wiki/Schulze-Methode
(load "schulze.scm")

(choices '(A B C D E))

(vote '(A C B E D))
(vote '(A C B E D))
(vote '(A C B E D))
(vote '(A C B E D))
(vote '(A C B E D))

(vote '(A D E C B))
(vote '(A D E C B))
(vote '(A D E C B))
(vote '(A D E C B))
(vote '(A D E C B))

(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))
(vote '(B E D A C))

(vote '(C A B E D))
(vote '(C A B E D))
(vote '(C A B E D))

(vote '(C A E B D))
(vote '(C A E B D))
(vote '(C A E B D))
(vote '(C A E B D))
(vote '(C A E B D))
(vote '(C A E B D))
(vote '(C A E B D))

(vote '(C B A D E))
(vote '(C B A D E))

(vote '(D C E B A))
(vote '(D C E B A))
(vote '(D C E B A))
(vote '(D C E B A))
(vote '(D C E B A))
(vote '(D C E B A))
(vote '(D C E B A))

(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))
(vote '(E B A D C))

(result)

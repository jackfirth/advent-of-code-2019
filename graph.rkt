#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [graph? predicate/c]
  [empty-graph empty-graph?]
  [empty-graph? predicate/c]
  [nonempty-graph? predicate/c]
  [graph-vertices (-> graph? set?)]
  [graph-edges (-> graph? multidict?)]
  [graph-insert-vertex (-> graph? any/c graph?)]
  [graph-insert-vertices (-> graph? (sequence/c any/c) graph?)]
  [graph-insert-edge
   (->* (graph? any/c any/c) (#:undirected? boolean?) graph?)]
  [graph-insert-edges
   (->* (graph? (sequence/c entry?)) (#:undirected? boolean?) graph?)]
  [graph-remove-vertex (-> graph? any/c graph?)]
  [graph-remove-vertices (-> graph? (sequence/c any/c) graph?)]
  [graph-remove-edge (->* (graph? any/c any/c) (#:undirected? boolean?) graph?)]
  [into-undirected-graph reducer?]
  [into-directed-graph reducer?]
  [in-graph-topological-sort (-> graph? (sequence/c any/c))]
  [graph-terminal-vertices (-> graph? set?)]
  [graph-initial-vertices (-> graph? set?)]
  [graph-outgoing-edges (-> graph? any/c set?)]
  [graph-incoming-edges (-> graph? any/c set?)]
  [graph-inverse (-> graph? graph?)]))

(require fancy-app
         point-free
         racket/list
         racket/sequence
         racket/set
         racket/stream
         rebellion/collection/entry
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/type/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-record-type graph (edges disconnected-vertices))

(define empty-graph
  (graph #:edges empty-multidict #:disconnected-vertices (set)))

(define (graph-vertices g)
  (define edges (graph-edges g))
  (set-union (multidict-unique-keys edges)
             (multidict-unique-values edges)
             (graph-disconnected-vertices g)))

(define (graph-insert-vertex g v)
  (define edges (graph-edges g))
  (define disconnected (graph-disconnected-vertices g))
  (cond
    [(or (set-member? disconnected v)
         (multidict-contains-key? edges v)
         (multidict-contains-value? edges v))
     g]
    [else
     (graph #:edges edges #:disconnected-vertices (set-add disconnected v))]))

(define (graph-insert-vertices g vs)
  (for/fold ([g g]) ([v vs]) (graph-insert-vertex g v)))

(define (graph-insert-edge g source destination #:undirected? [undirected? #f])
  (cond
    [undirected?
     (define g* (graph-insert-edge g destination source))
     (graph-insert-edge g* source destination)]
    [else
     (define edges (multidict-add (graph-edges g) source destination))
     (define disconnected
       (set-remove (set-remove (graph-disconnected-vertices g) source)
                   destination))
     (graph #:edges edges #:disconnected-vertices disconnected)]))

(define (graph-insert-edges g edges #:undirected? [undirected? #f])
  (for/fold ([g g]) ([e edges])
    (define source (entry-key e))
    (define destination (entry-value e))
    (graph-insert-edge g source destination #:undirected? undirected?)))

(define (graph-insert-edge-entry g e undirected?)
  (graph-insert-edge g (entry-key e) (entry-value e) #:undirected? undirected?))

(define into-undirected-graph
  (make-fold-reducer (λ (g e) (graph-insert-edge-entry g e #t)) empty-graph))

(define into-directed-graph
  (make-fold-reducer (λ (g e) (graph-insert-edge-entry g e #f)) empty-graph))

(module+ test
  (test-case "empty-graph"
    (check-equal? (graph-edges empty-graph) empty-multidict)
    (check-equal? (graph-vertices empty-graph) (set)))

  (test-case "graph-insert-edge"
    (test-case "directed"
      (test-case "not-already-present"
        (define g (graph-insert-edge empty-graph 'a 'b))
        (check-equal? (graph-edges g) (multidict 'a 'b))
        (check-equal? (graph-vertices g) (set 'a 'b)))

      (test-case "vertices-already-present"
        (define original (graph-insert-vertices empty-graph (list 'a 'b)))
        (define g (graph-insert-edge original 'a 'b))
        (check-equal? (graph-edges g) (multidict 'a 'b))
        (check-equal? (graph-vertices g) (set 'a 'b)))

      (test-case "source-vertex-already-present"
        (define original (graph-insert-vertex empty-graph 'a))
        (define g (graph-insert-edge original 'a 'b))
        (check-equal? (graph-edges g) (multidict 'a 'b))
        (check-equal? (graph-vertices g) (set 'a 'b)))

      (test-case "destination-vertex-already-present"
        (define original (graph-insert-vertex empty-graph 'b))
        (define g (graph-insert-edge original 'a 'b))
        (check-equal? (graph-edges g) (multidict 'a 'b))
        (check-equal? (graph-vertices g) (set 'a 'b)))))
  
  (test-case "graph-insert-vertex"
    
    (test-case "empty"
      (define g (graph-insert-vertex empty-graph 'a))
      (check-equal? (graph-edges g) empty-multidict)
      (check-equal? (graph-vertices g) (set 'a)))

    (test-case "not-already-present"
      (define original (graph-insert-vertex empty-graph 'a))
      (define g (graph-insert-vertex original 'b))
      (check-equal? (graph-edges g) empty-multidict)
      (check-equal? (graph-vertices g) (set 'a 'b)))

    (test-case "already-present"
      (define original (graph-insert-vertex empty-graph 'a))
      (define g (graph-insert-vertex original 'a))
      (check-equal? (graph-edges g) empty-multidict)
      (check-equal? (graph-vertices g) (set 'a)))))

(define (graph-remove-vertex g v)
  (define old-edges (graph-edges g))
  (define edges
    (~> (multidict-replace-values old-edges v (set))
        multidict-inverse
        (multidict-replace-values _ v (set))
        multidict-inverse))
  (define outgoing (multidict-ref old-edges v))
  (define incoming (multidict-ref (multidict-inverse old-edges) v))
  (define disconnected (set-remove (graph-disconnected-vertices g) v))
  (define g* (graph #:edges edges #:disconnected-vertices disconnected))
  (graph-insert-vertices g* (set-remove (set-union outgoing incoming) v)))

(define (graph-remove-edge g source destination #:undirected? [undirected? #f])
  (define edges
    (~> (graph-edges g)
        (multidict-remove _ source destination)
        (if undirected?
            (multidict-remove _ destination source)
            values)))
  (define g*
    (graph #:edges edges
           #:disconnected-vertices (graph-disconnected-vertices g)))
  (graph-insert-vertices g* (list source destination)))

(define (graph-remove-vertices g vs)
  (for/fold ([g g]) ([v vs]) (graph-remove-vertex g v)))

(define (graph-terminal-vertices g)
  (define disconnected (graph-disconnected-vertices g))
  (define edges (graph-edges g))
  (set-union disconnected
             (set-subtract (multidict-unique-values edges)
                           (multidict-unique-keys edges))))

(define (graph-initial-vertices g)
  (define disconnected (graph-disconnected-vertices g))
  (define edges (graph-edges g))
  (set-union disconnected
             (set-subtract (multidict-unique-keys edges)
                           (multidict-unique-values edges))))

(module+ test
  (test-case "graph-terminal-vertices"
    (define g
      (graph #:edges (multidict 'a 'b 'a 'c 'b 'c 'd 'd)
             #:disconnected-vertices (set 'e 'f)))
    (define expected (set 'c 'e 'f))
    (check-equal? (graph-terminal-vertices g) expected))

  (test-case "graph-initial-vertices"
    (define g
      (graph #:edges (multidict 'a 'b 'a 'c 'b 'c 'd 'd)
             #:disconnected-vertices (set 'e 'f)))
    (define expected (set 'a 'e 'f))
    (check-equal? (graph-initial-vertices g) expected)))

(define (empty-graph? v)
  (and (graph? v)
       (empty-multidict? (graph-edges v))
       (set-empty? (graph-disconnected-vertices v))))

(define (nonempty-graph? v)
  (and (graph? v)
       (nonempty-multidict? (graph-edges v))
       (not (set-empty? (graph-disconnected-vertices v)))))

(define (in-graph-topological-sort g)
  (cond
    [(empty-graph? g) empty-stream]
    [else
     (define initials (graph-initial-vertices g))
     (define noninitials
       (stream* (in-graph-topological-sort (graph-remove-vertices g initials))))
     (stream-append initials noninitials)]))

(module+ test
  (test-case "in-graph-topological-sort"
    (define g
      (graph #:edges (multidict 5 11
                                7 11
                                7 8
                                3 8
                                3 10
                                11 2
                                11 9
                                11 10
                                8 9)
             #:disconnected-vertices (set)))
    (check-equal? (reduce-all into-set (in-graph-topological-sort g))
                  (set 5 11 7 8 3 10 2 9))
    (define topo-sorted (sequence->list (in-graph-topological-sort g)))
    (check-true (< (index-of topo-sorted 5) (index-of topo-sorted 11)))
    (check-true (< (index-of topo-sorted 7) (index-of topo-sorted 11)))
    (check-true (< (index-of topo-sorted 7) (index-of topo-sorted 8)))
    (check-true (< (index-of topo-sorted 3) (index-of topo-sorted 8)))
    (check-true (< (index-of topo-sorted 3) (index-of topo-sorted 10)))
    (check-true (< (index-of topo-sorted 11) (index-of topo-sorted 2)))
    (check-true (< (index-of topo-sorted 11) (index-of topo-sorted 9)))
    (check-true (< (index-of topo-sorted 11) (index-of topo-sorted 10)))
    (check-true (< (index-of topo-sorted 8) (index-of topo-sorted 9)))))

(define (graph-outgoing-edges g v)
  (multidict-ref (graph-edges g) v))

(define (graph-incoming-edges g v)
  (multidict-ref (multidict-inverse (graph-edges g)) v))

(define (graph-inverse g)
  (graph #:edges (multidict-inverse (graph-edges g))
         #:disconnected-vertices (graph-disconnected-vertices g)))

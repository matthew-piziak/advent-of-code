# Advent of Code

## Framework

Advent of Code problems tend to take a particular form and so I solve them in a particular way. Here I explain my choices.

### Why Clojure?

- The JVM is fast.
- The JVM has many libraries.
- A dynamic language is good for AoC, where safety offered by types isn't worth the loss of development speed.
- Eval-at-point makes for a interactive 2D REPL experience.
- The debugger is excellent.
- `core` is full of fast and well-designed datatypes and functions.
- Clojure's language features that make it easy to work with maps.
- LISP macros provide unmatched abstraction capability, though this is not particularly useful here.
- Good support for terse anonymous code, which makes exploratory coding ergonomic.
- High stability means Clojure tooling remains familiar from year to year.
- The `recur` keyword for stack checking (explained below).

### Process

Most AoC problems involve processing a linear file of data in some way, so the spine of my solution is a recursive
function with `recur` for tail-recursion checking.

From 2021 Day 1:
```clojure
(defn nav
  ([depth forward steps]
   (if (empty? steps) (* depth forward)
       (let [step (first steps)]
         (case (step :dir)
           "forward" (recur depth (+ forward (step :n)) (rest steps))
           "down" (recur (+ depth (step :n)) forward (rest steps))
           "up" (recur (- depth (step :n)) forward (rest steps)))))))
```

The first parameter is the input being consumed (sometimes after some postprocessing), and the rest of the parameters
are accumulators.

In general the function will look like this:
```clojure
(defn solve [input aux1 aux2 aux3 ...]
  (if base-case? solution
    (recur (rest input) ...)))
```

The accumulators are where you put your fancy data structures. So if you need a map or a min-heap or a queue or whatever
to be fast, that's where they go. Now because of `recur` you don't need to worry about the stack anymore, and the
strategy is reduced to finding the correct procedure and choosing the correct data structures.

If you need to do a few linear preprocessing passes over your data to make it neat, that's more likely to be successful
than an accidentally exponential-time hack.

The rest is just finesse. Break out new functions wherever the abstraction is helpful. Know `core`. Collect some utility
functions for reading data. Check out `core.matrix` and `instaparse`. Have fun.

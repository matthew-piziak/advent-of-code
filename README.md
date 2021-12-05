# Advent of Code

## Framework

Advent of Code problems tend to take a particular form and so I solve them in a particular way.

Here's how I do it:

### Why Clojure?

- The JVM is fast.
- The JVM has many libraries.
- A dynamic language is good for AoC, where safety offered by types isn't worth the loss of development speed.
- Eval-at-point makes for a interactive 2D REPL experience.
- The debugger is excellent.
- `core` is full of fast and well-designed datatypes and functions.
- Clojure has language features that make it easy to work with maps.
- Good support for terse anonymous code, which makes exploratory coding ergonomic.
- High stability means Clojure tooling remains familiar from year to year.
- The `recur` keyword for stack checking (explained below).
- LISP macros provide unmatched abstraction capability, though this is not particularly useful here.

### `recur`

Most AoC problems involve processing a linear file of data in some way, so the spine of my solution is a recursive
function with `recur`. In Clojure, `(defn foo [] ... (recur bar))` is like `(def foo [] ... (foo bar))`, with the
important feature that Clojure will check that it is tail-recursive. This prevents you from accidentally creating a
large stack, which in turn helps you stay performant and ensures you won't overflow when you move from your example
input to your real input.

### Example (2021 Day 1)

```clojure
(defn nav
  ([steps depth forward]
   (if (empty? steps) (* depth forward)
       (let [step (first steps)]
         (case (step :dir)
           "forward" (recur (rest steps) depth (+ forward (step :n)))
           "down" (recur (rest steps) (+ depth (step :n)) forward)
           "up" (recur (rest steps) (- depth (step :n)) forward))))))
```

The first parameter is the input being consumed (sometimes after some postprocessing), and the rest of the parameters
are accumulators.

### Generally

The function will look like this:
```clojure
(defn solve [input aux1 aux2 aux3 ...]
  (if base-case? solution
    (recur (rest input) ...)))
```

The accumulators are where you put your fancy data structures. So if you need a map or a min-heap or a queue or whatever
to be fast, that's where they go.

Because of `recur` you don't need to worry about the stack anymore, and the strategy is reduced to finding the correct
procedure and choosing the correct data structures.

### That's basically it

The rest is just finesse. Break out new functions wherever the abstraction is helpful. Know `core`. Collect some utility
functions for reading data. Check out `core.matrix` and `instaparse`. Have fun!

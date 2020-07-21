* An implementation of Mergeable Replicated Data Types in Clojure

http://gowthamk.github.io/docs/mrdt.pdf

Prerequisites:
- clojure
- clj
- openjdk

* Running tests
From the cli:

```
./bin/kaocha
```

use the `--test-help` to see a list of possible arguments

From the repl:

```
(use 'kaocha.repl)
;=> nil
(run)
0 tests, 0 assertions, 0 failures.
;=> #:kaocha.result{:count 0, :pass 0, :error 0, :fail 0, :pending
```
* Building
To build an uberjar, run this:
```
clojure -A:build
```
Then run it:
```
java -cp target/paragon.jar clojure.main -m paragon.main
```

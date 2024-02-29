# Welcome to LambLang aka an implementation of simply typed 🐑-duh calculus.

For now, it is a course project, but who knows where the math will take us.

## How to run?

* Clone this repo :)
* Install cabal if you haven't already
* In the repository, run `cabal run LambLang`
* Enter your program in the repl, and hit enter to run!

## What programs can you write in LambLang?

Currently, the most exciting feature is to read and print strings. Here are some example programs:
* `printStr "Hello, world!"`
* `printStr "What is your name? " >>= \x -> readStr >>= \name -> printStr "Hello, " >>= \x -> printStr name >>= \x -> printStr " :)\n" >>= \x -> printStr "We hope you enjoy LambLang!"`
* `readStr >>= \x -> printStr x`

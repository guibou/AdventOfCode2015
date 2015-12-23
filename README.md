My solutions to adventofcode.com

in Haskell.

Everything is in the src directory, stored as randomly as possible. Usually you have a *dayXX* and *dayXX'* functions which represents the two solutions for a day, and *dayXXInput* which represents the transformed input. Sometime it is *IO* if the input is stored in a file, sometime not. Sometime the input is hardcoded in the solution... (Yeah, ugly ;)

Be sure that I'm more organized in real life, but the point was to code really quickly.

I always finished in a "virtual" leaderboard except for exercise 20, for which I was stuck finding a smart solution where the brute force solution was enough. By "virtual" leaderboard, I mean that it always took me less time that the last person in the leaderboard to finish the exercise, but because of timezone / work / life / kids / big bang, I was not available for coding when the exercises were unconcealed. (Yes, I cannot prove it, but I don't care ;)

Many of theses exercises should be coded differently, mostly :

- You should implement tests for all exercise, I did not, because I was testing in ghci until the answer was ready to submit. Writing proper test may had slowed me.
- Most exercises are solved using brute force, and that's not always satisfying.
- Never, ever, use *nub* in real code, that's *n* square complexity...

I'm not satisfied by many of the exercises, but mainly:

- exercise 19: I was lucky with my input, and I stopped once I found a solution, which was the right one. Apparently the input was crafted specially to avoid multiples solutions, hence the first solution was the right one, but I still don't understand why.
- exercise 20: Brute force, you killed me. I had a solution which runs in ten minutes, but instead of letting it run, I was looking for something smarter. Finally ended with something ugly using vector, that's fast, but not satisfying.
- exercise 21/22: My first solution was crappy as hell because I did not know anything about lenses and monad transformers. Finally got something acceptable, however theses exercises are not difficult, but needs a good code architecture, which I did not provide.
- exercise 24: I was lucky, my brute force solution works, but is not guaranteed to work on any input.

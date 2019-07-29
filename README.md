# Capture IO

<details><summary>Capture the stdout and stderr of an IO action.</summary>

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Readme where

import System.IO.Capture

test = [example|
```

</details>

This library is intended for writing tests and documentation. It allows
you to capture what would be printed to stdout and stderr of an IO action.
Useful for recreating what might be seen in a GHCi session.

Some example usages:

```haskell
>> a <- captureOutput (print True)
>> print (a == "True\n")
True
```

It also comes with a quasiquoter which allows you to write something that
looks like an interactive session. This README is actually a literate
Haskell file using this quasiquoter.

```haskell
>> print 5
5
```

```haskell
>> print "Hello"
"Hello"
```

``haskell
>> putStrLn "Hello, world!"
Hello, world!
```

``haskell
>> print 1
>> print 2
>> print 3
1
2
3
```

<details>
```haskell
|]
```
</details>

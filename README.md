How to shoot yourself in the foot with lenses and state
=======================================================

_Run the [code](https://github.com/andreasabel/shoot-yourself-in-the-foot-with-lenses)
with `ghci -pgmL markdown-unlit README.lhs` using
[Simon Hengel's markdown-unlit tool](https://github.com/sol/markdown-unlit)._


```haskell
{-# LANGUAGE BlockArguments #-}

import Control.Monad         ( when )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.State   ( MonadState, get, put, modify, StateT, execStateT )

import Data.Functor          ( (<&>) )
import Data.Functor.Const    ( Const(Const), getConst )
import Data.Functor.Identity ( Identity(Identity), runIdentity )
```

Background
----------

Van Laarhoven lenses implement optics through a `Functor`-polymorphic definition:
```haskell
type Lens' o i = forall f. Functor f => (i -> f i) -> o -> f o
```
A value of `Lens' o i` gives us a means to focus on a part `i` (_inner_) of a whole `o` (_outer_).

For instance we would like to read and modify the fields of the following record.
```haskell
data St = St { _value :: Int, _changed :: Bool }
```

Van Laarhoven lenses for record fields are implemented following a fixed scheme:
```haskell
value :: Lens' St Int
value f st = f (_value st) <&> \ v -> st{ _value = v }

changed :: Lens' St Bool
changed f st = f (_changed st) <&> \ b -> st{ _changed = b }
```
We take the value of the field, pipe it through the updater `f` and set the field to its result.
There aren't any correct alternatives to this scheme, thanks to the parametric nature of `Functor f`.

The type of van Laarhoven lenses can be instantiated to functors `Const` and `Identity` to
implement getters and modifiers for the inner part:
```haskell
get_ :: Lens' o i -> o -> i
get_ l = getConst . l Const

over :: Lens' o i -> (i -> i) -> o -> o
over l f = runIdentity . l (Identity . f)
```

A setter is as usual an instance of the modifier with a constant modification function.
```haskell
set :: Lens' o i -> i -> o -> o
set l = over l . const
```

With refreshed background, let's proceed to the main part of this article.

Monadic updaters
----------------

Say we wish to update a part of our program state while also throwing some effects.
It is easy to implement some function `modifyM` to that extend:
```haskell
modifyM :: MonadState o m => Lens' o i -> (i -> m i) -> m ()
modifyM l f = do
  o  <- get
  o' <- l f o
  put o'
```
This can even be written as simple pipeline `get >>= l f >>= put`.
Seems like van Laarhoven lenses were made for effectful updates!

Let try that out!
We write a small monadic program that sets the `value` in `St` and records whether it changed
the old value.
```haskell
type M = StateT St IO
```

Function `setChanged` sets the `changed` field to `True`.
```haskell
setChanged :: M ()
setChanged = modifyM changed $ const $ pure True
```

Function `putValue v` set the `value` field to `v`.
If the new value differs from the old one, it prints a message and sets `changed`.
```haskell
putValue :: Int -> StateT St IO ()
putValue new = modifyM value \ old -> do
  when (new /= old) do
    liftIO $ putStrLn $ unwords [ "Changing value to", show new ]
    setChanged
  pure new
```

Let's test this with initial value `0` and new value `42`.
```haskell
main :: IO ()
main = do
  let initial = St{ _value = 0, _changed = False }
  final <- flip execStateT initial $ putValue 42
  putStrLn $ unwords $ concat
    [ [ "Value has" ]
    , [ "not"       | not $ get_ changed final ]
    , [ "changed"   ]
    ]
```

Great stuff, let's run it!
It prints:
```
Changing value to 42
Value has not changed
```
That is unexpected!
The program contradicts itself!

Sanity-checking whether we forgot a negation somewhere... no, the `not` is there and correct.


Analysis
--------

Since we are dealing with a short program the culprit should not be hard to find.

Our first suspicion goes to `modifyM`.
It gets the state, modifies it through `l f` and writes back the result...
Seems exactly what we want it to do.

So the lenses must be blamed!  Let's revisit the definition of `value`:
```
value f st = f (_value st) <&> \ v -> st{ _value = v }
```
It reads the field `_value` from state `st`, pipes it through the potentially effectful `f`,
and updates `st` with the result.

But in our case `f` also updates `st` (the field `changed`), and those updates are not
included in the result returned by the lens.  Thus, updates through a side effect get lost!

Is there anything we can fix in our implementation of the lenses or `modifyM` to salvage the situation?

1. An effect-safe version of the `value` lens would look like this:
   ```haskell
   valueSafe :: MonadState St m => (Int -> m Int) -> St -> m St
   valueSafe f st = do
     let old = _value st
     new <- f old
     st' <- get
     put st'{ _value = new }
     return st'
   ```
   Rather than updating `st`, we retrieve `st'`, the state possibly updated by the effects of `f`,
   and store the new value there.

   However, `valueSafe` isn't a `Lens'` anymore!
   It cannot be specialized to `get_`, `set` and `over`.

2. Can we do a safe version of `modifyM`?
   ```haskell
   modifyMSafe :: MonadState o m => Lens' o i -> (i -> m i) -> m ()
   modifyMSafe l f = do
     i  <- get_ l <$> get
     i' <- f i
     modify $ set l i'
   ```
   (Or as one-liner: `get <&> get_ l >>= f >>= modify . set l`.)
   This works because we use the lens `l` in an effect-free way, just through its getter and setter instance.
   Yet our original idea for `modifyM` cannot be salvaged.

Conclusion
----------

The definition of van Laarhofen lenses `Functor f => (i -> f i) -> o -> f o` seems to _ask_ for effectful updates,
as it is the most _direct_ way to use them.
However, following its Siren call can have devastating consequences such as state loss that is hard to discover.

In the wild I was bitten by this trap in our 160kloc Agda codebase, where it took me a full work day to isolate the cause of the weird state loss I was observing:
<https://github.com/agda/agda/pull/7470#discussion_r1747232483>

_Write a comment by opening an issue in the [issue tracker](https://github.com/andreasabel/shoot-yourself-in-the-foot-with-lenses/issues)!_

Source: <https://github.com/andreasabel/shoot-yourself-in-the-foot-with-lenses>

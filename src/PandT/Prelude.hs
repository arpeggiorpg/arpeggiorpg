module PandT.Prelude
( module ClassyPrelude
, module PandT.Prelude
) where

import ClassyPrelude
import Control.Lens as PandT.Prelude (
    (^.), (^?), at, makePrisms, makeLenses, Lens, Lens', Prism, Prism', view, over, set, lens,
    mapped, _Just, _1, _2, _Left, _Right
    )
import Control.Monad.Trans.Maybe (MaybeT(..))



-- TBD in ClassyPrelude
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

-- The "why do I have to define these myself" functions

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

runForeverM :: Monad m => (a -> m a) -> a -> m ()
runForeverM go start = go start >>= runForeverM go

traceShowMessage :: Show a => String -> a -> a
traceShowMessage message obj = trace (message ++ (show obj) ++ ": ") obj

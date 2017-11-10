module FunctionEvaluator where

import           Control.Monad.State
import qualified Data.Map.Strict     as M

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a = evalState (evalS f a) M.empty

evalS :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> State (M.Map a b) b
evalS f a = do
    m <- get
    case a `M.lookup` m of
        Just b -> return b
        Nothing -> case f a of
            Left b -> put (M.insert a b m) >> return b
            Right (as, induct) -> do
                bs <- traverse (evalS f) as
                let res = induct bs
                modify (M.insert a res)
                return res

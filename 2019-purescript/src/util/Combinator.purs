module Advent.Util.Combinator where 

always :: forall a b. a -> b -> a
always x _ = x 

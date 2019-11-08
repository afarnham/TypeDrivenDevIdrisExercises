deMorgan1 : Either (a -> Void) (b -> Void) -> ((a, b) -> Void)
deMorgan1 x y =
  case x of
    Left l => l (fst y)
    Right r => r (snd y)

deMorgan2 : (Either a b -> Void) -> (a -> Void, b -> Void)
deMorgan2 f =
  (\a => f (Left a),
   \b => f (Right b))

deMorgan3 : (a -> Void, b -> Void) -> (Either a b -> Void)
deMorgan3 tuple =
  \c => case c of
    Left a => (fst tuple) a
    Right b => (snd tuple) b

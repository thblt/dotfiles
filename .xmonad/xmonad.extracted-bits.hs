-- Bits of XMonad configuration I may need again

-- A MultiToggle transformer for decorations
data DecoTransformer = DECO deriving (Read, Show, Eq, Typeable)
instance Transformer DecoTransformer Window where
  transform _ x k = k (myDecoration x) (const x)

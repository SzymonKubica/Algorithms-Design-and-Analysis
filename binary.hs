type Binary = [Bit]

data Bit = O | I
  deriving (Show)

inc :: Binary -> Binary
inc [] = [I]
inc (O : x) = I : x
inc (I : x) = O : inc x

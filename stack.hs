module Stack where
  
  --- Fundamental constructs.
  
  -- The definition of a stack data type.
  -- Stack is the constructor function.
  data Stack = Stack [Float]
  
  -- Defining how to show the contents of a stack.
  -- `unwords` joins a list of strings together by spaces.
  -- I'd like to get this to be separated by commas.
  instance Show Stack where
    show (Stack contents) = "{" ++ (unwords (map show contents)) ++ "}"
  
  -- Creates a new stack initialized to zero.
  newStack :: Stack
  newStack = Stack [1.0, 1.0, 1.0, 1.0]
  
  -- Pushes an item onto the stack.
  push :: Float -> Stack -> Stack
  push item (Stack contents) = Stack (take 4 (item : contents))
  
  -- Grabs the top value from the stack.
  peek :: Stack -> Float
  peek (Stack (first:rest)) = first
  
  -- Removes the top value from a stack and returns both the value and the stack.
  -- This is clumsy to do with referential transparency.
  pop :: Stack -> (Float, Stack)
  pop (Stack (first:rest)) = (first, Stack rest)
  
  --- Stack manipulation functions that depend only the top value.
  --- All of these functions, except mapFirst, have type Stack -> Stack
  
  -- A generalization of operations that change the first value.
  mapFirst :: (Float -> Float) -> Stack -> Stack
  mapFirst operation (Stack (first:rest)) = Stack ((operation first) : rest)
  
  -- Changes the sign on the top value of the stack.
  chs s = mapFirst negate s
  
  -- Takes the absolute value of the top value of the stack.
  sAbs s = mapFirst abs s
  
  -- Sets the top value to 0.
  clear s = mapFirst (\x -> 0) s
  
  -- Takes the root of the top value.
  sSqrt s = mapFirst sqrt s
  
  -- Trigonometric functions
  sCos s = mapFirst cos s
  sSin s = mapFirst sin s
  sTan s = mapFirst tan s
  
  -- Exponential function
  sExp s = mapFirst exp s
  
  -- Logarithm
  ln s = mapFirst log s
  
  --- Stack manipulation functions that depend on the top two values.
  --- All of these functions, except mapTwo, have type Stack -> Stack
  
  mapTwo :: (Float -> Float -> Float) -> Stack -> Stack
  mapTwo operation (Stack (first:second:rest)) = Stack (((operation first second) : rest) ++ [last rest])
  
  -- Mathematical operators.
  add s = mapTwo (+) s
  sub s = mapTwo (-) s
  mul s = mapTwo (*) s
  sDiv s = mapTwo (/) s
  ytox s = mapTwo (**) s
  
  --- Miscellaneous functions.
  
  -- Rotates the stack.
  rotate :: Stack -> Stack
  rotate (Stack (first:rest)) = Stack (rest ++ [first])

  -- Switches the top two values of the stack.
  x :: Stack -> Stack
  x (Stack (first:second:rest)) = Stack (second : (first : rest))
  
  -- Identity function.
  pass :: Stack -> Stack
  pass s = s
  
  -- Grabs the contents of a stack.
  stackContents :: Stack -> [Float]
  stackContents (Stack contents) = contents